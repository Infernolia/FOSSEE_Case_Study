#!/usr/bin/env python
# coding: utf-8

# # User Defined Functions with cuDF

# Sometimes, the built-in methods of cudf.DataFrame don't do exactly what we want. We need to write a custom function (also known as a user defined function) to apply over the DataFrame.
# 
# cuDF’s DataFrame class has two primary methods that let users run custom Python functions on GPUs: `apply_rows` and `apply_chunks`. In this tutorial, we’ll walk through how to use `apply_rows` and `apply_chunks` to create your own UDFs and show how you can implement a GPU-accelerated windowing function. At the end, we'll also walk through a more advanced example of applying a user defined function on a grouped DataFrame (using `apply_grouped`).
# 

# ## `apply_rows`

# `apply_rows` processes each of the DataFrame rows independently in parallel. Under the hood, the `apply_rows` method will optimally divide the long columns into chunks, and assign chunks into different GPU blocks for parallel computation. 
# 
# In order to use `apply_rows`, we need to write a kernel function. A kernel function is a function that will be executed on each row of the DataFrame set the output value for each row. **The execution order of rows is arbitrary, so each execution of the function MUST be independent of other execution.**
# 
# How does this work? User defined functions with cuDF rely on CUDA under the hood. Exploring CUDA and GPU architecture in-depth is out of scope for this tutorial. But, at a very high level, in cuDF's user defined functions:
# 
# 
# - Compute is spread across multiple "blocks", which have access to both global memory but also their own in-block memory 
# - Within each block, many "threads" operate independently and can quickly access data in their block-specific shared memory 
# 
# 
# As a result, the loop in the example function below resembles serial code, but executes in parallel in multiple threads on the GPU. When `kernel` is invoked, the function arguments corresponding to the input/output are strided so as to improve GPU parallelism. The kernel function is compiled to the GPU using `numba.cuda`, so the kernel function must only use Python features/functions that are [supported](https://numba.pydata.org/numba-doc/dev/cuda/cudapysupported.html) by Numba for CUDA.

# A kernel function takes the form
# 
# ```python
# def kernel(in1, in2, in3, ..., out1, out2, ..., kwarg1, kwarg2, ...):
#     for i, (x, y, z, ...) in enumerate(zip(in1, in2, in3, ...)):
#         out1[i] = ...
#         out2[i] = ...
# ```
# 
# `in1, in2, in3, ...` are the input columns. `out1, out2, ...` are the output columns. The kernel function should not return a result. Instead, output columns are passed as arguments and the result is written to them. Each thread writes a result to a specific index in the output column, which is why write `out1[i] = ...`.
# 
# Additional keyword arguments can be passed (`kwarg1, kwarg2, ...`). Inside the kernel function, [standard numba.cuda attributes](https://numba.pydata.org/numba-doc/dev/cuda/kernels.html#thread-positioning) like `numba.cuda.threadIdx` can be used to access things like the thread or block indices. We'll explain a little bit more about this below.
# 
# To execute this function on our DataFrame, we use `apply_rows`. `apply_rows` is called like:
# 
# ```python
# df = df.apply_rows(kernel
#                    incols=['in1', 'in2', 'in3', ...],
#                    outcols={'out1': np.float64, 'out2': np.int8, ...},
#                    kwargs={'kwarg1': val1, 'kwarg2': val2, ...})
# ```
# 
# `incols` is a list of the arguments for our `kernel` function representing the columns in the DataFrame. As a result, `in1`, `in2`, etc. must match the names of columns in the DataFrame that we intend to use.
# 
# `outcols` is a dictionary mapping the output column names to their dtype. If we intend to generate two output columns, we need to `outcols` needs to contain two keys (`out1` and `out2`).
# 
# `kwargs` is a dictionary mapping the keyword argument parameters to their values. If our `kernel` function needs additional arguments contained in our DataFrame, we can pass them in here. `kwargs` can be an empty dictionary if there are no keyword arguments.
# 
# After calling `apply_rows` as above, `df` would have extra columns `out1`, `out2`, ... with the output results. 

# ## Example: Haversine distance

# In the example below, we create a DataFrame representing pairs of latitude and longitude points. We use `apply_rows` to calculate the [Haversine distance](https://en.wikipedia.org/wiki/Haversine_formula) between two points in the input arrays.
# 
# $$
# d = 2r \arcsin\left(\sqrt{\sin^2\left(\frac{\varphi_2 - \varphi_1}{2}\right) + \cos(\varphi_1) \cos(\varphi_2)\sin^2\left(\frac{\lambda_2 - \lambda_1}{2}\right)}\right)
# $$
# 
# where $\varphi_1,\varphi_2$ are the latitudes and $\lambda_1,\lambda_2$ are the longitudes.

# In[ ]:


from math import cos, sin, asin, sqrt, pi, atan2

import time
import cudf
import numpy as np
from numba import cuda


# In[ ]:


np.random.seed(12)
data_length = 1000

df = cudf.DataFrame()
df['lat1'] = np.random.normal(10, 1, data_length)
df['lon1'] = np.random.normal(10, 1, data_length)
df['lat2'] = np.random.normal(10, 1, data_length)
df['lon2'] = np.random.normal(10, 1, data_length)


# In[ ]:


def haversine_distance_kernel(lat1, lon1, lat2, lon2, out):
    """Haversine distance formula taken from Michael Dunn's StackOverflow post:
    https://stackoverflow.com/questions/4913349/haversine-formula-in-python-bearing-and-distance-between-two-gps-points
    """
    for i, (x_1, y_1, x_2, y_2) in enumerate(zip(lat1, lon1, lat2, lon2)):
        print('thread id:', cuda.threadIdx.x, 'block id:', cuda.blockIdx.x,
              'array size:', lat1.size, 'block threads:', cuda.blockDim.x)

        x_1 = pi/180 * x_1
        y_1 = pi/180 * y_1
        x_2 = pi/180 * x_2
        y_2 = pi/180 * y_2
        
        dlon = y_2 - y_1
        dlat = x_2 - x_1
        a = sin(dlat/2)**2 + cos(x_1) * cos(x_2) * sin(dlon/2)**2
        
        c = 2 * asin(sqrt(a)) 
        r = 6371 # Radius of earth in kilometers
        
        out[i] = c * r


# In[ ]:


df = df.apply_rows(haversine_distance_kernel,
                   incols=['lat1', 'lon1', 'lat2', 'lon2'],
                   outcols=dict(out=np.float64),
                   kwargs=dict())


# In[ ]:


print(df.head())


# Notice that we had a `print` statement in our kernel but didn't see any printed output. Print statements in kernels will only appear in terminal output; Jupyter Notebooks won't display them. We included this for this tutorial, and have copied some sample output below:
# 
# ```
# ...
# thread id: 0 block id: 4 array size: 1 block threads: 64
# thread id: 1 block id: 4 array size: 1 block threads: 64
# thread id: 2 block id: 4 array size: 1 block threads: 64
# ...
# thread id: 61 block id: 4 array size: 1 block threads: 64
# thread id: 62 block id: 4 array size: 1 block threads: 64
# thread id: 63 block id: 4 array size: 1 block threads: 64
# ...
# thread id: 29 block id: 0 array size: 2 block threads: 64
# thread id: 30 block id: 0 array size: 2 block threads: 64
# ...
# ```

# In the example above, the printed output from applying our `haversine_distance_kernel` function shows some informative information. If you were to look at the entire printed output, you'd notice a few things:
# 
# - The processing was spread across 15 CUDA blocks
# - Within each block, 64 separate threads were used for computation.
# - In this case, most threads in a block handled one element from the input array, but some threads have to deal with two elements, because there are 1000 rows and 960 threads (15 blocks * 64 threads per block)
# 
# `apply_rows` handled all of this for us!

# ## **Exercise 1**
# 
# Modify the above example to pass in the radius of the earth `r` as a keyword argument to the kernel.
# 
# <details><summary><b>Solution</b></summary>
#    <pre>
# def haversine_distance_kernel(lat1, lon1, lat2, lon2, out, r):
#     """Haversine distance formula taken from Michael Dunn's StackOverflow post:
#     https://stackoverflow.com/questions/4913349/haversine-formula-in-python-bearing-and-distance-between-two-gps-points
#     """
#     for i, (x_1, y_1, x_2, y_2) in enumerate(zip(lat1, lon1, lat2, lon2)):
#         print('thread_id:', cuda.threadIdx.x, 'bid:', cuda.blockIdx.x,
#               'array size:', lat1.size, 'block threads:', cuda.blockDim.x)
# 
#         x_1 = pi/180 * x_1
#         y_1 = pi/180 * y_1
#         x_2 = pi/180 * x_2
#         y_2 = pi/180 * y_2
#         
#         dlon = y_2 - y_1
#         dlat = x_2 - x_1
#         a = sin(dlat/2)**2 + cos(x_1) * cos(x_2) * sin(dlon/2)**2
#         
#         c = 2 * asin(sqrt(a)) 
#         
#         out[i] = c * r
#    </pre>
#     <pre>
# df = df.apply_rows(haversine_distance_kernel,
#                    incols=['lat1', 'lon1', 'lat2', 'lon2'],
#                    outcols=dict(out=np.float64),
#                    kwargs=dict(r=6371))
# print(df.head()
# </pre>
# </details>

# In[ ]:





# ## **Exercise 2**
# 
# Write a kernel to compute the [bearing formula](https://www.movable-type.co.uk/scripts/latlong.html):
# 
# $$\operatorname{atan2}(\sin(\lambda_2-\lambda_1)\cos(\varphi_2), \cos(\varphi_1)\sin(\varphi_2)-\sin(\varphi_1)\cos(\varphi_2)\cos(\lambda_2-\lambda_1))$$
#        
# where again $\varphi_1,\varphi_2$ are the latitudes and $\lambda_1,\lambda_2$ are the longitudes.
# 
# <details><summary><b>Solution</b></summary>
#    <pre>
# from math import atan2
# 
# def bearing_kernel(lat1, lon1, lat2, lon2, out):
#     for i, (x_1, y_1, x_2, y_2) in enumerate(zip(lat1, lon1, lat2, lon2)):
#         print('thread_id:', cuda.threadIdx.x, 'bid:', cuda.blockIdx.x,
#               'array size:', lat1.size, 'block threads:', cuda.blockDim.x)
# 
#             x_1 = pi/180 * x_1
#             y_1 = pi/180 * y_1
#             x_2 = pi/180 * x_2
#             y_2 = pi/180 * y_2
# 
#             dlon = y_2 - y_1
#             a = atan2(sin(dlon)*cos(x_2), cos(x_1)*sin(x_1) - sin(x_1)*cos(x_2)*cos(dlon))
#             # Convert radians [-π, π] to degrees [0°, 360°]
#             out[i] = (180/pi*a + 180) % 360
# 
# df = df.apply_rows(bearing_kernel,
#                    incols=['lat1', 'lon1', 'lat2', 'lon2'],
#                    outcols=dict(out=np.float64),
#                    kwargs=dict())
# 
# print(df.head())
# 
# </pre>
# </details>

# In[ ]:





# ## `apply_chunks`

# In the section above, the data was generally split into single-element chunks. `apply_chunks` is a more general version of `apply_rows` that gives us control over how the data is chunked on the GPU. We can specify how to divide the long array, map each of the array chunks to different GPU blocks to process (using the `chunks` argument) and assign the number of threads per block (using the `tpb` argument).
# 
# Applying kernels with `apply_chunks` is very similar to applying kernels with `apply_rows`. Except, when we call `apply_chunks`, we must also provide:
# - The chunk size `chunks` as an integer or `cudf.Series` of integer offsets
# - The number of threads per block, `tpb`. Note that `tpb` can be omitted, but in that case, it defaults to `1` thread per block, which is very inefficient. We recommend always setting this argument.
# 
# The kernel is executed by each thread, with full access to all the elements in that chunk of the array. In this example below, with `chunks=16`, cuDF tries to uniformly cut the 1000 elements into chunks of size 16 spread across multiple blocks. Eight threads per block process the subarray of size 16, since we set `tpb=8`.

# In[ ]:


# This is the exact same kernel as above. 
def haversine_distance_kernel(lat1, lon1, lat2, lon2, out):
    """Haversine distance formula taken from Michael Dunn's StackOverflow post:
    https://stackoverflow.com/questions/4913349/haversine-formula-in-python-bearing-and-distance-between-two-gps-points
    """
    for i, (x_1, y_1, x_2, y_2) in enumerate(zip(lat1, lon1, lat2, lon2)):
        print('thread_id:', cuda.threadIdx.x, 'bid:', cuda.blockIdx.x,
              'array size:', lat1.size, 'block threads:', cuda.blockDim.x)

        x_1 = pi/180 * x_1
        y_1 = pi/180 * y_1
        x_2 = pi/180 * x_2
        y_2 = pi/180 * y_2
        
        dlon = y_2 - y_1
        dlat = x_2 - x_1
        a = sin(dlat/2)**2 + cos(x_1) * cos(x_2) * sin(dlon/2)**2
        
        c = 2 * asin(sqrt(a)) 
        r = 6371 # Radius of earth in kilometers
        
        out[i] = c * r


# In[ ]:


outdf = df.apply_chunks(haversine_distance_kernel,
                        incols=['lat1', 'lon1', 'lat2', 'lon2'],
                        outcols=dict(out=np.float64),
                        kwargs=dict(),
                        chunks=16,
                        tpb=8)


# In[ ]:


print(outdf.head())


# ## **Exercise 3**
# 
# Use your bearing formula function from Exercise 2 with `apply_chunks` instead of `apply_rows` (if you did not complete the exercises, click **solution** below each exercise to get the solution).
# 
# 
# <details><summary><b>Solution</b></summary>
#    <pre>
# 
# from math import atan2
# 
# def bearing_kernel(lat1, lon1, lat2, lon2, out):
#     for i, (x_1, y_1, x_2, y_2) in enumerate(zip(lat1, lon1, lat2, lon2)):
# 
#         x_1 = pi/180 * x_1
#         y_1 = pi/180 * y_1
#         x_2 = pi/180 * x_2
#         y_2 = pi/180 * y_2
# 
#         dlon = y_2 - y_1
#         a = atan2(sin(dlon)*cos(x_2), cos(x_1)*sin(x_1) - sin(x_1)*cos(x_2)*cos(dlon))
#         # Convert radians [-π, π] to degrees [0°, 360°]
#         out[i] = (180/pi*a + 180) % 360
# 
# <br>
# 
# df = df.apply_chunks(bearing_kernel,
#                      incols=['lat1', 'lon1', 'lat2', 'lon2'],
#                      outcols=dict(out=np.float64),
#                      kwargs=dict(),
#                      chunks=16,
#                      tpb=8)
# 
# print(df.head())
# </pre>
# </details>

# In[ ]:





# # Advanced UDFs: `apply_grouped`

# In this section, we'll walk through how we can apply UDFs to a grouped DataFrame, and why you might want to do this in the first place.
# 
# In the financial services industry, data scientists often need to compute features from time series data. One of the most popular ways to process time series data is to compute a moving average, as if you were sliding a window across your array. With the skills we've learned so far, you could create a custom UDF to do exactly that! You could define the function, pass it into `apply_rows` or `apply_chunks`, and get the moving average results.
# 
# But, often, our DataFrame will contain *multiple* time series that we want to process logically separately (such the time series of prices for different stocks). In the introductory notebook, we learned about the `groupby` concept in cuDF that helps us define separate groups and process them separately.
# 
# In the following example, we’ll show how to combine these two concepts (groupbys and UDFs) to calculate moving averages within each separate group.

# First, we'll create a random 15 row DataFrame with one categorical feature and one random integer valued feature.

# In[ ]:


df = cudf.DataFrame(
        {
            "stock": [1] * 5 + [2] * 5 + [3] * 5,
            "price": [np.random.randint(0, 100) for _ in range(15)],
        }
     )
print(df.head())


# Next, we'll group the DataFrame by its categorical feature.

# In[ ]:


# grouped_df = df.groupby("stock", method="cudf")
grouped_df = df.groupby("stock")


# Now, we'll define a kernel which takes the moving average of a sliding window. We'll call this function `rolling_avg`.

# In[ ]:


def rolling_avg(price, avg):
    win_size = 3
    for i in range(cuda.threadIdx.x, len(price), cuda.blockDim.x):
        if i < win_size - 1:
            # If there is not enough data to fill the window,
            # take the average to be NaN
            avg[i] = np.nan
        else:
            total = 0
            for j in range(i - win_size + 1, i + 1):
                total += price[j]
            avg[i] = total / win_size


# With our `rolling_avg` function defined, we can pass it to the `apply_grouped` method for grouped DataFrames and compute the moving average within each group.
# 
# Currently, the function argument that corresponds to the column you're taking the average of (`price`) must match the name of your column. As a result, we name that argument `price`.

# In[ ]:


start = time.time()
# Compute moving avgs on all groups
results = grouped_df.apply_grouped(rolling_avg,
                               incols=['price'],
                               outcols=dict(avg=np.float64))

end = time.time()
print('cuDF time', end-start)

print("Results:")
print(results)


# Notice that the value for `avg` at beginning of each group is null. This makes sense, since we can't have an average value for a sliding window of size `3` until we have at least three values. We used the line `avg[i] = np.nan` in our `rolling_avg` function to geneate this result in the output.

# ## Conclusion
# 
# At this point, we've introduced UDFs and the ways you can apply them to DataFrames. Feel free to experiment writing your own UDFs in the cells below.
# 
# For a walk-through of UDFs in more detail, we encourage you to explore our [Overview of User Defined Functions with cuDF](https://docs.rapids.ai/api/cudf/nightly/guide-to-udfs.html).

# In[ ]:




