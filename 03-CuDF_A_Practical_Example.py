#!/usr/bin/env python
# coding: utf-8

# # Intro to cuDF
# 
# Welcome to third cuDF tutorial notebook! This is a practical example that utilizes cuDF and cuPy, geared primarily for new users. The purpose of this tutorial is to introduce new users to a data science processing pipeline using RAPIDS on real life datasets.

# We will be working on the famous data science problem: The Titanic Challenge using RAPIDS. This notebook will focus on preprocessing data using CuDF and CuPy.

# In[ ]:


import os
import numpy as np
import math
np.random.seed(12)
import cudf
import cupy as cp


# First we need to load the dataset from the csv into CuDF dataframes, for the preprocessing steps.

# In[1]:


df = cudf.read_csv('../data/train.csv')
print(df)


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# Thus we have successfully used CuDF and CuPy to process the Titanic dataset, and converted the data to a form more suitable to apply machine learning algorithms. In the extra labs for future labs in CuML we will be using this processed dataset.

# If you need to refer to the dataset, you can download it here.

# In[ ]:




