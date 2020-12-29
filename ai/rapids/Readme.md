To use RAPIDS in your application, you have several options.<BR>
<BR>
  
# To start the container and notebook server, run the following commands. 
Note that we are mounting the entire tutorial here, but you can mount only your needed codes by altering the -v argument provided.<BR>
  
  ```bash

$ docker pull rapidsai/rapidsai:cuda10.1-runtime-ubuntu18.04-py3.7
$ sudo docker run --gpus all --rm -it -p 8888:8888 -p 8787:8787 -p 8786:8786 \ 
-v /home/hpclabs/amarathe/MLBootcamp/ai/rapids/English/Python/jupyter_notebook:/rapids/notebooks/host rapidsai/rapidsai:cuda10.1-runtime-ubuntu18.04-py3.7<BR>


```
  
You can open http://localhost:8000 and view the codes there. You can find this tutorial in the host directory. You can also explore different RAPIDs sample notebooks provided in the base directory.

<BR>
  
# To build the DockerContainer from scratch using the Dockerfile provided in this tutorial, run the following commands.<BR>




```bash

# Run from the Root Directory
$ docker build . 

# Run with GPUs and Network Access
$ docker run --gpus all -it --rm -p 8888:8888 ~~~Container ID~~~~

# Run the Jupyter Notebook
$ jupyter notebook --allow-root --ip 0.0.0.0

```
<BR>

# To make your own Dockerfile, and using it to run the tutorial you can follow:<BR>
  
Using the following short Dockerfile users can leverage the existing RAPIDS images and build a custom secure image:<BR>

```bash
FROM rapidsai/rapidsai-nightly:cuda10.2-runtime-ubuntu18.04-py3.7
RUN sed -i "s/NotebookApp.token=''/NotebookApp.token='secure-token-here'/g" /opt/docker/bin/entrypoint_source
```

Once built, the resulting image will be secured with the new token.<BR>

This example can be repurposed by replacing the sed command with other commands for custom libraries or settings.<BR>
  
  
  
  <BR>


# For more information about RAPIDS applications and Docker, please refer <a href="https://hub.docker.com/r/rapidsai/rapidsai/"> here</a>
