
FROM rapidsai/rapidsai:cuda10.1-runtime-ubuntu18.04-py3.7 


RUN apt-get install -y cmake build-essential python


RUN apt-get install --no-install-recommends -y python3 python3-pip && \
 pip3 install --upgrade pip && \
 apt-get install --no-install-recommends -y python3-setuptools && \
 pip3 install jupyter
 
 
 RUN pip3 install jupyterlab

## Uncomment this line to run Jupyter notebook by default
CMD jupyter notebook --ip 0.0.0.0 --port 8888 --allow-root



