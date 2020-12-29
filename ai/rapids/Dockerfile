FROM rapidsai/rapidsai-nightly:cuda10.2-runtime-ubuntu18.04-py3.7
RUN sed -i "s/NotebookApp.token=''/NotebookApp.token='secure-token-here'/g" /opt/docker/bin/entrypoint_source
