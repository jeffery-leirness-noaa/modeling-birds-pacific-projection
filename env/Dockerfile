# Install specific R version
FROM rocker/geospatial:4.3.1

# Install python
RUN apt-get update -qq && \
 apt-get install -y python3-pip tcl tk libz-dev libpng-dev
RUN ln -f /usr/bin/python3 /usr/bin/python
RUN ln -f /usr/bin/pip3 /usr/bin/pip
RUN pip install -U pip

# Install azureml-mlflow
RUN pip install azureml-mlflow

# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Copy the renv.lock lockfile to the container
WORKDIR /project
COPY renv.lock renv.lock

# Set renv library path
ENV RENV_PATHS_LIBRARY renv/library

# Restore packages from renv lockfile
RUN R -e "renv::restore()"
