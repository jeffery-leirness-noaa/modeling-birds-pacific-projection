# install specific R version
FROM rocker/geospatial:4.4.3

# install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# copy renv.lock to the container
WORKDIR /project
COPY renv.lock renv.lock

# set renv library path
ENV RENV_PATHS_LIBRARY renv/library

# restore packages from renv.lock
RUN R -e "renv::restore()"

# add label for github repository
LABEL org.opencontainers.image.source=https://github.com/jeffery-leirness-noaa/modeling-birds-pacific-projection
