FROM rocker/r-ver:4.2.2

## Install packages
RUN apt-get update \ 
  && apt-get install -y --no-install-recommends \ 
    libgdal-dev \ 
    libproj-dev \ 
    libgeos-dev \ 
    lbzip2 \ 
    libnetcdf-dev \ 
    libsqlite3-dev \ 
    libssl-dev \ 
    libudunits2-dev \ 
    netcdf-bin \ 
    sqlite3 \ 
    imagemagick \ 
    pandoc \ 
    pandoc-citeproc \ 
    make \ 
    ghostscript \ 
    poppler-utils \ 
    zip \ 
    wget \ 
    fonts-dejavu-extra \ 
    curl \ 
    tk \ 
    openjdk-11-jre \ 
    gdebi-core \
  && rm -rf /var/lib/apt/lists/* 

## Install quarto (version 1.2.269)
ARG QUARTO_VERSION="1.2.269"
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb


## Install R package versions from MRAN (based on a date - YYYY-MM-DD)
RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('tidyverse'); \
"

RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('sf'); \
  install.packages('sp'); \
  install.packages('foreach'); \
  install.packages('testthat'); \
  install.packages('tidybayes'); \
  install.packages('jsonlite'); \
  install.packages('rlang'); \
  install.packages('bookdown'); \
  install.packages('rnaturalearth'); \
  install.packages('rnaturalearthdata'); \
  install.packages('patchwork'); \
  install.packages('ggnewscale'); \
  install.packages('tidybayes'); \
  install.packages('rgeos'); \
  install.packages('sn'); \
  install.packages('tidybayes'); \
  install.packages('inlabru'); \
  install.packages('cli'); \
  install.packages('Hmisc'); \
  install.packages('ncmeta'); \
  install.packages('stars'); \
  install.packages('geojsonR'); \
  install.packages('geojsonsf'); \
  install.packages('s2'); \
  install.packages('R.utils'); \
  install.packages('quarto'); \
  install.packages('ggh4x'); \
  install.packages('furrr'); \
  install.packages('gratia'); \     
  install.packages('gbm'); \ 
  install.packages('scales'); \ 
  install.packages('mgcv'); \ 
  install.packages('lubridate'); \ 
  install.packages('forcats'); \ 
  install.packages('performace'); \ 
  install.packages('emmeans'); \ 
  install.packages('ggeffects'); \ 
  install.packages('insights'); \ 
  install.packages('MuMIn'); \ 
  install.packages('glmmTMB'); \ 
  install.packages('readxl'); \ 
  install.packages('modelr'); \ 
"

## Install packages
RUN apt-get update \ 
  && apt-get install -y --no-install-recommends \ 
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  cmake \
  && rm -rf /var/lib/apt/lists/* 

RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('devtools'); \ 
  devtools::install_github(\"yutannihilation/ggsflabel@a489481bec896f874c95ed6016baf38f21b79cc3\"); \ 
" 


RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('glmmTMB'); \ 
" 

RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('performance'); \ 
" 

RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('doParallel'); \ 
  install.packages('parameters'); \ 
  install.packages('DHARMa'); \ 
  install.packages('broom.mixed'); \ 
  install.packages('mgcViz'); \ 
  install.packages('gridGraphics'); \ 
  install.packages('kableExtra'); \ 
" 

RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('officer'); \ 
  install.packages('officedown'); \ 
" 

## Create project directory in docker image 
RUN mkdir /home/Project

## Copy scripts and parameters (folders and contents) into docker image project directory
## COPY scripts/ /home/Project/scripts/ 
## COPY docs/ /home/Project/docs/ 
WORKDIR /home/Project/ 