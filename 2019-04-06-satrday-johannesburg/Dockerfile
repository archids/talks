FROM rocker/r-ver:3.5.2

RUN apt-get update -qq && \
    apt-get install -y \
      pandoc \
      libssl-dev \
      libcurl4-gnutls-dev \
      libxml2-dev

RUN R -e "install.packages(c('plumber', 'dplyr', 'plotly'))"

COPY riegel-api.R riegel-api.R

EXPOSE 8000

ENTRYPOINT ["R", "-e", "library(plumber); plumb('riegel-api.R')$run(port=8000, host='0.0.0.0')"]
