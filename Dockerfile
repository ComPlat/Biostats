FROM rocker/shiny:4.0.5

# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    golang-go \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true

RUN install2.r --error --skipinstalled \
    shiny \
    shinydashboard \
    shinyWidgets \
    shinyjs \
    forecast \
    jsonlite \
    ggplot2 \
    htmltools \
    plotly \
    drc \
    DT \
    httr \
    agricolae \
    broom \
    readxl \
    openxlsx \
    purrr \
    RColorBrewer \
    remotes
    
RUN bash -c "cd /home; cd shiny; mkdir txtq"
USER shiny
COPY ./app/ ./myapp
COPY ./tcp .
RUN mkdir /home/shiny/results

USER root
RUN R -e 'install.packages("/myapp/MTT", repos= NULL, type = "source")'
RUN R -e 'remotes::install_github("ComPlat/comeln")'
#USER shiny # not the best idea --> but otherwise go has not the permission for download

EXPOSE 4001
COPY ./app/ /srv/shiny-server/
COPY ./run.sh .

ENV SHINY_LOG_STDERR=1

CMD ["/bin/bash", "-c", "./run.sh"]