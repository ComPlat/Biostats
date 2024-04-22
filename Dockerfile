FROM rocker/shiny:4.3.1

RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
ENV SHINY_LOG_STDERR=1

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
    pheatmap \
    png \
    RColorBrewer \
    remotes \
    xml2 \
    xlsx \
    openssl \
    ggpmisc \
    jose \
    R6 \
    patchwork \
    cowplot \
    quarto
    
USER shiny
COPY ./app/ ./myapp
RUN mkdir /home/shiny/results

COPY ./MTT/ /home/MTT
COPY ./comeln/ /home/comeln
USER root
RUN bash -c "cd /home/MTT; R CMD INSTALL ."
RUN bash -c "cd /home/comeln; R CMD INSTALL ."

EXPOSE 4001
COPY ./app/ /srv/shiny-server/
COPY ./run.sh .

ENV SHINY_LOG_STDERR=1

CMD ["/bin/bash", "-c", "./run.sh"]

