FROM rocker/r-ver:4.3.2

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      ca-certificates \
      cron \
      g++ \
      libcurl4-openssl-dev \
      libicu-dev \
      libpq-dev \
      libssl-dev \
      libxml2-dev \
      make \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages(c("httr2", "jsonlite", "purrr", "dplyr", "tidyr", "readr", "stringr", "tibble", "DBI", "RPostgres"), repos = "https://cloud.r-project.org")'

WORKDIR /app

COPY . /app/etl/
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh

RUN chmod +x /usr/local/bin/docker-entrypoint.sh \
    && mkdir -p /etc/etl /var/log /app/data-raw /app/etl/output \
    && touch /var/log/etl.log \
    && printf '%s\n' \
      'SHELL=/bin/sh' \
      'PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin' \
      '0 * * * * root . /etc/etl/container.env; cd /app && Rscript /app/etl/run_etl.R >> /var/log/etl.log 2>&1' \
      > /etc/cron.d/etl \
    && chmod 0644 /etc/cron.d/etl

ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
CMD ["cron", "-f"]
