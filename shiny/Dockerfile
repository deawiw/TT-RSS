FROM rocker/shiny:4.3.2

RUN apt-get update && apt-get install -y libpq-dev libssl-dev && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages(c("DBI", "RPostgres"), repos="https://cran.rstudio.com")'

COPY app.R /srv/shiny-server/app/app.R

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]