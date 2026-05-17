FROM rocker/shiny:4.3.2

RUN apt-get update && apt-get install -y libpq-dev libssl-dev && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages(c("DBI", "RPostgres", "httr", "jsonlite"), repos="https://cran.rstudio.com")'

COPY app.R /srv/shiny-server/app/app.R

RUN echo 'DB_HOST=db\nDB_PORT=5432\nDB_NAME=news_analytics\nDB_USER=ttrss' >> /home/shiny/.Renviron

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]