FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y libpq-dev libssl-dev libcurl4-openssl-dev && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages(c("plumber", "httr2", "jsonlite", "DBI", "RPostgres", "dotenv", "devtools"), repos="https://cran.rstudio.com")'

COPY . /app/mcp_server/

WORKDIR /app

EXPOSE 8000

CMD ["Rscript", "-e", "library(plumber); r <- plumb('/app/mcp_server/mcp_server.R'); r$run(port=8000, host='0.0.0.0')"]