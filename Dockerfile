FROM r-base:latest

# Proxy settings (for Insee network)
ENV http_proxy=http://proxy-rie.http.insee.fr:8080
ENV https_proxy=http://proxy-rie.http.insee.fr:8080

# Install standard debian and R dependencies for R package development
RUN apt-get update && apt-get -y install libssl-dev libcurl4-openssl-dev libssh2-1-dev libxml2-dev git-core
RUN R -e "install.packages(c('devtools', 'roxygen2', 'testthat'), repos='https://cran.rstudio.com/')"

# Add all files of the package in /home/pkg
ADD . /home/pkg

# Install R packages specific to the tested package
RUN R -e "install.packages(c('sampling', 'data.table', 'vardpoor', 'magrittr', 'tibble'), repos='https://cran.rstudio.com/')"

# Build and check
RUN cd /home/pkg && \
  R CMD build . --no-build-vignettes --no-manual && \
  PKG_FILE_NAME=$(ls -1t *.tar.gz | head -n 1) && \
  R CMD check "${PKG_FILE_NAME}" --no-build-vignettes --no-manual

