FROM r-base:latest

# Proxy settings (for Insee network)
ENV http_proxy=http://proxy-rie.http.insee.fr:8080
ENV https_proxy=http://proxy-rie.http.insee.fr:8080

# Install standard debian and R dependencies for R package development
RUN apt-get update && apt-get -y install libssl-dev libcurl4-openssl-dev libssh2-1-dev libxml2-dev git-core pandoc
RUN R -e "install.packages(c('devtools', 'roxygen2', 'testthat'), repos='https://cran.rstudio.com/')"

# Add all files of the package in /home/pkg
ADD . /home/pkg

# Install dependencies, build and check package
RUN cd /home/pkg && \
  R -e "devtools::install_deps('.', c('Depends', 'Imports', 'Suggests'))" && \
  R CMD build . --no-build-vignettes --no-manual && \
  PKG_FILE_NAME=$(ls -1t *.tar.gz | head -n 1) && \
  R CMD check "${PKG_FILE_NAME}" --no-build-vignettes --no-manual

# Error in build if not "Status: OK" on the last line of log
RUN cd /home/pkg && \
  PKG_NAME=$(ls -1t *.tar.gz | head -n 1 | cut -d'_' -f 1) && \
  LAST_LINE=$(tail -1 /home/pkg/${PKG_NAME}.Rcheck/00check.log) && \
  if [ "${LAST_LINE}" != "Status: OK" ]; then exit 1 ; fi