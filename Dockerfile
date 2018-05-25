FROM rocker/r-base

ENV http_proxy=http://proxy-rie.http.insee.fr:8080
ENV https_proxy=http://proxy-rie.http.insee.fr:8080

RUN apt-get update
RUN apt-get -y install libssl-dev libcurl4-openssl-dev git-core

RUN R -e "install.packages(c('testthat', 'sampling', 'vardpoor', 'magrittr', 'devtools'), repos='https://cran.rstudio.com/')"
RUN pwd
RUN ls
RUN R CMD build . --no-build-vignettes --no-manual
RUN PKG_FILE_NAME=$(ls -1t *.tar.gz | head -n 1)
RUN R CMD check "${PKG_FILE_NAME}" --no-build-vignettes --no-manual
RUN R -e 'devtools::test()'
