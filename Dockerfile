FROM r-base:latest

ENV http_proxy=http://proxy-rie.http.insee.fr:8080
ENV https_proxy=http://proxy-rie.http.insee.fr:8080

RUN apt-get update
RUN apt-get -y install libssl-dev libcurl4-openssl-dev git-core libssh2-1-dev libxml2-dev

RUN R -e "install.packages(c('testthat', 'sampling', 'vardpoor', 'magrittr', 'devtools'), repos='https://cran.rstudio.com/')"
RUN cd /home
RUN ls
RUN R CMD build . --no-build-vignettes --no-manual
RUN PKG_FILE_NAME=$(ls -1t *.tar.gz | head -n 1)
RUN R CMD check "${PKG_FILE_NAME}" --no-build-vignettes --no-manual
RUN R -e 'devtools::test()'
