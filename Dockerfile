FROM rstudio/plumber

ARG PLUMBER_PORT=8080
# create the application folder
RUN mkdir -p ~/application
# Install java and rJava
RUN apt-get -y update && apt-get install -y \
   default-jdk \
   r-cran-rjava \
   && apt-get clean \
   && rm -rf /var/lib/apt/lists/

# copy everything from the current directory into the container
COPY "/" "application/"
WORKDIR "application/"

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install()"

EXPOSE $PLUMBER_PORT
# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "extras/RunPlumber.R"]