FROM rocker/shiny

ARG HOSTNAME
ARG PORT
ARG SID
ARG USERNAME
ARG PASSWORD

ENV HOSTNAME=$HOSTNAME
ENV PORT=$PORT
ENV SID=$SID
ENV USERNAME=$USERNAME
ENV PASSWORD=$PASSWORD

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev\
    libssl-dev\
    libxml2-dev\
    librsvg2-dev\
    alien\
    git\
    curl\
    libaio1\
    openjdk-8-jdk\
    openjdk-8-jre

WORKDIR /tmp

RUN curl -LJO "https://download.oracle.com/otn_software/linux/instantclient/1917000/oracle-instantclient19.17-devel-19.17.0.0.0-1.x86_64.rpm"\
	&& curl -LJO "https://download.oracle.com/otn_software/linux/instantclient/1917000/oracle-instantclient19.17-basiclite-19.17.0.0.0-1.x86_64.rpm"\
  && curl -LJO "https://cran.r-project.org/src/contrib/ROracle_1.3-1.1.tar.gz"\
  && curl -LJO "https://download.oracle.com/otn-pub/otn_software/jdbc/218/ojdbc8.jar"

RUN sudo alien -i oracle-instantclient19.17-devel-19.17.0.0.0-1.x86_64.rpm\
  && sudo alien -i oracle-instantclient19.17-basiclite-19.17.0.0.0-1.x86_64.rpm 

RUN Rscript -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "remotes::install_github('rstudio/renv')"

WORKDIR /srv/shiny-server

RUN git clone https://github.com/sktrinh12/kinome-tree.git

RUN sudo R CMD javareconf JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"

RUN cd kinome-tree && Rscript -e 'renv::restore()'\
  && Rscript -e 'install.packages(c("DBI", "rJava", "RJDBC"))'

RUN sudo R CMD INSTALL\
  --configure-args="--with-oci-lib=/usr/lib/oracle/19.17/client64/lib --with-oci-inc=/usr/include/oracle/19.17/client64" /tmp/ROracle_1.3-1.1.tar.gz\
  && sudo chown -R shiny:shiny /srv/shiny-server/kinome-tree\
  && sudo mkdir -p /home/shiny/lib/ojdbc8\
  && sudo mv /tmp/ojdbc8.jar /home/shiny/lib/ojdbc8

USER shiny
