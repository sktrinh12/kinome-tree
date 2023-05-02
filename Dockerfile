FROM ubuntu:20.04 

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y\
		software-properties-common\
    gnupg

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9\
    && add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'

RUN apt-get install -y\
    r-base\
    sudo\
    libcurl4-openssl-dev\
    libssl-dev\
    libxml2-dev\
    librsvg2-dev\
    alien\
    git\
    curl\
    libaio1\
    openjdk-8-jdk\
    openjdk-8-jre\
    vim

RUN useradd -ms /bin/bash shiny 

WORKDIR /tmp

RUN curl -LJO "https://download.oracle.com/otn_software/linux/instantclient/1917000/oracle-instantclient19.17-devel-19.17.0.0.0-1.x86_64.rpm"\
	&& curl -LJO "https://download.oracle.com/otn_software/linux/instantclient/1917000/oracle-instantclient19.17-basiclite-19.17.0.0.0-1.x86_64.rpm"\
  && curl -LJO "https://cran.r-project.org/src/contrib/ROracle_1.3-1.1.tar.gz"\
  && curl -LJO "https://download.oracle.com/otn-pub/otn_software/jdbc/218/ojdbc8.jar"

RUN sudo alien -i oracle-instantclient19.17-devel-19.17.0.0.0-1.x86_64.rpm\
  && sudo alien -i oracle-instantclient19.17-basiclite-19.17.0.0.0-1.x86_64.rpm 

COPY ./ /home/shiny/app/

WORKDIR /home/shiny/app

RUN sudo R CMD javareconf JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"

RUN Rscript install_packages.R

RUN sudo R CMD INSTALL\
  --configure-args="--with-oci-lib=/usr/lib/oracle/19.17/client64/lib --with-oci-inc=/usr/include/oracle/19.17/client64" /tmp/ROracle_1.3-1.1.tar.gz\
  && sudo chown -R shiny:shiny /home/shiny/app\
  && sudo mkdir -p /home/shiny/lib/ojdbc8\
  && sudo mv /tmp/ojdbc8.jar /home/shiny/lib/ojdbc8

USER shiny

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

EXPOSE 80

CMD ["R", "-e", "shiny::runApp('/home/shiny/app', host= '0.0.0.0', port = 80)"]
