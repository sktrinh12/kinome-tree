FROM 352353521492.dkr.ecr.us-west-2.amazonaws.com/rshiny-oracle:latest

ARG VERSION_NUMBER
ARG ENVIRONMENT 
ENV VERSION_NUMBER=$VERSION_NUMBER
ENV ENVIRONMENT=$ENVIRONMENT
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update

WORKDIR /home/shiny/app

COPY ./ .

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

CMD ["R",  "-e", "shiny::runApp('/home/shiny/app', host='0.0.0.0', port=80)"]
