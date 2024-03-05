#FROM debian:stable

#RUN apt-get update
#RUN apt-get install -y openjdk-17-jdk openjfx 

FROM openjdk

WORKDIR /app
COPY START START
#ENV PATH="$PATH:."

#RUN apt-get update
#RUN apt-get install pv 

#ADD . ./
#ENV JAVA_FX_HOME=/usr/share/java/ 

RUN ["chmod", "+x", "START"]
CMD bin/sh


