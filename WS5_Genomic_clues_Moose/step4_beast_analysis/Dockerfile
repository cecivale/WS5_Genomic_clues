FROM ubuntu

WORKDIR /app
COPY START START
ENV PATH="$PATH:."

RUN apt-get update
RUN apt-get install -y openjdk-17-jdk openjfx 
RUN apt-get install pv 

RUN ["chmod", "+x", "START"]
CMD bin/sh

