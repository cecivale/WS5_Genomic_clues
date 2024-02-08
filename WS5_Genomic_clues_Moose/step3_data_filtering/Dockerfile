#FROM openjdk:8
FROM jlesage/baseimage-gui:alpine-3.15-v4
RUN add-pkg tar
RUN add-pkg curl
#RUN apt-get update && apt-get install -y curl  
#RUN apt-get update && apt-get install -y tar
RUN curl https://ormbunkar.se/aliview/downloads/linux/linux-version-1.28/aliview.tgz --output aliview.tgz
RUN tar -xf aliview.tgz 
#RUN chmod +x ./aliview
CMD java -jar aliview/aliview.jar
