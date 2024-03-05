FROM ubuntu
WORKDIR /app
COPY START START
ENV PATH="$PATH:."
RUN apt-get update && apt-get install pv 
RUN ["chmod", "+x", "START"]
CMD bin/sh