FROM evolbioinfo/mafft:v7.520
WORKDIR /app
COPY START START
ENV PATH="$PATH:."
RUN apt-get update && apt-get install pv 
RUN ["chmod", "+x", "START"]
CMD bin/sh