FROM evolbioinfo/mafft:v7.520
WORKDIR /app
COPY START START
ENV PATH="$PATH:."
RUN apt-get update && apt-get install pv 
RUN apt-get update && apt-get install -y -qq --no-install-recommends --purge r-base
RUN ["chmod", "+x", "START"]
CMD bin/sh

