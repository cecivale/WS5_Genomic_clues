FROM evolbioinfo/mafft:v7.520
WORKDIR /app
COPY START START
ENV PATH="$PATH:."
RUN apt-get update && apt-get install pv  && apt-get install r-base
RUN ["chmod", "+x", "START"]
CMD bin/sh

