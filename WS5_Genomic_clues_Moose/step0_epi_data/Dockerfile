FROM ubuntu
WORKDIR /scripts
COPY start start
COPY fig1_merscov.pdf fig1_merscov.pdf
RUN apt-get update && apt-get install pv
#RUN apt-get update && apt-get install xdg-utils
#CMD ["Rscript", "myscript.R"]
RUN ["chmod", "+x", "start"]
#ENTRYPOINT [ "./step0_chat.sh" ]
CMD ["sh", "start"]