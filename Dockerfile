FROM fpco/stack-build-small:lts-19 as intermediate

WORKDIR /app

ARG SRC_TAG

RUN mkdir /app/run 
RUN git clone https://github.com/ajithnn/rupaka.git /app/run/rupaka
RUN cd /app/run/rupaka && git fetch && git fetch --tags
RUN cd /app/run/rupaka && git checkout ${SRC_TAG}

RUN cd /app/run/rupaka && stack setup
RUN cd /app/run/rupaka && stack build --ghc-options -O
RUN cd /app/run/rupaka && stack --no-terminal install

FROM alpine:3.14 

WORKDIR /app 

RUN mkdir /app/rupaka

RUN apk update && apk upgrade && apk add bash

COPY --from=intermediate /root/.local/bin/rupaka /app/rupaka/
COPY --from=intermediate /app/run/rupaka/init.sh /app/rupaka/init.sh

ENTRYPOINT ["/bin/bash","/app/rupaka/init.sh"]
