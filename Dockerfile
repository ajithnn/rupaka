FROM fpco/stack-build-small:lts-19 as intermediate

WORKDIR /app

ARG SRC_TAG

RUN mkdir /app/run 
RUN git clone https://github.com/ajithnn/rupaka.git /app/run/rupaka
RUN cd /app/run/rupaka && git fetch && git fetch --tags
RUN cd /app/run/rupaka && git checkout ${SRC_TAG}

RUN cd /app/run/rupaka && stack setup
RUN cd /app/run/rupaka && stack build --ghc-options -O

FROM alpine:3.14 

WORKDIR /app 

RUN mkdir /app/rupaka
COPY --from=intermediate /app/run/rupaka/.stack-work/dist/x86_64-linux/Cabal-3.4.1.0/build/rupaka-exe/rupaka-exe /app/rupaka/rupaka
COPY --from=intermediate /app/run/rupaka/init.sh /app/rupaka/init.sh

RUN apk update && apk upgrade && apk add bash

ENTRYPOINT ["/bin/bash","/app/rupaka/init.sh"]
