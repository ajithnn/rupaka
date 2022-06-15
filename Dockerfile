FROM fpco/stack-build-small:lts-19

WORKDIR /app

ARG SSH_PRIVATE_KEY
ARG SRC_TAG

RUN mkdir /root/.ssh/
RUN echo "${SSH_PRIVATE_KEY}" > /root/.ssh/id_rsa
RUN chmod 600 /root/.ssh/id_rsa 
RUN chmod 700 /root/.ssh 
RUN touch /root/.ssh/known_hosts 
RUN chmod 644 /root/.ssh/known_hosts 
RUN  apt-get -yq update && apt-get -yqq install ssh
RUN ssh-keyscan github.com >> /root/.ssh/known_hosts 

RUN mkdir /app/run 
RUN git clone git@github.com:ajithnn/rupaka.git /app/run/rupaka
RUN cd /app/run/rupaka && git fetch && git fetch --tags
RUN cd /app/run/rupaka && git checkout ${SRC_TAG}

RUN cd /app/run/rupaka && stack setup
RUN cd /app/run/rupaka && CONFIG_FILEPATH="./new_sample.cfg" stack build --force-dirty --ghc-options -O --ghc-options -fforce-recomp && stack exec rupaka-exe -- "output.json"

ENTRYPOINT ["/bin/bash","/app/run/rupaka/init.sh"]
