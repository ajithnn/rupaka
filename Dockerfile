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
RUN ssh-keyscan github.com >> /root/.ssh/known_hosts 

RUN mkdir /app/run 
RUN cd /app/run
RUN git clone git@github.com:ajithnn/rupaka.git rupaka
RUN cd /app/run/rupaka 
RUN git fetch && git fetch --tags
RUN git checkout ${SRC_TAG}

RUN stack --no-terminal test

ENTRYPOINT ["/bin/bash","/app/run/rupaka/init.sh"]
