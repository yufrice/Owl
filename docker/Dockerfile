FROM haskell:latest

RUN mkdir -p /opt/keter/bin
RUN mkdir /opt/keter/etc

ADD keter-config.yaml /opt/keter/etc/keter-config.yaml
ADD pull.sh .
RUN ./pull.sh

EXPOSE 8484

CMD ["/opt/keter/bin/keter", "/opt/keter/etc/keter-config.yaml"]