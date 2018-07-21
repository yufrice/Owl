FROM haskell:latest

ARG GitLab_Token "$GitLab_Token"
ARG ReleaseVer "$ReleaseVer"

# pull.sh deps
RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y curl unzip

# keter build
RUN stack update
RUN stack setup --resolver=lts-9.21
RUN stack install keter

# keter chroot
RUN mkdir -p /opt/keter/bin
RUN mkdir /opt/keter/etc
RUN mkdir /opt/keter/incoming

RUN cp ~/.local/bin/keter /opt/keter/bin
ADD keter-config.yaml /opt/keter/etc/keter-config.yaml
ADD pull.sh .
RUN ./pull.sh

EXPOSE 8484

CMD ["/opt/keter/bin/keter", "/opt/keter/etc/keter-config.yaml"]