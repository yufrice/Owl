FROM yufrice/keter-build:latest

ARG GitLab_Token="$GitLab_Token"
ARG ReleaseVer="$ReleaseVer"

# pull.sh deps
RUN apt-get update \
    && apt-get install -y curl unzip --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

COPY keter-config.yaml /opt/keter/etc/keter-config.yaml
COPY pull.sh .
RUN ./pull.sh

EXPOSE 8484

CMD ["/opt/keter/bin/keter", "/opt/keter/etc/keter-config.yaml"]