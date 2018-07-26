FROM yufrice/yesod-hlint:latest

EXPOSE 80

COPY . /
RUN stack build
CMD stack exec -- yesod devel