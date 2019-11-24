FROM alpine as build

RUN apk --update add curl

WORKDIR /build

RUN curl -s -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm

COPY src /build/src
COPY elm.json /build/

RUN ./elm make src/Main.elm --optimize --output=main.js

FROM nginx:1.17

RUN rm /usr/share/nginx/html/*
COPY --from=build /build/main.js /usr/share/nginx/html/
COPY main.css /usr/share/nginx/html/
COPY mdc-data-table.css /usr/share/nginx/html/
COPY index.html /usr/share/nginx/html/

COPY docker/nginx.conf /etc/nginx/
COPY docker/start.sh /

RUN chmod +x /start.sh
CMD ["/start.sh"]
