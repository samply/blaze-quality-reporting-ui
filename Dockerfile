FROM nginx:alpine

RUN rm /usr/share/nginx/html/*
COPY dist/* /usr/share/nginx/html/

COPY docker/nginx.conf /etc/nginx/
