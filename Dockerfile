FROM nginx:1.17

RUN rm /usr/share/nginx/html/*
COPY main.js /usr/share/nginx/html/
COPY main.css /usr/share/nginx/html/
COPY mdc-data-table.css /usr/share/nginx/html/
COPY index.html /usr/share/nginx/html/

COPY docker/nginx.conf /etc/nginx/
COPY docker/start.sh /

RUN chmod +x /start.sh
CMD ["/start.sh"]
