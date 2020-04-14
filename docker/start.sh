#!/bin/sh

mkdir -p "/usr/share/nginx/html${CONTEXT_PATH:-}"

# Adds MD5 sums to the main.css
STYLE_MD5SUM=$(md5sum /usr/share/nginx/html/main.css | cut -d ' ' -f1)
mv /usr/share/nginx/html/main.css "/usr/share/nginx/html${CONTEXT_PATH:-}/main-${STYLE_MD5SUM}.css"
sed -i "s#/main.css#${CONTEXT_PATH:-}/main-${STYLE_MD5SUM}.css#" /usr/share/nginx/html/index.html

# Add the MD5 hash into the main.js filename in order to support cache invalidation
MAIN_MD5SUM=$(md5sum /usr/share/nginx/html/main.js | cut -d ' ' -f1)
mv /usr/share/nginx/html/main.js "/usr/share/nginx/html${CONTEXT_PATH:-}/main-${MAIN_MD5SUM}.js"
sed -i "s#/main.js#${CONTEXT_PATH:-}/main-${MAIN_MD5SUM}.js#" /usr/share/nginx/html/index.html

# Replace start.sh with nginx
exec nginx -g "daemon off;"
