VERSION=0.1.0

build:
	elm make src/Main.elm --optimize --output=main.js
	docker build -t samply/blaze-qr-ui:$(VERSION) .

build-dev:
	elm make src/Main.elm --output=main.js

.PHONY: build, build-dev
