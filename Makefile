VERSION = "0.4.0"

build:
	elm make src/Main.elm --optimize --output=main.js
	docker build -t samply/blaze-quality-reporting-ui:${VERSION} .

push:
	docker push samply/blaze-quality-reporting-ui:${VERSION}

build-dev:
	elm make src/Main.elm --output=main.js

format:
	elm-format src --yes

review:
	elm-review

.PHONY: build build-dev format review
