VERSION = "0.7.2"

build:
	npm run prod
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
