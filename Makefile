build:
	elm make src/Main.elm --optimize --output=main.js
	docker build .

build-dev:
	elm make src/Main.elm --output=main.js

format:
	elm-format src --yes

review:
	elm-review

.PHONY: build, build-dev, format, review
