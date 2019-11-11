build:
	elm make src/Main.elm --optimize --output=main.js

build-dev:
	elm make src/Main.elm --output=main.js

.PHONY: build, build-dev
