FIND_JS = find server/src web/src -name "*.js" ! -name "*node_modules*"

.PHONY: all
all: deps build

.PHONY: heroku
heroku: all

.PHONY: deps
deps:
	yarn
	cd server && yarn
	cd web && elm package install -y

.PHONY: start
start:
	cd server && yarn start

.PHONY: start-dev
start-dev:
	cd server && yarn run start:dev

.PHONY: serve
serve:
	cd web && elm-live src/Main.elm --pushstate --port=8080 --debug

.PHONY: build
build:
	cd web && elm make src/Main.elm --output=build/index.html

.PHONY: build-dev
build-dev:
	cd web && elm make src/Main.elm --output=build/dev/index.html --debug

.PHONY: lint
lint:
	cd web && elm make src/Main.elm --warn --output=/dev/null
	$(FIND_JS) | xargs ./node_modules/.bin/eslint

.PHONY: pretty
pretty:
	elm-format web/src/ --yes
	$(FIND_JS) | xargs ./node_modules/.bin/prettier --write --tab-width 4

.PHONY: db
db:
	mongod --dbpath ./data

.PHONY: test
test: test-elm
	./node_modules/.bin/jest --env=jsdom

.PHONY: test-elm
test-elm: 
	cd web && elm-test

.PHONY: cover
cover: test-elm
	./node_modules/.bin/jest --env=jsdom --coverage

.PHONY: clean
clean:
	rm -rf node_modules
	rm -rf web/elm-stuff
	rm -rf server/node_modules

.PHONY: test-watch-api
test-watch-api:
	./node_modules/.bin/jest --env=jsdom --watch

.PHONY: test-watch-elm
test-watch-elm:
	cd web && elm-test --watch

.PHONY: docs
docs:
	cd server && npm run docs

.PHONY: shell
shell:
	nix-shell --command zsh
