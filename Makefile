FIND_JS = find server/src web/src -name "*.js" ! -name "*node_modules*"

deps:
	yarn
	cd server && yarn
	cd web && yarn

start:
	cd server && yarn start

start-dev:
	cd server && yarn run start:dev

serve:
	cd web && yarn start

build:
	cd web && yarn run build

lint:
	$(FIND_JS) | xargs ./node_modules/.bin/eslint

pretty:
	$(FIND_JS) | xargs ./node_modules/.bin/prettier --write --tab-width 4

db:
	mongod --dbpath ./data

test:
	./node_modules/.bin/jest --env=jsdom

cover:
	./node_modules/.bin/jest --env=jsdom --coverage

clean:
	rm -rf node_modules
	rm -rf web/node_modules
	rm -rf server/node_modules

test-watch:
	./node_modules/.bin/jest --env=jsdom --watch

docs:
	cd server && npm run docs

shell:
	nix-shell --command zsh