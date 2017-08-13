all: deps build

run: build exec

build:
	stack build

.PHONY: deps
deps:
	stack install

.PHONY: exec
exec:
	stack exec Birds-exe

watch:
	stack build --file-watch --exec Birds-exe

.PHONY: shell
shell:
	nix-shell --command zsh

format:
	stylish-haskell -i src/*.hs src/Api/*.hs app/*

.PHONY: db
db:
	pg_ctl start -D ./data

.PHONY: db-stop
db-stop:
	pg_ctl stop -D ./data

.PHONY: db-init
db-init:
	mkdir -p ./data
	initdb -D ./data
	pg_ctl start -D ./data -w
	psql postgres -c "CREATE DATABASE test"
	psql postgres -c "CREATE USER test WITH PASSWORD 'test'"
	pg_ctl stop -D ./data 

