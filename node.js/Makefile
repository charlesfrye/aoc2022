PHONY: help
.DEFAULT_GOAL := help

help:  ## get a list of all the targets, and their short descriptions
	@# source for the incantation: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.*?##"}; {printf "\033[36m%-12s\033[0m %s\n", $$1, $$2}'


solution:  ## print the answers to both parts and their tests
	./answer.sh

tests:  ## run just tests

environment:  ## finish env setup
	npm install eslint typescript ts-node
	eslint --init
	npm install @typescript-eslint/eslint-plugin @typescript-eslint/parser
	@echo "now add the key 'project' with value './aocXX/tsconfig.json' to the .eslintrc.json parserOptions"
