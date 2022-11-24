PHONY: help
.DEFAULT_GOAL := help

help:  ## get a list of all the targets, and their short descriptions
	@# source for the incantation: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.*?##"}; {printf "\033[36m%-12s\033[0m %s\n", $$1, $$2}'


answer:  ## print the answers to both parts and their tests
	cargo build
	@./answer.sh

test:  ## check whether the test answers are correct
	RUST_BACKTRACE=1 cargo test
