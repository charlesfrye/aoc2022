PHONY: help
.DEFAULT_GOAL := help

help: ## get a list of all the targets, and their short descriptions
	@# source for the incantation: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.*?##"}; {printf "\033[36m%-12s\033[0m %s\n", $$1, $$2}'


rust-dir: ## set up a subfolder for a rust solution for the given $DAY
	mkdir -p rust/aoc${DAY}
	cp -r rust/aoc00/src rust/aoc00/.devcontainer rust/aoc${DAY}
	echo "run cargo init inside the devcontainer to get started"
