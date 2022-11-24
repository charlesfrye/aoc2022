PHONY: help
.DEFAULT_GOAL := help

help: ## get a list of all the targets, and their short descriptions
	@# source for the incantation: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.*?##"}; {printf "\033[36m%-12s\033[0m %s\n", $$1, $$2}'


rust-dir: ## set up a subfolder for a rust solution for the given $DAY
	@$(eval from := rust/aoc00)
	@$(eval target := rust/aoc${DAY})
	mkdir -p $(target)
	cp -r $(from)/src $(from)/answer.sh $(from)/Makefile $(from)/input.txt $(from)/test.txt $(from)/Cargo.toml $(from)/.devcontainer $(target)

haskell-dir: ## set up a subfolder for a haskell solution for the given $DAY
	@$(eval from := haskell)
	@$(eval target := haskell/aoc${DAY})
	mkdir -p $(target)
	cp -r $(from)/Makefile $(from)/answer.sh $(target)
	@echo "NEXT: enter $(target) and run 'stack new --bare aoc${DAY}'"
	@echo "      then change DAY in answer.sh from 00 to ${DAY}"
