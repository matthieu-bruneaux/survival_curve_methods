### * Description

# Matthieu on 2023-01-18:
#
# This Makefile is written to help with some basic tasks when working on a
# Linux system. It can probably work reasonably well on a MacOS, but I did not
# test it on this system.
#
# It could also potentially be useful when automating some tasks in GitHub
# Actions or GitLab CI.

### * Setup

TOP_DIR=$(shell git rev-parse --show-toplevel)

### ** Colors

# https://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux
# https://siongui.github.io/2016/02/18/makefile-echo-color-output/#id3
RED = "\\033[31m"
BLUE = "\\033[94m"
GREEN = "\\033[92m"
NC = "\\033[0m"

### * Rules

### ** help

# http://swcarpentry.github.io/make-novice/08-self-doc/
# https://stackoverflow.com/questions/19129485/in-linux-is-there-a-way-to-align-text-on-a-delimiter
.PHONY: help
help: Makefile
	@printf "\n"
	@printf "Please use 'make <target>' where <target> is one of\n"
	@printf "\n"
	@sed -n 's/^## /    /p' $< | column -t -s ":"

### ** readme

## readme : render 'README.md' from 'README.Rmd'
.PHONY: readme
readme:
	@printf "\n"
	@printf "$(GREEN)*** Rendering 'README.md' from 'README.Rmd' ***$(NC)\n"
	@printf "\n"
	@Rscript -e 'rmarkdown::render("README.Rmd", quiet = TRUE)'

