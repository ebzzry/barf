#-------------------------------------------------------------------------------
# Head
#-------------------------------------------------------------------------------

SHELL := bash
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:


#-------------------------------------------------------------------------------
# Body
#-------------------------------------------------------------------------------

NAME=barf
BINDIR=$(HOME)/bin
BINARY=$(BINDIR)/$(NAME)
SCRIPT=$(PWD)/$(NAME)
CL=cl-launch

.PHONY: all $(NAME) clean uninstall

all: $(NAME)

$(NAME):
	@$(CL) --output $(PWD)/$(NAME) --dump ! --lisp sbcl --quicklisp --system $(NAME) --dispatch-system $(NAME)/barf

install: $(NAME)
	@ln -sf $(SCRIPT) $(BINDIR)/barf

uninstall:
	@rm -f $(BINDIR)/barf

clean:
	@rm -f $(NAME)

build:
	docker build -t barf .
