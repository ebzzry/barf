NAME=baf
BINDIR=$(HOME)/bin
BINARY=$(BINDIR)/$(NAME)
SCRIPT=$(PWD)/$(NAME)
CL=cl-launch

.PHONY: all $(NAME) clean uninstall

all: $(NAME)

$(NAME):
	@$(CL) --output $(PWD)/$(NAME) --dump ! --lisp sbcl --quicklisp --system $(NAME) --dispatch-system $(NAME)/baf

install: $(NAME)
	@ln -sf $(SCRIPT) $(BINDIR)/baf

uninstall:
	@rm -f $(BINDIR)/baf

clean:
	@rm -f $(NAME)

