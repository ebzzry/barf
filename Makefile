NAME=nix-lisp
BINDIR=$(HOME)/bin
BINARY=$(BINDIR)/$(NAME)
SCRIPT=$(PWD)/$(NAME)
CL=cl-launch

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	$(CL) --output $(PWD)/$(NAME) --dump ! --lisp sbcl --quicklisp --system $(NAME) --dispatch-system $(NAME)/nix

install: $(NAME)
	ln -sf $(SCRIPT) $(BINDIR)/nix

clean:
	rm -f $(NAME)

