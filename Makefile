NAME=nix-lisp
BINDIR=$(HOME)/bin
BINARY=$(BINDIR)/$(NAME)
SCRIPT=$(PWD)/$(NAME)
CL=cl-launch

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	$(CL) --output $(NAME) --dump ! --lisp sbcl --quicklisp --dispatch-system $(NAME)/nix --system $(NAME)

install: $(NAME)
	ln -sf $(SCRIPT) $(BINDIR)/nix

clean:
	rm -f $(NAME)

