##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## koak
##

MAKEFLAGS	+=	--no-print-directory
BINARY_PATH	:=	$(shell stack path --local-install-root)

STACK_NAME	=	koak

NAME		=	koak

all:	$(NAME)
.PHONY:	all

$(NAME):
	stack build --pedantic
	cp $(BINARY_PATH)/bin/$(STACK_NAME)-exe ./$(NAME)
.PHONY:	$(NAME)

debug:
	stack ghci
.PHONY:	debug

clean:
	stack clean
.PHONY:	clean

fclean:	clean
	stack purge
	rm -f $(NAME)
.PHONY:	fclean

tests_run:
	stack test --coverage
.PHONY:	tests_run

re::	fclean
re::	all
.PHONY:	re
