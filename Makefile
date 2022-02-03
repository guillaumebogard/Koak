##
## EPITECH PROJECT, 2022
## makefile
## File description:
## KOAK
##

MAKEFLAGS	+=	--no-print-directory
BINARY_PATH	:=	$(shell stack path --local-install-root)

all:
	stack build
	cp $(BINARY_PATH)/bin/koak-exe ./koak

debug:
	stack build --pedantic
	cp $(BINARY_PATH)/bin/koak-exe ./koak

clean:
	stack purge
	stack clean

fclean:	clean
	rm -f koak

tests_run:	clean
	stack test

re:	fclean all

.PHONY:	all clean fclean re debug test
