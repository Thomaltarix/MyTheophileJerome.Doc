##
## EPITECH PROJECT, 2024
## MyPandoc
## File description:
## Makefile
##

NAME	=	mypandoc

REPO 	= 	MyPandoc

DIRECTORY = $(shell stack path --local-install-root)
COVERAGE = test/coverage/$(REPO)-test.tix

all: $(NAME)

$(NAME):
	@echo -ne "\nCompilation: "
	@stack build
	@cp $(DIRECTORY)/bin/$(REPO)-exe $(NAME)
	@echo -e "\033[92mDone\n\033[0m"

clean:
	@echo -ne "Clean: "
	@stack clean
	@echo -e "\033[92m Done\033[0m"

fclean: clean
	@echo -ne "Fclean: "
	@rm -f $(NAME)
	@echo -e "\033[92mDone\033[0m"

re:	fclean all
