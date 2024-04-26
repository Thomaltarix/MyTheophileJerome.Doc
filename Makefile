##
## EPITECH PROJECT, 2024
## MyPandoc
## File description:
## Makefile
##

NAME	=	mypandoc

REPO 	= 	MyPandoc

DIRECTORY = $(shell stack path --local-install-root)

COVERAGE_NAME = MyPandoc-test

COVERAGE_PATH = \
	$(DIRECTORY)/hpc/$(REPO)/$(COVERAGE_NAME)/$(COVERAGE_NAME).tix


# JSON TESTS
JSON_TESTER = ./json_ftest.py

JSON_TESTS =	Json-FTests/Test-Json.json 		\
				Json-FTests/Test-Xml.json 		\

.PHONY: all clean fclean re tests_run clean_tests

all: $(NAME)

$(NAME):
	@echo -ne "\nCompilation: "
	@stack build
	@cp $(DIRECTORY)/bin/$(REPO)-exe $(NAME)
	@echo -e "\033[92mDone\n\033[0m"

clean:
	@echo -ne "Clean: "
	@stack clean
	@rm -rf .stack-work
	@echo -e "\033[92m Done\033[0m"

fclean: clean clean_tests
	@echo -ne "Fclean: "
	@rm -f $(NAME)
	@echo -e "\033[92mDone\033[0m"

re:	fclean all

tests_run: tests clean_tests json_tests

tests:
	@echo -ne "\nTests: "
	@stack test --coverage
	@mkdir -p test/coverage
	@cp $(COVERAGE_PATH) test/coverage/
	@echo -e "\033[92mDone\033[0m"

clean_tests:
	@echo -ne "Clean tests: "
	@rm -rf test/coverage
	@rm -f $(COVERAGE_PATH)
	@rm -f app/Main
	@echo -e "\033[92mDone\033[0m"

json_tests: $(NAME)
	@pip install junit-xml
	@echo -ne "\nJson tests: \n"
	@$(JSON_TESTER) $(JSON_TESTS) -d
