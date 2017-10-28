PACKAGE_NAME := spacefortress.game

.PHONY = clean wheel install uninstall

wheel: dist
dist: build
build:
	@python setup.py bdist_wheel

uninstall:
	@pip uninstall --yes $(PACKAGE_NAME); true

install: dist uninstall
	@pip install --no-deps --ignore-installed dist/$(PACKAGE_NAME)*.whl

clean:
	@rm -rf wheels* *.egg* build __pycache__ dist
	@rm -f `find . -name "*.pyc"`
