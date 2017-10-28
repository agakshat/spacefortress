PACKAGE_NAME := spacefortress

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))/../../

.PHONY = clean wheel install uninstall wheels wheels32 wheels64

wheel: dist
dist: build
build:
	@python setup.py bdist_wheel

uninstall:
	@pip uninstall --yes $(PACKAGE_NAME); true

install: dist uninstall
	@pip install --no-deps --ignore-installed dist/$(PACKAGE_NAME)*.whl

wheels: wheels32 wheels64
	@mkdir -p $@
	@cp -r wheels32/* wheels/
	@cp -r wheels64/* wheels/

wheels32:
	@docker run -v $(ROOT_DIR):/io quay.io/pypa/manylinux1_i686 linux32 ./io/python/spacefortress/build-wheels.sh
	@mkdir -p $@; mv wheels/*-manylinux1* $@
	@rm -rf wheels

wheels64:
	@docker run -v $(ROOT_DIR):/io quay.io/pypa/manylinux1_x86_64 ./io/python/spacefortress/build-wheels.sh
	@mkdir -p $@; mv wheels/*-manylinux1* $@
	@rm -rf wheels

clean:
	@rm -rf wheels* *.egg* build __pycache__ dist
	@rm -f `find . -name "*.pyc"`
	@rm -f `find . -name "*.so"`
