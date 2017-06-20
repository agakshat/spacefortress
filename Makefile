PYTHONLIB = libssfcairo.so
TARGET = ssf/$(PYTHONLIB)

.PHONY = clean lib

lib: $(TARGET)

$(TARGET): build/lib.*/$(TARGET)
	cp build/lib.*/$(TARGET) ssf/

build/lib.*/$(TARGET):
	python setup.py build_ext

clean:
	rm -rf build dist
	rm -f `find . -name "*.pyc"`
	rm -f `find . -name "*.so"`
	rm -f *.avi *.log *.h5f *.png
