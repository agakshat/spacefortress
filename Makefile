PYTHONLIB = libssfcairo.so
TARGET = ssf/$(PYTHONLIB)

.PHONY = clean lib

lib: $(TARGET)

$(TARGET): build/lib.*/$(TARGET)
	cp build/lib.*/$(TARGET) ssf/

build/lib.*/$(TARGET):
	python setup.py build_ext

clean:
	rm -f ssf/*.so
	rm -rf build dist
