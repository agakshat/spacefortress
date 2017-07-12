#!/bin/bash
set -e -x

yum install -y git cairo-devel

cd /io/python/spacefortress

# Compile wheels
for PYBIN in /opt/python/*/bin; do
  PYVER=`${PYBIN}/python -c "import platform; print(platform.python_version())"`
  if [ ${PYVER:0:3} != 2.6 ]; then
	  "${PYBIN}/pip" install -r dev-requirements.txt
	  "${PYBIN}/python" setup.py bdist_wheel
  fi
done

for whl in dist/*.whl; do
    auditwheel repair "$whl" -w wheels/
done
