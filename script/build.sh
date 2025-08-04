pushd gambit
git apply ../index_html.patch
make clean
./configure \
  --enable-single-host \
  --enable-targets=js \
  --enable-defailt-compile-options="(compactness 9)" \
  --prefix=`pwd`
make -j`nproc`
make doc TEXI2HTML=`which texi2html`
make modules
make -C contrib/try try
cp ../scheme/src/* ./contrib/try/try
popd
