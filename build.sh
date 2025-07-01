pushd gambit
make clean
./configure --enable-single-host --enable-targets=js --enable-defailt-compile-options="(compactness 9)" --prefix=`pwd`
make -j8
make doc TEXI2HTML=`which texi2html`
make modules
make -C contrib/try try
popd
