git -C gambit apply ../index_html.patch
make -C gambit clean
pushd gambit
./configure \
  --enable-single-host \
  --enable-targets=js \
  --enable-defailt-compile-options="(compactness 9)" \
  --prefix=`pwd`
popd
make -C gambit -j8
make -C gambit doc TEXI2HTML=`which texi2html`
make -C gambit modules
make -C gambit/contrib/try try
cp src/prolog.* gambit/contrib/try/try
