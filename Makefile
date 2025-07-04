IMPLS=racket gauche guile chicken chibi sagittarius gambit chez

all: $(IMPLS)

.PHONY: $(IMPLS) clean

gauche:
	gosh -r 7 -I src test/test.seven.scm

chicken:
	csi -require-extension r7rs -include-path src -eval '(include "src/prolog.sld")' -script test/test.seven.scm

racket:
	racket test/test.rkt

guile:
	guile --fresh-auto-compile -x .sld -L src test/test.seven.scm

chibi:
	chibi-scheme -I src -I test test/test.seven.scm

sagittarius:
	sagittarius --clean-cache --disable-cache --loadsuffix=.sld --standard=7 --loadpath=src test/test.seven.scm

chez:
	scheme --libdirs $$CHEZSCHEMELIBDIRS:$$PWD/src --script test/test.chez.scm

gambit:
	gsi -:r7rs src/ test/test.seven.scm

clean:
	rm -f *.log test/*.log
