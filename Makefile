IMPLS=racket gauche guile chicken chibi sagittarius gambit chez biwa mit-scheme

all: $(IMPLS)

.PHONY: $(IMPLS) clean

gauche:
	gosh -r 7 -I src test/test.seven.scm

chicken:
	csi -R r7rs -I src -e '(include "src/prolog.sld")' -s test/test.seven.scm

racket:
	racket test/test.rkt

guile:
	guile -x .sld -L src test/test.seven.scm

chibi:
	chibi-scheme -I src -I test test/test.seven.scm

sagittarius:
	sagittarius -S .sld -r 7 -L src test/test.seven.scm

chez:
	scheme --libdirs $$CHEZSCHEMELIBDIRS:$$PWD/src --script test/test.chez.scm

gambit:
	gsi -:r7rs src/ test/test.seven.scm

biwa:
	biwas test/test.biwa.scm

clean:
	rm -f *.log test/*.log
