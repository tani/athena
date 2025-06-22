IMPLS=racket gauche guile chicken chibi sagittarius gambit chez biwa

all: $(IMPLS)

.PHONY: $(IMPLS) clean

gauche:
	gosh -r 7 -I . test.seven.scm

chicken:
	csi -R r7rs -e '(include "prolog.sld")' -s test.seven.scm

racket:
	racket test.rkt

guile:
	guile -x .sld -L . test.seven.scm

chibi:
	chibi-scheme -I . test.seven.scm

sagittarius:
	sagittarius -S .sld -r 7 -L . test.seven.scm

chez:
	scheme --libdirs $$CHEZSCHEMELIBDIRS:$$PWD --script test.chez.scm

gambit:
	gsi -:r7rs test.gambit.scm

biwa:
	biwas test.biwa.scm

clean:
	rm -f *.log
