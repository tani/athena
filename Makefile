IMPLS=gauche guile chicken chibi sagittarius gambit

all: $(IMPLS)

.PHONY: $(IMPLS) clean

gauche:
	gosh -I . test.scm

chicken:
	csi -R r7rs -e '(include "prolog.scm")' -s test.scm

guile:
	guile -L . test.scm

chibi:
	chibi-scheme -I . test.scm

sagittarius:
	sagittarius -L . test.scm

gambit:
	gsi -:r7rs . test.scm

clean:
	rm -f *.log
