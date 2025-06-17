IMPLS=gauche guile chicken chibi sagittarius gambit chez

all: $(IMPLS)

.PHONY: $(IMPLS) clean

gauche:
	gosh -r 7 -I . test7.scm

chicken:
	csi -R r7rs -e '(include "prolog.sld")' -s test7.scm

guile:
	guile -x .sld -L . test7.scm

chibi:
	chibi-scheme -I . test7.scm

sagittarius:
	sagittarius -S .sld -r 7 -L . test7.scm

chez:
	CHEZSCHEMELIBDIRS=${CHEZSCHEMELIBDIRS}:$(shell pwd) scheme --script test6.scm

clean:
	rm -f *.log
