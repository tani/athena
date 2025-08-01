IMPLS=racket gauche guile chicken chibi sagittarius gambit chez

all: $(IMPLS)

.PHONY: $(IMPLS) clean format format-src format-test

gauche:
	gosh -r 7 -I src test/test.7.scm

chicken:
	csi -require-extension r7rs -include-path src -eval '(include "src/prolog.sld")' -script test/test.7.scm

racket:
	racket test/test.rkt

guile:
	guile --fresh-auto-compile -x .sld -L src test/test.7.scm

chibi:
	chibi-scheme -I src -I test test/test.7.scm

sagittarius:
	sagittarius --clean-cache --disable-cache --loadsuffix=.sld --standard=7 --loadpath=src test/test.7.scm

chez:
	scheme --libdirs $$CHEZSCHEMELIBDIRS:$$PWD/src --script test/test.6.scm

gambit:
	gsi -:r7rs src/ test/test.7.scm

clean:
	rm -f *.log test/*.log

format: format-src format-test

format-src:
	@echo "Formatting source files..."
	@for file in src/*.scm src/*.sld src/*.rkt src/*.sls; do \
		if [ -f "$$file" ]; then \
			echo "Formatting $$file"; \
			schemat < "$$file" | sponge "$$file"; \
		fi; \
	done

format-test:
	@echo "Formatting test files..."
	@for file in test/*.scm test/*.rkt; do \
		if [ -f "$$file" ]; then \
			echo "Formatting $$file"; \
			schemat < "$$file" | sponge "$$file"; \
		fi; \
	done
