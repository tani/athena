.PHONY: all test-all test-racket test-gauche test-chicken test-guile test-chibi test-sagittarius test-chez test-gambit test-sbcl test-ecl test-clisp test-abcl clean format

# Default target - run all tests
all: test-all

# Run tests for all implementations
test-all: test-racket test-gauche test-guile test-chicken test-chibi test-sagittarius test-gambit test-chez test-sbcl test-ecl test-clisp test-abcl

# Test specific implementations
test-racket:
	racket scheme/test/test.rkt

test-gauche:
	gosh -r 7 -I scheme/src scheme/test/test.7.scm

test-chicken:
	csi -require-extension r7rs -include-path scheme/src -eval '(include "scheme/src/prolog.sld")' -script scheme/test/test.7.scm

test-guile:
	guile --fresh-auto-compile -x .sld -L scheme/src scheme/test/test.7.scm

test-chibi:
	chibi-scheme -I scheme/src -I scheme/test scheme/test/test.7.scm

test-sagittarius:
	sagittarius --clean-cache --disable-cache --loadsuffix=.sld --standard=7 --loadpath=scheme/src scheme/test/test.7.scm

test-chez:
	scheme --libdirs $$CHEZSCHEMELIBDIRS:$$PWD/scheme/src --script scheme/test/test.6.scm

test-gambit:
	gsi -:r7rs scheme/src/ scheme/test/test.7.scm

# Test Common Lisp implementations
test-sbcl:
	sbcl --eval "(require 'asdf)" --eval "(push #P\"./common-lisp/\" asdf:*central-registry*)" --eval "(asdf:test-system :prolog)" --quit

test-ecl:
	ecl --eval "(require 'asdf)" --eval "(push #P\"./common-lisp/\" asdf:*central-registry*)" --eval "(asdf:test-system :prolog)" --eval "(quit)"

test-clisp:
	clisp -x "(require :asdf)" -x "(push #P\"./common-lisp/\" asdf:*central-registry*)" -x "(asdf:test-system :prolog)" -x "(quit)"

test-abcl:
	abcl --eval "(require 'asdf)" --eval "(push #P\"./common-lisp/\" asdf:*central-registry*)" --eval "(asdf:test-system :prolog)" --eval "(quit)"

# Clean build artifacts
clean:
	rm -f *.log scheme/test/*.log common-lisp/test/*.log

# Format all source files
format:
	find scheme common-lisp -type f \( -name '*.scm' -o -name '*.sld' -o -name '*.rkt' -o -name '*.sls' -o -name '*.lisp' \) -exec sh -c 'echo "Formatting {}"; schemat < "{}" | sponge "{}"' \;
