.PHONY: all test-all test-racket test-gauche test-chicken test-guile test-chibi test-sagittarius test-chez test-gambit clean format

# Default target - run all tests
all: test-all

# Run tests for all implementations
test-all: test-racket test-gauche test-guile test-chicken test-chibi test-sagittarius test-gambit test-chez

# Test specific implementations
test-racket:
	racket test/test.rkt

test-gauche:
	gosh -r 7 -I src test/test.7.scm

test-chicken:
	csi -require-extension r7rs -include-path src -eval '(include "src/prolog.sld")' -script test/test.7.scm

test-guile:
	guile --fresh-auto-compile -x .sld -L src test/test.7.scm

test-chibi:
	chibi-scheme -I src -I test test/test.7.scm

test-sagittarius:
	sagittarius --clean-cache --disable-cache --loadsuffix=.sld --standard=7 --loadpath=src test/test.7.scm

test-chez:
	scheme --libdirs $$CHEZSCHEMELIBDIRS:$$PWD/src --script test/test.6.scm

test-gambit:
	gsi -:r7rs src/ test/test.7.scm

# Clean build artifacts
clean:
	rm -f *.log test/*.log

# Format all source files
format:
	find src test -type f \( -name '*.scm' -o -name '*.sld' -o -name '*.rkt' -o -name '*.sls' \) -exec sh -c 'echo "Formatting {}"; schemat < "{}" | sponge "{}"' \;
