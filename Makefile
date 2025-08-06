# Clean build artifacts
clean:
	rm -f *.log scheme/test/*.log common-lisp/test/*.log

# Format all source files
format:
	find scheme common-lisp -type f \( -name '*.scm' -o -name '*.sld' -o -name '*.rkt' -o -name '*.sls' -o -name '*.lisp' \) -exec sh -c 'echo "Formatting {}"; schemat < "{}" | sponge "{}"' \;