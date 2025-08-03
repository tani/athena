#!/usr/bin/env bash
# run-fiveam-tests.sh — Script to run FiveAM tests in proper environment

set -e

echo "Athena Prolog Engine - FiveAM Test Runner"
echo "========================================"

# Check if we're in a Nix environment
if command -v nix >/dev/null 2>&1 && [ -f flake.nix ]; then
    echo "Running in Nix development environment..."
    
    # Check if FiveAM is available
    if nix develop --command sbcl --eval "(find-package :fiveam)" --quit 2>/dev/null; then
        echo "FiveAM is available, running tests..."
        nix develop --command sbcl --script test-fiveam-devshell.lisp
    else
        echo "FiveAM not found in environment, trying alternative..."
        nix develop --command sbcl --eval "(require :asdf)" --eval "(asdf:load-system :fiveam)" --eval "(format t \"FiveAM loaded successfully~%\")" --quit
        if [ $? -eq 0 ]; then
            echo "FiveAM loaded via ASDF, running tests..."
            nix develop --command sbcl --script test-fiveam-devshell.lisp
        else
            echo "Unable to load FiveAM, falling back to simple test..."
            nix develop --command sbcl --script test-asdf-simple.lisp
        fi
    fi
else
    echo "Nix not available or not in project directory."
    echo "Attempting to run tests with system SBCL..."
    
    # Try with system SBCL
    if command -v sbcl >/dev/null 2>&1; then
        echo "Found system SBCL, trying to load FiveAM..."
        if sbcl --eval "(require :asdf)" --eval "(asdf:load-system :fiveam)" --eval "(format t \"FiveAM available~%\")" --quit 2>/dev/null; then
            echo "Running FiveAM tests with system SBCL..."
            sbcl --script test-fiveam-devshell.lisp
        else
            echo "FiveAM not available, running basic ASDF tests..."
            sbcl --script test-asdf-simple.lisp
        fi
    else
        echo "Error: No Common Lisp implementation found."
        echo "Please install SBCL or use the Nix development environment:"
        echo "  nix develop"
        echo "  ./run-fiveam-tests.sh"
        exit 1
    fi
fi

echo ""
echo "Test execution completed."