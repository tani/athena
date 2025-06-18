# Athena AGENTS Instructions

This repository implements the Athena logic programming engine in Scheme.

## Development

1. Run any command within the Nix development environment using the `nix develop -c` prefix. For example, to run tests for a specific implementation:
   ```bash
   nix develop -c make IMPLS=gauche
   ```
   To execute the full test matrix:
   ```bash
   nix develop -c make all
   ```

2. You need to finalize editing with the full test matrix.

## Guidelines for Contributors

- Ensure all tests pass before committing.
- Use clear commit messages describing your changes.
- No specific formatting command is required.
- If you run into a parentheses mismatch in Scheme/LISP code, the script
  `script/paren_visualize.awk` can help visualize the balance. Pipe the
  problematic code through it to get an indented view that highlights
  unbalanced parentheses.
