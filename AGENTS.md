# Athena AGENTS Instructions

This repository implements the Athena logic programming engine in Scheme.

## Development

1. Enter the Nix development environment:
   ```bash
   nix develop
   ```
2. Run tests for a specific implementation:
   ```bash
   make IMPLS=gauche
   ```
   or run the full matrix with:
   ```bash
   make all
   ```

## Guidelines for Contributors

- Ensure all tests pass before committing.
- Use clear commit messages describing your changes.
- No specific formatting command is required.
- If you run into a parentheses mismatch in Scheme/LISP code, the script
  `script/paren_visualize.awk` can help visualize the balance. Pipe the
  problematic code through it to get an indented view that highlights
  unbalanced parentheses.
