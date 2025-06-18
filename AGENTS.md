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

## Guidelines for Contributors

- Ensure all tests pass before committing.
- Use clear commit messages describing your changes.
- No specific formatting command is required.
