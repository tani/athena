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
