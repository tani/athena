# Gemini Workspace Configuration

This document provides recommendations for working with the Athena project in the Gemini environment.

## Project Overview

This project appears to be a Prolog implementation written in Scheme, with support for multiple Scheme interpreters.

## Development

### Environment Setup (Nix)

This project uses [Nix Flakes](https://nixos.wiki/wiki/Flakes) to define a reproducible development environment. To activate it, run:

```bash
nix develop
```

This will provide a shell with all the required Scheme implementations and other dependencies installed.


### Running Tests

The `Makefile` provides targets for running tests with different Scheme implementations.

- **Run all tests:**
  ```bash
  make all
  ```

- **Run tests for a specific implementation:**
  - **Gauche:** `make gauche`
  - **Chicken:** `make chicken`
  - **Racket:** `make racket`
  - **Guile:** `make guile`
  - **Chibi:** `make chibi`
  - **Sagittarius:** `make sagittarius`
  - **Chez:** `make chez`
  - **Gambit:** `make gambit`
  - **Biwa:** `make biwa`

### Cleaning up

To remove generated log files, run:
```bash
make clean
```
