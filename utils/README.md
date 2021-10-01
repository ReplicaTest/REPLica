# replica utils

This directory contains various files that can ease the integration of replica in your project.
- [`_replica`](./_replica) zsh completion for replica.
  In your `.zshrc``
    1. Add the location of `_replica` to `$fpath`
    2. If not added yet, add:
    ```
    autoload -U compinit
    compinit
    ```
- [`.gitignore`](./.gitignore) provides the baseline to ignore replica internals in your gi project.
- [`Makefile`](./Makefile) provides a target for dhall to json transformation.
