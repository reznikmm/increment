Incremental analysis in Ada _(increment)_
=========================================

> Incremental analysis library

This package provides incremental analysis algorithms
and related data structures. The main target of the project is construction
of integrated development environment (IDE).

The library perfoms a lexical and syntactical analisys of a program text
and construct a parsing tree. As text changes are introduced in the tree
and subsequent analisys pass restores consistent parsing tree for new text.
Unaffected parts of the tree are kept unchanged.

We try to (re-)implement ideas described by Tim A. Wagner his work
"Practical Algorithms for Incremental Software Development Environments"

## Install

Run
```
make all install PREFIX=/path/to/install
```

### Dependencies
It depends on
* [Matreshka](https://forge.ada-ru.org/matreshka) library.
* [ada-pretty](https://github.com/reznikmm/ada-pretty) -
  an Ada Pretty Printer library.
* [Anagram](https://github.com/reznikmm/anagram) - parser construction library.

### Usage
Add `with "increment";` in your project file.

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/reznikmm/increment/issues/new) or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik

