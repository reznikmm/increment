Incremental analysis in Ada _(increment)_
=========================================

[![Build](https://github.com/reznikmm/increment/workflows/Build/badge.svg)](https://github.com/reznikmm/increment/actions)
[![Download](https://api.bintray.com/packages/reznikmm/matreshka/increment/images/download.svg)](https://bintray.com/reznikmm/matreshka/increment/_latestVersion)
[![reuse compliant](https://img.shields.io/badge/reuse-compliant-green.svg)](https://reuse.software/)

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

## Usage
Add `with "increment";` in your project file.

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/reznikmm/increment/issues/new) or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik

