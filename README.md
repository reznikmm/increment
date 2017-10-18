# Incremental analysis in Ada

This package provides incremental analysis algorithms
and related data structures.

We try to (re-)implement ideas described by Tim A. Wagner his work
"Practical Algorithms for Incremental Software Development Environments"

For each analysed document we keep a persistent history of its parsing
tree changes as a sequence (actually tree) of versions. Each such version
represents consistent state. Each node of parsing tree provides a flag
to report if nested nodes were changed in given version of the document.
This allows us to quickly locate the changed subtrees.
