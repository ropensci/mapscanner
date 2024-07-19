# CRAN notes for mapscanner_0.1.1 submission

This submission fixes previous warnings from g++ 14.x which arose on CRAN Fedora checks, and also drops previous explicit C++11 specification.

The submission still generates NOTEs on some systems regarding large installed size, which slightly exceeds 5MB (6.4MB at most on apple-darwin17.0). The docs are slightly under half of this, yet are necessary as the package enables digital rectification of hand-drawn marks on maps. Including example maps as image files is therefore an essential part of the documentation. Every effort has been made to ensure numbers and sizes of included files are as small as possible. Any further reduction in file sizes renders images markedly less useful, and would decrease the ability of people to understand how the package is intended to be used.

The package has been checked on all environments listed below, and generates only the one note regarding the multiple licenses used here - one for the main package, and another + LICENSE file for internally bundled code.

## Test environments

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R3.6, R4.0, R-devel

CRAN win-builder:
* R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.
