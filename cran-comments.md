# CRAN notes for mapscanner_0.0.4 submission

This is a first submission. There is no accompanying DOI because the study for which the package was developed was field-based, and could not happen due to COVID. We hope to continue the study in the near future, and will include a DOI as soon as practicable. The package may also exceed a 5MB installed size on some systems, due to sizes and numbers of included image files. This is necessary because the package encodes a technique to rectify hand-drawn marks on maps, for which sufficiently intelligible examples are necessary. Every effort has been given to ensuing the included files are as small as possible while maintaining sufficient visual clarity to demonstrate the techniques.

The package has been checked on all environments listed below, and generates only the one note regarding the multiple licenses used here - one for the main package, and another + LICENSE file for internally bundled code.

## Test environments

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R3.6, R4.0, R-devel

CRAN win-builder:
* R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.
