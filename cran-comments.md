# CRAN notes for mapscanner_0.0.3 submission

This is a first submission which has been checked on all environments listed
below, and generates only the one note regarding the multiple licenses used
here - one for the main package, and another + LICENSE file for internally
bundled code.

## Test environments

* Linux (via Travis-ci): R-release, R-devel, R-oldrelease
* OSX (via github actions): R-release
* Windows (via github actions): R3.6, R4.0, R-devel
* win-builder: R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.
