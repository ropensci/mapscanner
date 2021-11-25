# CRAN notes for mapscanner_0.0.6 submission

This submission fixes the Solaris failure on previous submission, along with failing example and test on previous submission attempt. These failures are due to dependencies in external packages which rely on external system libraries. Those packages themselves skip tests of the functions used on Solaris machines, which is what this submission now also does. The submission was checked on Solaris via r-hub with no errors or notes.

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
