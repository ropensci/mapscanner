# CRAN notes for mapscanner_0.0.5 submission

This is a first submission. There is no accompanying DOI because the study for which the package was developed was field-based, and could not happen due to COVID. We hope to continue the study in the near future, and will include a DOI as soon as practicable. Modifications make in response to feedback from the initial submission attempt are:

- Description does not include the phrase "This package ...", or the package name.
- All 'requireNamespace' calls now wrapped in 'if' conditions
- One 'dontrun' removed from one example, leaving two for (1) code which relies on a suggested package; and (2) code which requires an API key, and also calls an additional, potentially unreliable external API.
- All functions now document return values, including from functions called for side-effects only.
- Code in examples which was previously commented out has now been wrapped in `\dontrun{}`.
- One line in vignette which inadvertently wrote file to that directory now writes to `tempdir()`.
- `graphics::par` values now reset via `on.exit`.
- All instances of `par` now properly namespaced as `graphics::par`.

The package has been checked on all environments listed below, and generates only the one note regarding the multiple licenses used here - one for the main package, and another + LICENSE file for internally bundled code.

## Test environments

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R3.6, R4.0, R-devel

CRAN win-builder:
* R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.
