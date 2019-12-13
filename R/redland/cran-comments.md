
## Test environments

* macOS 10.14.5, R 3.6.1
* Ubuntu 19.04, R 3.6.1
* Windows 7, R 3.6.1, R 3.6.1
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.5.3 (2019-03-11)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.6.1 (2019-07-05)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) 2019-09-27 r77229

## Changes since last release

* Resolve deprecated function warnings during package build. (#79)
  Note the the first CRAN submission for this package version 1.10.17-11 contained build warnings regarding
  the deprecated  C library rdflib_node_to_string(). In order to remove this warning, the dependant R 
  function getNextResult() has been removed. The function getNextResult() has been superceded by the
  more efficient getResults().
* Fix memory protection problems reported by 'rchk'. (#78)

## R CMD check results

* There were no ERRORs.
* There were several WARNINGS, all of which originate in the upstream librdf code library which is 
  being wrapped in R, and so are not due to the code in this package.
* There was the following 1 NOTE: 
  - A NOTE regarding "sub-directories of 1Mb or more" for the libs directory.
    For the Windows builds only, the "libs" directory contains only the redland.dll 
    files build for each Windows sub-architecture (i385, x64) and is necessary for 
    the redland package to run.

## Downstream dependencies

* The downstream dependencies (datapack, rdflib) have been checked with devtools::revdep_check(), which passed
  with 0 errors, 0 warnings, 0 notes.
