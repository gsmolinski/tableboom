## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.
* "There are ::: calls to the package's namespace in its code." - this is
due to usage of `callr::callr()` which needs namespace calls even if used
on package's internal functions.
