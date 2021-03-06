### Changes in v0.0.61 (2017-02-22)

#### BUG FIXES
  1. Fixed: R `v3.4+` requires native symbols registration (#9)

### Changes in v0.0.59 (2017-01-22)

#### BUG FIXES
  1. Fixed: `print.DFI.FEXPR` not actually printing but just returning the string (#6)
  2. Fixed: `NOT` condition not working anymore in version 0.0.47 (#7)
  
#### NEW FEATURES
  1. Added `EQNA` condition to select NA values in DFI objects. (#8)
  2. Improved `unionIndexesList`,`OR`,`IN` performances


### Changes in v0.0.47 (2016-12-12)

#### BREAKING CHANGES
  1. Standard `data.frame`/`matrix` subset and replacement operators do not work anymore on `DFI` objects but you need perform them on `DFI.unWrap(DFIobj)`.
            This decision has been made to avoid wrong unnoticed results when `DFI` objects are modified. Also, the internal representation has changed and it
            may be subjected to further changes in the future, so the users should not rely on the current structure but only use the provided functions. (#5)

#### BUG FIXES
  1. Fixed: `print.DFI` not working properly when `DFI` wraps a `matrix` (#1)
  2. Fixed: `DFI.subset` ignoring `return.indexes=FALSE` when `filter=NULL` (#2)
  
#### NEW FEATURES
  1. Added drop argument support in `DFI.subset` function. (#3)


### v0.0.41 released to CRAN on 2016-11-06
