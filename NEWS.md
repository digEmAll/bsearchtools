### Changes in v0.0.47

#### BREAKING CHANGES
  1. Standard `data.frame`/`matrix` subset and replacement operators do not work anymore on `DFI` objects but you need perform them on `DFI.unWrap(DFIobj)`.
            This decision has been made to avoid wrong unnoticed results when `DFI` objects where modified. Also, the internal representation has changed and it
            may be subjected to further changes in the future, so the users should not rely on the current structure but only use the provided functions. (#5)

#### BUG FIXES
  1. Fixed: `print.DFI` not working properly when `DFI` wraps a `matrix` (#1)
  2. Fixed: `DFI.subset` ignoring `return.indexes=FALSE` when `filter=NULL` (#2)
  
#### NEW FEATURES
  1. Added drop argument support in `DFI.subset` function. (#3)


### v0.0.41 released to CRAN on 2016-11-06
