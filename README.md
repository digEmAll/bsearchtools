# bsearchtools
## Binary search tools for R.

The `bsearchtools` package exposes the binary search based functions of the C++ standar library 
(`std::lower_bound`, `std::upper_bound`) plus other convenience functions, allowing faster lookups on sorted vectors.

It also includes a lightweight**(*)** `data.frame` wrapper (`DFI`), which automatically creates indexes on the 
columns for faster lookups.

These functions are especially designed to be used in non-vectorized operations (e.g. inside loops).  
For vectorized operations the great `data.table` package already fullfills basically every R programmer needs.

__(*)__   
_lightweight_ in the sense that is just a `data.frame`, with `class(x) = c('DFI','data.frame')` 
plus an attribute containing the necessary indexes data. So it still can be used as a plain normal `data.frame`, 
since the base functions have not been overridden (except for `print`, which prepend the list of indexed columns).

### Examples : 

- Get lower and upper bound indexes :
```r
sortedVec <- c(1,3,3,5,7,12,15,42)

lb(sortedVec,3) # returns 2
ub(sortedVec,3) # returns 4
```

- Get indexes of elements equal to a value / in a [from,to] range :
```r
sortedVec <- c(1,3,3,5,7,12,15,42)

indexesEqualTo(sortedVec,3) # returns c(2,3)
indexesInRange(sortedVec,5,15) # returns c(4,5,6,7)

```

- DFI data.frame wrapper
```r
DF <- data.frame(Name=c('John','Jennifer','John','Emily','Peter','Anna','Emily'), 
                 Age=c(30,50,15,27,25,35,70),
                 stringsAsFactors = FALSE)

# create a DFI object from a data.frame (you can also use as.DFI)
DFIobj <- DFI(DF)

# select rows with this filter : 
# (Name == 'John' | Name == 'Emily') & Age >= 25 & Age <= 60
res <- DFI.subset(DFIobj, AND(OR(EQ('Name','John'),EQ('Name','Emily')),RG('Age',25,60)))
# returns :
# > res
#    Name Age
# 1  John  30
# 4 Emily  27

```

### License

GPL (>= 2)



