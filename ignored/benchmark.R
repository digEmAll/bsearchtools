library(bsearchtools)

####################################################################################
#                             BENCHMARK FUNCTION                                   #
####################################################################################
measure <- function(...,replications=100){
  exprs <- as.list(match.call(expand.dots = FALSE)[['...']])
  exprNames <- if(is.null(names(exprs))) rep.int("",length(exprs)) else names(exprs)
  #print(exprs)
  
  for(i in which(exprNames == "")){
    exprNames[i] <- paste(deparse(exprs[[i]]),collapse=" ")
  }
  
  result <- data.frame(Expr=exprNames,Total=NA,AvgSingle=NA)
  for(i in 1:length(exprs)){
    e <- exprs[[i]]
    res <- system.time(
      {
        count <- 1
        while(count<=replications){
          v <- eval(e,envir=parent.frame())
          count <- count + 1
        }
      })
    result$Total[i] <- res['elapsed']
    result$AvgSingle[i] <- result$Total[i] / replications
  }
  
  minIdx <- which.min(result$Total)
  minVal <- result$Total[minIdx]
  result$Relative <- result$Total / minVal
  result <- result[order(result$Relative),]
  
  if(minVal == 0)
    warning(paste('expression: ',exprNames[minIdx],'took 0 seconds please increase the number of replications'))

  return(result)
}



####################################################################################
#                               CREATE SAMPLE OBJECT                               #
####################################################################################
set.seed(123) # for reproducibility
DF <- data.frame(Letters=sample(LETTERS,1e6,replace=TRUE),
                 Values=sample(1:1e4,1e6,replace=TRUE),
                 Values2=sample(1:1e4,1e6,replace=TRUE),
                 stringsAsFactors = FALSE)
# we want to index only 'Values' column, by default all columns are indexed
DFIobj <- DFI(DF,indexes.col.names = c('Letters','Values','Values2'))


####################################################################################
#                                  RG BENCHMARK                                    #
####################################################################################
RGbench <-
measure(
  NormalSubset=DF[DF$Values >= 4500 & DF$Values <= 5000, 'Letters' ],
  NormalIndexes=which(DF$Values >= 4500 & DF$Values <= 5000),
  DFISubset=DFI.subset(DFIobj,filter=RG('Values',4500,5000),colFilter='Letters'),
  DFISubsetUnordered=DFI.subset(DFIobj,filter=RG('Values',4500,5000),colFilter='Letters',sort.indexes = FALSE),
  DFISubsetOnlyIndexes=DFI.subset(DFIobj,filter=RG('Values',4500,5000),colFilter='Letters', return.indexes = TRUE, sort.indexes = TRUE),
  DFISubsetOnlyIndexesUnordered=DFI.subset(DFIobj,filter=RG('Values',4500,5000),colFilter='Letters', return.indexes = TRUE, sort.indexes = FALSE),
  replications = 4000
)

# > RGbench
#                            Expr Total AvgSingle  Relative
# 6 DFISubsetOnlyIndexesUnordered  0.24 0.0000600   1.00000
# 4            DFISubsetUnordered  2.78 0.0006950  11.58333
# 5          DFISubsetOnlyIndexes  5.39 0.0013475  22.45833
# 3                     DFISubset  8.61 0.0021525  35.87500
# 2                 NormalIndexes 85.04 0.0212600 354.33333
# 1                  NormalSubset 89.60 0.0224000 373.33333


####################################################################################
#                                 AND BENCHMARK                                    #
####################################################################################
ANDbench <-
measure(
  NormalSubset=DF[(DF$Values >= 4500 & DF$Values <= 5000) & (DF$Values2 >= 4500 & DF$Values2 <= 5000), 'Letters' ],
  NormalIndexes=which((DF$Values >= 4500 & DF$Values <= 5000) & (DF$Values2 >= 4500 & DF$Values2 <= 5000)),
  DFISubset=DFI.subset(DFIobj,filter=AND(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters'),
  DFISubsetUnordered=DFI.subset(DFIobj,filter=AND(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters',sort.indexes = FALSE),
  DFISubsetOnlyIndexes=DFI.subset(DFIobj,filter=AND(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters', return.indexes = TRUE, sort.indexes = TRUE),
  DFISubsetOnlyIndexesUnordered=DFI.subset(DFIobj,filter=AND(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters', return.indexes = TRUE, sort.indexes = FALSE),
  replications = 1000
)

# > ANDbench
#                            Expr Total AvgSingle  Relative
# 6 DFISubsetOnlyIndexesUnordered  1.78   0.00178  1.000000
# 5          DFISubsetOnlyIndexes  1.81   0.00181  1.016854
# 4            DFISubsetUnordered  1.85   0.00185  1.039326
# 3                     DFISubset  1.89   0.00189  1.061798
# 2                 NormalIndexes 32.06   0.03206 18.011236
# 1                  NormalSubset 32.18   0.03218 18.078652


####################################################################################
#                                  OR BENCHMARK                                    #
####################################################################################
ORbench <-
measure(
  NormalSubset=DF[(DF$Values >= 4500 & DF$Values <= 5000) | (DF$Values2 >= 4500 & DF$Values2 <= 5000), 'Letters' ],
  NormalIndexes=which((DF$Values >= 4500 & DF$Values <= 5000) | (DF$Values2 >= 4500 & DF$Values2 <= 5000)),
  DFISubset=DFI.subset(DFIobj,filter=OR(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters'),
  DFISubsetUnordered=DFI.subset(DFIobj,filter=OR(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters',sort.indexes = FALSE),
  DFISubsetOnlyIndexes=DFI.subset(DFIobj,filter=OR(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters', return.indexes = TRUE, sort.indexes = TRUE),
  DFISubsetOnlyIndexesUnordered=DFI.subset(DFIobj,filter=OR(RG('Values',4500,5000),RG('Values2',4500,5000)),colFilter='Letters', return.indexes = TRUE, sort.indexes = FALSE),
  replications = 1000
)

# > ORbench
#                            Expr Total AvgSingle  Relative
# 6 DFISubsetOnlyIndexesUnordered  2.06   0.00206  1.000000
# 4            DFISubsetUnordered  4.33   0.00433  2.101942
# 5          DFISubsetOnlyIndexes  4.86   0.00486  2.359223
# 3                     DFISubset  5.92   0.00592  2.873786
# 2                 NormalIndexes 36.97   0.03697 17.946602
# 1                  NormalSubset 37.92   0.03792 18.407767

