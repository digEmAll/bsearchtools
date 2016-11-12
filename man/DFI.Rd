\name{DFI}
\alias{DFI}
\alias{as.DFI}
\alias{is.DFI}
\alias{print.DFI}
\title{Create a data.frame (or matrix) with indexes}
\description{
Turn a data.frame (or matrix) object into a DFI object allowing faster lookups on indexed columns (indexed column to be intended as DB indexes).
}
\usage{
DFI(DF, indexes.col.names=colnames(DF))
as.DFI(DF, indexes.col.names=colnames(DF)) # exactly the same as DFI()
is.DFI(x)
# S3 methods for class 'DFI'
\method{print}{DFI}(x, \dots)
}
\arguments{
  \item{DF}{A data.frame or matrix object (must have column names defined). }
  \item{indexes.col.names}{The column names for which we want to create the indexes. Only integer,numeric,logical and character are supported, so be careful since data.frame by default turns strings into factors (see data.frame \code{stringsAsFactors} argument) }
  \item{x}{A DFI object.}
  \item{\dots}{optional arguments passed to inner \code{print.data.frame} method .}
}
\details{

Basically, \code{DFI()} function creates a copy of \code{DF}, changes its class attribute to \code{c("DFI","data.frame")} and adds an attribute called "indexes" which is a list containing, for each column to be indexed, a list with two elements : the ordered indexes and the sorted column values. 
These attribues will be used to perform faster lookups. 
}
\note{
Indexes are NOT recreated/updated automatically; 
if you change some value in one or more indexed columns (e.g. using \code{`[<-`} operator, the indexed won't be valid anymore and \code{DFI.subset()} will give wrong results.
}
\value{
	An object with class \code{c("DFI","data.frame")}
}
\seealso{
\code{\link[bsearchtools]{DFI}},\code{\link[bsearchtools]{DFI.subset}}
}
\examples{

DF <- data.frame(Foo=c(3,5,7,1,5,8,7,10),
                 Bar=c("A","B","B","C","B","B","C","A"),
                 Baz=c(TRUE,FALSE),
                 stringsAsFactors=FALSE)
DFIobj <- DFI(DF, c("Foo","Bar")) # create a DFI from DF with indexes on "Foo" and "Bar" columns

}

