\name{indexesMerge}
\alias{intersectIndexesList}
\alias{unionIndexesList}
\title{
Intersection / union of list of indexes
}
\description{
Functions to perform intersection or union of integer vectors.
This functions are used by DFI.subset for AND/OR filters
}
\usage{
intersectIndexesList(lst)
unionIndexesList(lst)
}
\arguments{
  \item{lst}{list of integer vectors on which intersection or union must be performed}
}
\details{
The returned vector is sorted ascending.
\code{intersectIndexesList} is implemented in C++ and corresponds to \code{sort(unique(Reduce(f=intersect,x=lst)))}.
\code{unionIndexesList} is implemented as \code{sort(unique(Reduce(f=union,x=lst)))}.
}
\value{
A vector of integers.
}
\examples{
intersectIndexesList(list(1:7,4:8,3:5))
unionIndexesList(list(1:7,4:8,3:5))
}

