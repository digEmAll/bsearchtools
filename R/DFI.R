
DFI <- function(DF,indexes.col.names=colnames(DF)){

  if(!is.data.frame(DF) && !is.matrix(DF))
    stop('DF must be a data.frame or a matrix')
  getColVector <- function(dd,nm) dd[[nm]]
  if(is.matrix(DF))
    getColVector <- function(dd,nm) dd[,nm]
  
  indexes <- list()
  if(!is.null(indexes.col.names)){
    for(name in indexes.col.names){
      col <- getColVector(DF,name)
      if(!is.atomic(col) || is.factor(col) || !(typeof(col) %in% c('integer','double','numeric','character','logical')) )
        stop(paste0('Invalid index "',name,'": Indexes can only be atomic vectors of types : integer,double,numeric,character,logical'))
      idxs <- order(col,na.last=NA)
      indexes[[name]] <- list(idxs=idxs,sorted=col[idxs])
    }
  }
  DFIobj <- structure(list(x=DF,idx=indexes),class='DFI')
  return(DFIobj)  
}

# the following 3 functions are the only one used to interrogate the wrapper
.getData <- function(DFIobj){
  #return(DFIobj$x)
  return(DFIobj[[1]])
}
.getIndexes <- function(DFIobj){
  #return(DFIobj$idx)
  return(DFIobj[[2]])
}

as.DFI <- function(DF,indexes.col.names=colnames(DF)){
  DFI(DF,indexes.col.names)
}

is.DFI <- function(x){
  inherits(x,'DFI')
}

print.DFI <- function(x,...){
  if(!is.DFI(x)){
    print(x,...)
  }else{
    print(.getData(x),...)
    cat(paste0('\nINDEXES:\n',paste0(DFI.indexes(x),collapse='\n')))
  }
}

as.matrix.DFI <- function(x,...){
  as.matrix(DFI.unWrap(x),...)
}

as.data.frame.DFI <- function(x,...){
  as.data.frame(DFI.unWrap(x),...)
}

DFI.getIndex <- function(DFIobj,name){
  return(.getIndexes(DFIobj)[[name]])
}

DFI.indexes <- function(DFIobj){
  return(names(.getIndexes(DFIobj)))
}

DFI.unWrap <- function(DFIobj){
  if(!is.DFI(DFIobj)){
    return(DFIobj);
  }
  return(.getData(DFIobj))
}

DFI.subset <- function(DFIobj, filter=NULL, return.indexes=FALSE, sort.indexes=TRUE, colFilter=NULL, drop=NULL){
  if(!is.DFI(DFIobj))
    stop('DFIobj is not of class DFI')
  if(is.null(filter)){
    idxs <- 1:nrow(.getData(DFIobj))
  }else{
    idxs <- .filterRecursive(DFIobj, filter)
    if(sort.indexes)
      idxs <- sort.int(idxs)
  }
  if(return.indexes)
    return(idxs)
  if(!is.null(drop)){
    if(!is.null(colFilter))
      return(.getData(DFIobj)[idxs,colFilter,drop=drop])
    return(.getData(DFIobj)[idxs,,drop=drop])
  }else{
    if(!is.null(colFilter))
      return(.getData(DFIobj)[idxs,colFilter])
    return(.getData(DFIobj)[idxs,])
  }
}

RG <- function(col,from,to){
  `class<-`(list(col=col,from=from,to=to),c('DFI.RG','DFI.FEXPR'))
}
IN <- function(col,values){
  do.call(OR,lapply(values,function(v)EQ(col,v)))
}
EQ <- function(col,val){
  `class<-`(list(col=col,val=val),c('DFI.EQ','DFI.FEXPR'))
}
NOT <- function(filter){
  `class<-`(list(filter=filter),c('DFI.NOT','DFI.FEXPR'))
}
OR <- function(...){
  orExprs <- list(...) 
  stopifnot(length(orExprs) > 0)
  `class<-`(orExprs,c('DFI.OR','DFI.FEXPR'))
}
AND <- function(...){
  andExprs <- list(...)
  stopifnot(length(andExprs) > 0)
  `class<-`(andExprs,c('DFI.AND','DFI.FEXPR'))
}

toString.DFI.FEXPR <- function(x,...){
  
  escapeIfChar <- function(v){
    if(is.character(v))
      return(paste("\"",v,"\"",sep=""))
    return(v)
  }
  parenthesizeIfNecessary <- function(z){
    if(z != ""){
      return(paste("(",z,")",sep=""))
    }
    return(z)
  }
  
  
  specific.class <- class(x)[1]
  
  if(specific.class == "DFI.RG"){
    
    tmp <- paste("!is.na(",x$col,")",sep="")
    
    gt <- ""
    if(!is.na(x$from)){
      gt <- paste(x$col,">=",escapeIfChar(x$from), sep=" ")
    }
    lt <- ""
    if(!is.na(x$to)){
      lt <- paste(x$col,"<=",escapeIfChar(x$to), sep=" ")
    }
    return(paste(c(tmp, gt,lt)[c(tmp, gt,lt) != ""],collapse=" & "))
    
  }else if(specific.class == "DFI.EQ"){
   
    tmp <- paste("!is.na(",x$col,")",sep="")
    s <- ""
    if(!is.na(x$val)){
      s <- paste(x$col,"==",escapeIfChar(x$val),sep=" ")
    }
    
    return(paste(c(tmp, s)[c(tmp, s) != ""],collapse=" & "))
    
  }else if(specific.class == "DFI.NOT"){
    
    inner <- toString.DFI.FEXPR(x$filter)
    if(inner != ""){
      return(paste("!(",inner,")",sep=""))
    }
    return("")
    
  }else if(specific.class == "DFI.AND"){
    
    inner <- sapply(x,function(xx){ parenthesizeIfNecessary(toString.DFI.FEXPR(xx)) })
    s <- paste(inner[inner != ""],collapse=" & ")
    return(s)
    
  }else if(specific.class == "DFI.OR"){
    
    inner <- sapply(x,function(xx){ parenthesizeIfNecessary(toString.DFI.FEXPR(xx)) })
    s <- paste(inner[inner != ""],collapse=" | ")
    return(s)
  }
  stop("unexpected filter expression")
}

print.DFI.FEXPR <- function(x,...){
  print(toString.DFI.FEXPR(x,...))
}

.eval.EQ <- function(DFIobj, expr){
  tmpDF <- DFI.getIndex(DFIobj, expr[[1]])
  return(indexesEqualTo(tmpDF[[2]],expr[[2]],tmpDF[[1]]))
}

.eval.RG <- function(DFIobj, expr){
  tmpDF <- DFI.getIndex(DFIobj, expr[[1]])
  return(indexesInRange(tmpDF[[2]],expr[[2]],expr[[3]],tmpDF[[1]]))
}

# equivalent (but faster) alternative to sort(unique(Reduce(f=intersect,x=lst)))
intersectIndexesList <- function(lst){
  L <- length(lst)
  if(L == 0)
    return(integer())
  if(L == 1)
    return(sort.int(unique(lst[[1]])))
  # .intersectInteger already sorts the results
  Reduce(f=.intersectInteger,x=lst)
}

# sort(Reduce(f=union,x=lst))
unionIndexesList <- function(lst){
  sort.int(Reduce(f=union,x=lst))
}

.filterRecursive <- function(DFIobj, expr){
  switch (class(expr)[1],
          'DFI.RG' = .eval.RG(DFIobj,expr),
          'DFI.EQ' = .eval.EQ(DFIobj,expr),
          'DFI.NOT' = setdiff(1:nrow(.getData(DFIobj)),.filterRecursive(DFIobj,expr[[1]])),
          'DFI.AND' = intersectIndexesList(lapply(expr,function(x).filterRecursive(DFIobj,x))),
          'DFI.OR' = unionIndexesList(lapply(expr,function(x).filterRecursive(DFIobj,x))),
          stop('unsupported expression, please use RG,IN,EQ,OR,AND functions to create it')
  )
}


  
