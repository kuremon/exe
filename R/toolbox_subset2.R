# Find index where call is evaluated in envir data.
# Used in subset2i
find.index.i=function(call,data){
  tryCatch(eval(call,data),error = function(e) rep(TRUE,nrow(data)))
}

# Find index where cond is a condition list.
# Used in subset2
find.index=function(cond,data){
  expr=names(cond)
  cond=cond[[1]][[1]]
  operator=cond[[1]]
  values=cond[[2]]
  tryCatch(do.call(operator,list(eval(parse(text=expr),data),values)),
           error = function(e) rep(TRUE,nrow(data)))
}

#' @title Subset non interactively on several arguments
#' @description A function to subset non interactively on several arguments combined by AND using condition data frames/list.
#' @param data the data frame to subset
#' @param cond a condition data frame
#' @details Conditions on non existing columns of \code{data} and on \code{NULL} values are ignored.
#' @return The subset data frame where all the conditions specified in \code{cond} have been combined together with an AND.
#' by a logical.
#' @export
#' @examples
#' \donttest{
#' cond=cbind(.in(Petal.Width,list(c(0.2,0.3))),.is(as.numeric(as.factor(Species)),1))
#' subset2(iris,cond)
#' cond1=cbind(cond,.is(Group,"flower"))
#' subset2(iris,cond1)
#' cond2=cbind(cond,.is(Sepal.Length,list(NULL)))
#' subset2(iris,cond2)
#' }
subset2=function(data,cond){
  for(k in seq(cond)){
    idx=find.index(cond[k],data)
    data=data[idx,]
    if(nrow(data)==0) return(data)
  }
  data
}

#' @title Subset interactively on several arguments
#' @description Subset on several simultaneous conditions.
#' @param data
#' @param ... conditions written as in \code{\link{transform}}
#' @return The subset data frame where all the conditions in \code{...} have been combined by AND.
#' @seealso \code{\link{subset2}}
#' @export
#' @examples
#' \donttest{
#' subset2i(mtcars,cyl%in%c(4,6),am==1,vs==0)
#' subset2i(mtcars,cyl%in%c(4,6),am==1,Group==2)
#' subset2i(mtcars,mpg==2)
#' }
subset2i=function(data,...){
  exprs=as.list(substitute(list(...)))[-1]
  subset2.base(data,exprs)
}

# This functin subset data on a list of call.
# This function is used in subset2i
subset2.base=function(data,call.list){
  for(k in seq(call.list)){
    idx=find.index.i(call.list[[k]],data)
    data=data[idx,]
    if(nrow(data)==0) return(data)
  }
  data
}