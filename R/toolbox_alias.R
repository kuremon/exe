#' @title Alias for exeres
#' @name alias_exeres
#' @description As the result of an exe called often include a naming function that computes name with non
#' valid R object like / or - or names starting with a number it is impossible to access autocomplete.
#' To bypass this restriction, exe assigns a class to its result \code{c("exeres","list")}. Structurally
#' an exeres object is absolutely equivalent to its underlying list. However when an element is looked for
#' with handle \code{$}, the aliases are displayed instead of the stored name. The stored names can still be 
#' retrieved with function \code{\link{real.names}}
#' @keywords alias exeres
NULL

# This function takes as argument the a named list and returned the corresponding exeres list
# with aliases (created with create_alias) ready to use
add_alias=function(exeres){
  attr(exeres,"alias") <- create_alias(names(exeres))
  class(exeres)=c("exeres","list")
  exeres
}

# This function takes as input a character vector of names and returns their aliases. 
create_alias=function(x){
  alias=as.list(x)
  names(alias)=paste0("row",gsub("[[:blank:]]","",gsub("-","..",gsub("\\/",".",x))))
  alias
}

#' @S3method names exeres
#' @method names exeres
names.exeres=function(x){
  names(attr(x,"alias"))
}

#' @S3method "[" exeres
#' @method "[" exeres
`[.exeres`<-function(x,i){
  if(is.numeric(i)) return(NextMethod())
  res=NULL
  names.x=real.names(x)
  for(str in i){
     res=c(res,x[grep(paste0("^",str),names.x)])
   }
  res
}

#' @S3method "$" exeres
#' @method "$" exeres
`$.exeres`<-function(x,name){
  alias=attr(x,"alias")
  x[[alias[[name]]]]
}

#' @title Get real names of an exeres
#' @param x an exeres object
#' @return A character vector of the stored names of \code{x} (as opposed to the aliases).
#' @export
real.names=function(x){
  attr(x,"names")
}