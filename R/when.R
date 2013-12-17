#' @rdname when
#' @export
is.condition=function(x){
  inherits(x,"condition")
}

when.call=function(expr,operator="==",values){
  res=as.matrix(lapply(values,function(x){
    if(is.null(x)) return(NULL)
    list(operator,x)}))
  res=as.data.frame(res)
  colnames(res)=deparse(expr)
  class(res)=c("data.frame","condition")
  res
}

#' @title when
#' @rdname when
#' @export
#' @examples
#' \donttest{
#' when(as.numeric(Year),"%within%",list(c(1998,2001),c(2002,2003)))
#' }
when=function(expr,operator="==",values){
  when.call(substitute(expr),operator,values)
}

#' @rdname when
#' @export
#' @examples
#' \donttest{
#' .is(Site,c("site1","site2"))
#' .is(Year,2001:2004)
#' }
.is=function(expr,values){when.call(expr=substitute(expr),operator="==",values=values)}

#' @rdname when
#' @export
.in=function(expr,values)when.call(expr=substitute(expr),operator="%in%",values=values)

#' @rdname when
#' @export
.within=function(expr,values)when.call(expr=substitute(expr),operator="%within%",values=values)

#' @rdname when
#' @export
#' @examples
#' \donttest{
#' .is(Site,c("site1","site2"))
#' .is(Year,2001:2004)
#' }
pair.of <- function(expr,values,operator="%in%"){
  res=combn(values,2,simplify=FALSE)
  when.call(expr=substitute(expr),operator=operator,values=res)
}

#' @rdname when
#' @export
symmetric.pair.of <- function(expr,values,operator="%in%"){
  res=unlist(combn(values,2,function(x)list(x,rev(x)),simplify=FALSE),recursive=FALSE)
  when.call(expr=substitute(expr),operator=operator,values=res)
}