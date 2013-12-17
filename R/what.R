#' @title Create an action or parameters table
#' @rdname action_param
#' @return A data frame
#' @export
#' @examples
#' \donttest{
#' A1=.action(c("do.this","do.that"))
#' A11=.action("do.this")
#' A2=.action(list(max,min))
#' A21=.action(max)
#' }
.action=function(values){
  do.call(composite.matrix,list(action=values))
}

#' @rdname action_param
#' @export
#' @examples
#' \donttest{
#' param=.param(scale=list(scale_alpha_continuous(),scale_alpha_manual()),color=c("red","blue"))
#' }
.param=function(...){
  do.call(composite.matrix,list(...))
}

is.basic.template=function(x){
  is.data.frame(x)||is.matrix(x)
}

#' @rdname action_param
#' @export
is.action=function(x){
  is.matrix(x)&&(colnames(x)=="action")
}

#' @rdname action_param
#' @export
is.param=function(x){
  is.matrix(x)&&(!is.action(x))
}