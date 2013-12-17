#' @title Operators to create a template
#' @rdname template_operators
#' @export
`%cb%` <- function(x,y){
  if(is.null(y)) return(x)
  if(is.null(x)) return(y)
  
  if(length.template(x)!=length.template(y)) stop("Templates x and y don't have the same number of rows.")
  #if(is.data.frame(x)&&is.data.frame(y)) return(cbind(x,y)) #PROBLEM WITH ACTION
  list(op="cbind",x=x,y=y)
}

#' @rdname template_operators
#' @export
`%rb%` <- function(x,y){
  if(is.null(y)) return(x)
  if(is.null(x)) return(y)
  if(length.template(x)==0) return(y)
  if(length.template(y)==0) return(x)
  list(op="rbind.fill",x=x,y=y)
}

#' @rdname template_operators
#' @export
`%ex%` <- function(x,y){
  if(is.null(y)) return(x)
  if(is.null(x)) return(y)
  list(op="expand.mgrid",x=x,y=y)
}

#' @title View template in full format
#' @export
full.format=function(template){
  if(is.action(template)||is.param(template)) template=as.composite(template)                              
  if(is.basic.template(template)) return(format(template))
  do.call(template$op,list(full.format(template$x),full.format(template$y)))
}

#' @title Get template length
#' @export
length.template=function(template){
  if(is.basic.template(template)) return(nrow(template))
  x=template$x
  y=template$y
  switch(template$op,
       cbind=length.template(x),
       rbind.fill=length.template(x)+length.template(y),
       expand.mgrid=length.template(x)*length.template(y))
}

#' @title Extract part of a template
#' @param template A recursive template
#' @param int a signle integer or an interval of the form \code{c(n,m)}. 
#' @export
extract.from.template=function(template,int){
  if(length(int)==0) return(data.frame(NULL))
  if(is.basic.template(template)) return(template[seq(min(int),max(int)),,drop=FALSE])
  op=template$op
  x=template$x
  y=template$y
  
  if(op=="cbind") return(extract.from.template(x,int)%cb%extract.from.template(y,int))
  if(op=="rbind.fill"){
    l.x=length.template(x)
    if(max(int)<=l.x) return(extract.from.template(x,int))
    if(l.x<min(int)) return(extract.from.template(y,int-l.x))
    return(extract.from.template(x,c(min(int),l.x))%rb%extract.from.template(y,c(1,max(int)-l.x)))
  }
  if(op=="expand.mgrid"){
    l.y=length.template(y)
    n.x.start=(min(int)-1)%/%l.y+1
    n.y.start=(min(int)-1)%%l.y+1
    n.x.end=(max(int)-1)%/%l.y+1
    n.y.end=(max(int)-1)%%l.y+1
    
    if(n.x.start==n.x.end){
      return(extract.from.template(x,n.x.start)%ex%extract.from.template(y,c(n.y.start,n.y.end)))
    }else{
      temp.start=extract.from.template(x,n.x.start)%ex%extract.from.template(y,c(n.y.start,l.y))
      temp.end=extract.from.template(x,n.x.end)%ex%extract.from.template(y,c(1,n.y.end))
      
      if((n.x.end-n.x.start)==1){ 
        return(temp.start%rb%temp.end)
      }else{
        return(temp.start%rb%(extract.from.template(x,c(n.x.start+1,n.x.end-1))%ex%y)%rb%temp.end)
      }
    }
  }
}