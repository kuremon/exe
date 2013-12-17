#####################
extract.value=function(x){
  apply(x,2,function(y)y[[1]][[2]])
}

#' @title Create naming layout
#' @rdname naming
#' @export
#' @examples
#' \donttest{
#' x=data.frame(.is(Site,"Paris"),.is(Year,2004),.in(Season,list(c("summer","winter"))),.is(Person,list(NULL)))
#' naming.folder=create_naming("Site","Season")
#' naming.folder(x)
#' naming.underscore=create_naming("Site","Season",sep.global="_")
#' naming.underscore(x)
#' }
create_naming=function(...,sep.internal="-"){
  args=as.list(substitute(list(...)))[-1]
  where.name=sapply(args,is.name)
  names.select=sapply(args[where.name],as.character)
  sep=c(NA,args[!where.name])
  
  function(template.list){
    list.names.trow=lapply(template.list,colnames)
    names.idx=as.numeric(match.in(names.select,list.names.trow))
    ok.match=which(!is.na(names.idx))
    names.idx=names.idx[ok.match]
    match.names=names.select[ok.match]
    match.sep=sep[ok.match]
    
    name.list=vector(2*length(names.idx),mode="list")
    for(k in seq(names.idx)){
      name.list[2*k-1]=ifelse(k==1,"",match.sep[k])
      template.list.k=template.list[[names.idx[k]]][,match.names[k],drop=FALSE]
      if(is.condition(template.list.k)){
        name.list.k=extract.value(template.list.k)
      }else{
        name.list.k=format(as.composite(template.list.k))[[1]]
      }
      name.list[2*k]=paste(name.list.k,collapse=sep.internal)
    }
    paste(name.list,collapse="")
  }
}

#' @export
apply_naming=function(naming,template){
  k=1
  res=character(length.template(template))
  
  f=function(template,current.name=NULL){
    
    if(is.null(template)){      # END CONDITION: NULL TEMPLATE -> APPLY ACTION TO DATA
      res[[k]] <<- naming(current.name)
      k<<-k+1
      return()
    }
    
    if(!is.basic.template(template)){ # IF COMPLEX TEMPLATE
      if(template$op=="rbind.fill"){ # IF TEMPLATE IS MADE OF TWO STACKED TEMPLATES, APPLY ONE THEN THE OTHER.
        f(template$x,current.name)
        f(template$y,current.name)
        return()
      }
      L=subdivide(template)
      template=L$leftmost
      branches=L$branches
    }else{
      branches=vector(nrow(template),mode="list")
    }
    
    for(k in seq(length.template(template))){
      temp.k=extract.from.template(template,k)
      current.name.k=c(current.name,list(temp.k))
      f(branches[[k]],current.name.k)
    }
  }
  f(template)
  res
}