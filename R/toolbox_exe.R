#' @export
subdivide=function(template){
  f=function(temp){
    
    if(is.basic.template(temp)){
      n=nrow(temp)
      return(list(leftmost=temp,branches=vector(n,mode="list"),tags=as.list(seq(n))))
    }
    
    op=temp$op
    x=temp$x
    y=temp$y
    
    res.x=f(x)
    
    leftmost.x=res.x$leftmost
    branches.x=res.x$branches
    tags.x=res.x$tags
    
    if(op=="rbind.fill"){
     res.y=f(y)
     leftmost.y=res.y$leftmost
     branches.y=res.y$branches
     tags.y=res.y$tags
     return(list(leftmost=leftmost.x%rb%leftmost.y,branches=c(branches.x,branches.y),tags=c(tags.x,tags.y)))
    }
    
    if(op=="cbind"){
      for(k in seq(branches.x)){
        branches.x[[k]]=branches.x[[k]]%cb%extract.from.template(y,range(tags.x[[k]]))
      }
    }
    
    if(op=="expand.mgrid"){
      l.y=length.template(y)
      for(k in seq(branches.x)){
        branches.x[[k]]=branches.x[[k]]%ex%y
        tags.x[[k]]=(tags.x[[k]]-1)*l.y
      }
    }
    
    list(leftmost=leftmost.x,branches=branches.x,tags=tags.x)
  }
  f(template)[c("leftmost","branches")]
}


#' @export
exe=function(template,data,naming){
  #k=1
  res=setNames(vector(length.template(template),mode="list"),apply_naming(naming,template))
  res=add_alias(res)
  
  f=function(template,data,current.name=NULL,action=NULL,param=NULL){
    
    if(is.null(template)){      # END CONDITION: NULL TEMPLATE -> APPLY ACTION TO DATA
      res[[naming(current.name)]] <<- do.call(action,c(list(data),param))
      #k<<-k+1
      return()
    }
    
    if(!is.basic.template(template)){ # IF COMPLEX TEMPLATE
      if(template$op=="rbind.fill"){ # IF TEMPLATE IS MADE OF TWO STACKED TEMPLATES, APPLY ONE THEN THE OTHER.
        f(template$x,data,current.name,action,param)
        f(template$y,data,current.name,action,param)
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
      if(is.condition(temp.k)){
        f(branches[[k]],subset2(data,temp.k),current.name.k,action,param)
      }else{
        temp.k.drop=as.list(drop(temp.k))
        if(is.action(temp.k)) f(branches[[k]],data,current.name.k,temp.k.drop$action,param)
        if(is.param(temp.k)) f(branches[[k]],data,current.name.k,action,c(param,temp.k.drop)) 
      }
    }
  }
  f(template,data)
  res
}