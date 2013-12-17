#' @title Create wrapper for test
#' @param test the name of the test to wrap
#' @param ... additional arguments of \code{test}
#' @param what the output of the test to keep
#' @return A function that tests with \code{test(...)} and keeps only
#' parameters \code{what} of the output and put them into a data frame.
#' @export
#' @examples
#' \donttest{
#' # Let's say for each we test if there is a difference in miles per gallon consumption between automatic and manual.
#' test_mpg_per_am=test.wrapper(test="t.test",formula=mpg~am,what=c("p.value","statistic"))
#' test_mpg_per_am(mtcars)
#' test_mpg_per_am(subset2(mtcars,.is(vs,1)))
#' }
test.wrapper=function(test,...,what){## ADD DEFAULT FOR WHAT
  test.args=list(...)
  is.what.missing=missing(what)
  
  function(...){
    fun.args=c(list(...),test.args)
    fun.args=rearrange(fun.args,c(formula=1))
    
    res <- tryCatch(do.call(test,fun.args),
                    error = function(e){
                      message(e$message);
                      if(is.what.missing) return(NULL)
                      setNames(replicate(length(what),numeric(0),simplify=F),what)
                    })
    if(is.what.missing){
      what.red=names(res)
    }
    else{
      what.red=intersect(what,names(res))
      if(length(what.red)!=length(what)){
        warning("The following arguments are not results of the test:\n",setdiff(what,what.red))
      }
    }  
    res <- as.data.frame(res[what.red])
    if(!is.null(new.name<-names(what.red))) names(res)<-new.name
    as.list(unrowname(res))
  }
}