library(proto)

nvl <- function(v1,v2){
  if(is.null(v1)) v2 else v1  
}


encodeColumn <- function(atts, map, default){
  vapply(atts, FUN=function(v) nvl(map[[v]], default), FUN.VALUE=0.0, USE.NAMES = FALSE)
}

lambda <- function(ni, k = 20, f = 4){
  1/(1+exp(-(ni-k)/f))
}

createMapping <- function(x,y, target.value, lambda.fun){
  tab <- table(x, y)
  
  col.idx <- which(colnames(tab) == target.value)
  col <- tab[,col.idx]
  target.p <- sum(col)/length(x)
  
  rs <- rowSums(tab)
  l <- lambda.fun(rs)
  
  map <- as.list(l*(col/rs) + (1-l)*target.p)
  names(map) <- rownames(tab)
  
  list(map = map, 
       default = sum(col)/sum(tab)
  )  
}


#' @export
CaEn <- function(x, y, target.value = levels(as.factor(y))[[1]], k = 20, f = 4){
  mapping <- createMapping(x,y, target.value, function(v) lambda(v, k, f))
  
  proto(map = mapping$map, 
        default = mapping$default,
        encode =  function(this, x){
          encodeColumn(x, this$map, this$default)   
        }
  )
}
