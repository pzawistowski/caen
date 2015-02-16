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

#' @export
CaEn <- function(atts, target, k = 20, f = 4){
  tab <- table(atts, target)
  col <- tab[,1]
  p <- sum(col)/length(atts)
    
  map <- as.list(lambda(col, k, f)*(col/rowSums(tab)) + (1-lambda(col, k, f))*p)
  names(map) <- rownames(tab)
  
  default <- sum(tab[,1])/sum(colSums(tab))
  map <- map
  
  proto(map = map, 
        default = default,
        encode =  function(this, atts){
          encodeColumn(atts, this$map, this$default)   
        }
  )
}
