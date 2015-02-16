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
CaEn <- function(atts, target, target.value = levels(as.factor(target))[[1]], k = 20, f = 4){
  tab <- table(atts, target)
  
  col.idx <- which(colnames(tab) == target.value)
  col <- tab[,col.idx]
  p <- sum(col)/length(atts)
    
  rs <- rowSums(tab)
  map <- as.list(lambda(rs, k, f)*(col/rs) + (1-lambda(rs, k, f))*p)
  names(map) <- rownames(tab)
  
  default <- sum(col)/sum(colSums(tab))
  
  proto(map = map, 
        default = default,
        encode =  function(this, atts){
          encodeColumn(atts, this$map, this$default)   
        }
  )
}
