library(proto)

nvl <- function(v1,v2){
  if(is.null(v1)) v2 else v1  
}


encodeColumn <- function(atts, map, default){
  vapply(atts, FUN=function(v) nvl(map[[v]], default), FUN.VALUE=0.0, USE.NAMES = FALSE)
}

#' @export
CaEn <- function(atts, target){
  tab <- table(atts, target)
  
  map <- as.list((tab/rowSums(tab))[,1])
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
