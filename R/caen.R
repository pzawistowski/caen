nvl <- function(v1,v2){
  if(is.null(v1)) v2 else v1  
}


encodeColumn <- function(atts, map){
  
  vapply(atts, FUN=function(v) nvl(map[[v]], map$default), FUN.VALUE=0.0, USE.NAMES = FALSE)
}

calculateMap <- function(atts, target){
  tab <- table(atts, target)
  
  map <- as.list((tab/rowSums(tab))[,1])
  names(map) <- rownames(tab)
  
  map$default <- sum(tab[,1])/sum(colSums(tab))
  
  map
}

#' @export
encode <- function(atts, target = NULL, map = NULL, map.return = FALSE){
  if(is.null(map)){
    map <- calculateMap(atts, target)
  }
  
  encoded = encodeColumn(atts, map) 
  if(map.return){
    list( encoded = encoded, map = map)
  }else{
    encoded
  }
}