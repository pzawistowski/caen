library(proto)

nvl <- function(v1,v2){
  if(is.null(v1)) v2 else v1  
}


encodeColumn <- function(col, mapping){
  if(is.character(col) || is.factor(col)){
    vapply(col, FUN=function(v) nvl(mapping$map[[as.character(v)]], mapping$default), FUN.VALUE=0.0, USE.NAMES = FALSE)
  }else{
    col
  }
}

encodeColumns <- function(atts, mapping){
  col.num <- ncol(atts)
  if(is.null(col.num) || col.num == 1){
    encodeColumn(atts, mapping)  
  }else{    
    sapply(1:col.num, function(i) encodeColumn(atts[,i], mapping[[i]]))
  }
}


lambda <- function(ni, k = 20, f = 4){
  1/(1+exp(-(ni-k)/f))
}

createMappingForColumn <- function(x,y, target.value, lambda.fun){
  if(is.character(x) || is.factor(x)){
    tab <- table(x, y)
    
    col.idx <- which(colnames(tab) == target.value)
    col <- tab[,col.idx]
    target.p <- sum(col)/length(x)
    
    rs <- rowSums(tab)
    l <- lambda.fun(rs)
    
    map <- as.list(l*(col/rs) + (1-l)*target.p)
    names(map) <- rownames(tab)
    
    list(map = map, default = sum(col)/sum(tab))  
  }else{
    list()
  }
}

createMapping <- function(x,y, target.value, lambda.fun){
  if(is.null(ncol(x))){
    createMappingForColumn(x,y, target.value, lambda.fun)
  }else{
    lapply(1:ncol(x), function(i) createMappingForColumn(x[,i], y, target.value, lambda.fun))
  }
}



#' @export
CaEn <- function(x, y, target.value = levels(as.factor(y))[[1]], k = 20, f = 4){
  proto::proto(mapping = createMapping(x,y, target.value, function(v) lambda(v, k, f)),
        encode =  function(this, x){
          encodeColumns(x, this$mapping)   
        }
  )
}
