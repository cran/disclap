ddisclap <-
function(x, p) {
  if (any(p <= 0) | any(p >= 1)) stop("0 < p < 1 is required")
  
  if (length(p) != length(x)) {
    if (length(p) == 1) {
      p <- rep(p, length(x))
    } else {
      stop("length(p) != 1 and length(p) != length(x)")
    }
  }
  
  ret <- ((1-p)/(1+p))*p^abs(x) 
  is <- p == 0
  
  if (length(is) > 0) {
    ret[is] <- ifelse(x[is] == 0, 1, 0)
  }
  
  return(ret)
}

