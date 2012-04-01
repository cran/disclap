ddisclap <-
function(d, p) {
  if (any(p <= 0) | any(p >= 1)) stop("0 < p < 1 is required")
  
  if (length(p) != length(d)) {
    if (length(p) == 1) {
      p <- rep(p, length(d))
    } else {
      stop("length(p) != 1 and length(p) != length(d)")
    }
  }
  
  ret <- ((1-p)/(1+p))*p^abs(d) 
  is <- p == 0
  
  if (length(is) > 0) {
    ret[is] <- ifelse(d[is] == 0, 1, 0)
  }
  
  return(ret)
}

