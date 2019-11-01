# dmvnorm
dmvnorm <- function (x, mu, Sigma, log = FALSE, tol = 1e-06) {
  if (is.vector(x)) 
    x = t(as.matrix(x))
  n = length(mu)
  if (is.vector(mu)) {
    p <- length(mu)
    if (is.matrix(x)) {
      mu <- matrix(rep(mu, nrow(x)), ncol = p, byrow = TRUE)
    }
  }
  else {
    p <- ncol(mu)
  }
  if (!all(dim(Sigma) == c(p, p)) || nrow(x) != nrow(mu)) 
    stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE) 
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1]))) 
    stop("Sigma is not positive definite")
  z = t(x - mu)
  logdetS = try(determinant(Sigma, logarithm = TRUE)$modulus)
  attributes(logdetS) <- NULL
  iS = try(solve(Sigma))
  if (class(iS) == "try-error" || class(logdetS) == "try-error") {
    warning("difficulty inverting/taking determinant of Var-Cov matrix")
    return(NA)
  }
  ssq = diag(t(z) %*% iS %*% z)
  loglik = -(n * (log(2*pi)) +  logdetS + ssq)/2
  if (log) loglik else exp(loglik)
}

M = matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1),nrow=3)
dmvnorm(1:3,mu=1:3,Sigma=M,log=TRUE)
dmvnorm(matrix(1:6,nrow=2),mu=1:3,Sigma=M,log=TRUE)
dmvnorm(matrix(1:6,nrow=2),mu=matrix(1:6,nrow=2),Sigma=M,log=TRUE)