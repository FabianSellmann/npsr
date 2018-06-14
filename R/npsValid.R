##' @title nps.valid
##' @param Q Histogram of dataset (l*m*n vector)
##' @param l |Z|
##' @param m |X|
##' @param n |Y|
##' @param N Number of Repetitions for Nested Sampling
##' @param S Number of Starting Points for Nested Sampling
##' @param int_z Precalculated Integer of the product over theta_z
##' @description Calculates M_Valid
nps.valid = function(Q, l, m, n, N = sum(Q), S = sum(Q), int_z){
  Rx = expand.grid(rep(list(1:m),l)) # m^l x l matrix
  Ry = expand.grid(rep(list(1:n),m )) #n^m x m matrix
  Rxy = cbind(Rx[rep(1:nrow(Rx), nrow(Ry)),],
              Ry[rep(1:nrow(Ry), each = nrow(Rx)),]) # (n^m)*(m^l) x l+m matrix
  # Create probability matrix for P(X = xi, Y = yi | zi)
  P = Create_P(l,m,n, Rxy)

  # Log-likelihood function for estimating the integral of theta_Z
  llf_xy = function(t){
    return(XY_product(unlist(t),P,Q))
  }
  Int_xy = estimate_integral(N=N,S=S,d=(m^l)*(n^m),llf=llf_xy, sample_theta = sample_theta)
  return (int_z*Int_xy)
}



