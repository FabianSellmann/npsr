##' Main function of the package.
##' @param df Dataframe with columns z,x and y
##' @param l Number of bins used to discretize Z
##' @param m Number of bins used to discretize X
##' @param n Number of bins used to discretize Y
##' @param N Number of Repetitions for Nested Sampling
##' @param S Number of Starting Points for Nested Sampling
##' @return result object of the test including the fields: nt, valid, invalid, ratio
##' @export
##' @import stats infotheo MASS gmp classInt
##' @examples
##' nps.test(data.frame(x = runif(3), y = runif(3), z = runif(3)),2,2,2, 3, 3)
nps.test = function(df,l,m,n, N, S){

  # Discretize values with given dimensions
  # dx = unname(unlist(discretize(df$x,nbins=m, disc="equalwidth")))
  # dy = unname(unlist(discretize(df$y,nbins=n, disc="equalwidth")))
  # dz = unname(unlist(discretize(df$z,nbins=l, disc="equalwidth")))

  dx = nps.discretize(df$x, m)
  dy = nps.discretize(df$y, m)
  dz = nps.discretize(df$z, m)

  ddf = data.frame(z = dz, x = dx, y = dy)

  nt = nps.necessary(ddf);
  if(!nt$passed) return(list(passed=FALSE, ratio = 0));

  ZXY = expand.grid((1:l), (1:m), (1:n))
  Q = apply(ZXY,1,function(row) {
     qi = sum(ddf$z == row[1] & ddf$x == row[2] & ddf$y == row[3])
     return (qi)
  })
  if(missing(N)){
    N = sum(Q)
  }
  if(missing(S)){
    S = sum(Q)
  }
  # Log-likelihood function for estimating the integral of theta_Z
  llf_z = function(t){
    return(Z_product(rep(unlist(t),m*n),Q))
  }

  # we need to use it for valid and invalid
  Int_Z = estimate_integral(N=N,S=S,d=l,llf=llf_z, sample_theta = sample_theta)
  invalid = nps.invalid(Q,l,m,n, N, S, int_z = Int_Z)
  valid = list(valid = nps.valid(Q,l,m,n, N, S, int_z = Int_Z))
  ratio = list(ratio = valid$valid/invalid$max)

  result = merge(nt, merge(valid, merge(invalid, ratio)))
  return (result)
}
##' @title nps.discretize
##' @description Discretizes the given \code{variable} into \code{d} bins using Jenks natural breaks optimization
##' @param variables Array of variables to be discretized
##' @param d number of classes (dimensions) to classify the given variables into
##' @return Discretized array
nps.discretize = function(variables,d){
  breaks = classIntervals(variables,d,style="jenks")$brks
  discretized = sapply(variables, function(v){
    bin = 1
    for(b in breaks[2:length(breaks)]){
      if(v <= b){
        return(bin)
      }
      bin = bin +1
    }
  })
  return (discretized)
}
