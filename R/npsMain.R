##' Main function of the package.
##' @param df Dataframe with columns z,x and y
##' @l Number of bins used to discretize Z
##' @m Number of bins used to discretize X
##' @n Number of bins used to discretize Y
##' @return result object of the test
nps.test = function(df,l,m,n){
  # Discretize values with given dimensions
  dx = unname(unlist(discretize(df$x,nbins=m, disc="equalwidth")))
  dy = unname(unlist(discretize(df$y,nbins=m, disc="equalwidth")))
  dz = unname(unlist(discretize(df$z,nbins=m, disc="equalwidth")))

  ddf = data.frame(z = dz, x = dx, y = dy)

  nt = nps.necessary(ddf);
  if(!nt$passed) return(FALSE)

  ZXY = expand.grid((1:l), (1:m), (1:n))
  Q = apply(ZXY,1,function(row) {
     qi = sum(ddf$z == row[1] & ddf$x == row[2] & ddf$y == row[3])
     return (qi)
  })

  invalid = nps.invalid(Q,l,m,n)
  valid = M_Valid(Q,l,m,n)

  ratio = valid/invalid$max
  result = merge(nt, merge(valid, merge(invalid, ratio)))
  return (result)
}
