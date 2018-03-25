##' Main function of the package.
##' @param df Dataframe with columns z,x and y
nps.test = function(df,l,m,n,alpha){
  # Discretize values with given dimensions
  dx = unname(unlist(discretize(df$x,nbins=m, disc="equalwidth")))
  dy = unname(unlist(discretize(df$y,nbins=m, disc="equalwidth")))
  dz = unname(unlist(discretize(df$z,nbins=m, disc="equalwidth")))

  ddf = data.frame(z = dz, x = dx, y = dy)

  nt = testIC(ddf);
  if(!nt$passed) return(FALSE)

  ZXY = expand.grid((1:l), (1:m), (1:n))
  Q = apply(ZXY,1,function(row) {
     qi = sum(ddf$z == row[1] & ddf$x == row[2] & ddf$y == row[3])
     return (qi)
  })

  invalid = M_Invalid(Q,l,m,n)
  valid = M_Valid(Q,l,m,n)

  ratio = valid/invalid$max
  result = merge(nt, merge(valid, merge(invalid, ratio)))
  return (result)
}
