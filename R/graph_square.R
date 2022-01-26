graph_square <- function(n, nbPaths)
{
  x <-  runif(n-2)
  y <-  runif(n-2)
  x <- c(0,x,1)
  y <- c(0,y,1)
  edge <- matrix(0, (n-1)*nbPaths, 3)
  colnames(edge) <- c("v1","v2", "distance")

  for(i in 1:nbPaths)
  {
    pos <- ((i-1)*(n-1) +1):(i*(n-1))
    U <- sample(2:(n-1))
    S <- c(1,U)
    E <- c(U,n)
    edge[pos, 1] <- S
    edge[pos, 2] <- E
    edge[pos, 3] <- sqrt((x[S] - x[E])^2 +  (y[S] - y[E])^2)
  }

  return(list(x = x, y = y, edge = edge))
}
