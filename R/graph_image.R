graph_image <- function(image){
  n = dim(image)[1]
  p = dim(image)[2]
  sommet = matrix(c(2:(n*p+1)),nrow=n,byrow = TRUE)
  n_edges = (n-1)*(p-2)*3 + 2*(n-1)*2 + 2*p
  edge = matrix(0,n_edges,3)
  colnames(edge) = c("v1","v2","distance")
  k=1
  for (i in 1:(n-1)){
    for (j in 2:(p-1)){
      edge[k,] = c(sommet[i,j],sommet[i+1,j-1],norm(as.matrix(image[i+1,j-1,]-image[i,j,])))
      edge[k+1,] = c(sommet[i,j],sommet[i+1,j],norm(as.matrix(image[i+1,j,]-image[i,j,])))
      edge[k+2,] = c(sommet[i,j],sommet[i+1,j+1],norm(as.matrix(image[i+1,j+1,]-image[i,j,])))

      k = k+3
    }
    edge[k,] = c(sommet[i,1],sommet[i+1,1],norm(as.matrix(image[i+1,1,]-image[i,1,])))
    edge[k+1,] = c(sommet[i,1],sommet[i+1,2],norm(as.matrix(image[i+1,2,]-image[i,1,])))

    edge[k+2,] = c(sommet[i,p],sommet[i+1,p-1],norm(as.matrix(image[i+1,p-1,]-image[i,p,])))
    edge[k+3,] = c(sommet[i,p],sommet[i+1,p],norm(as.matrix(image[i+1,p,]-image[i,p,])))

    k = k+4
  }
  for (j in 1:p){
    edge[k,] = c(1,sommet[1,j],0)
    edge[k+1,] = c(sommet[n,j],n*p+2,0)

    k=k+2
  }

  x = c(0)
  for (i in 1:n){
    x = c(x,rep(i,p))
  }
  x = c(x,n+1)

  y=c(0,rep(1:p,n),p+1)


  return(list(edge=edge,x=x,y=y))
}
