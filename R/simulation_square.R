simulation_square <- function(graphe,algorithm){

  res = algorithm(graphe)
  print(res)

  path = res$path
  plot(graphe$x, graphe$y)
  for(i in 1:dim(graphe$edge)[1]){segments(x0 = graphe$x[graphe$edge[i,1]],
                                           y0 = graphe$y[graphe$edge[i,1]],
                                           x1 = graphe$x[graphe$edge[i,2]],
                                           y1 = graphe$y[graphe$edge[i,2]], lwd = 0.1)}

  for (s in path){
    points(graphe$x[s], graphe$y[s], col = 2, cex = 2)
  }
  for (i in 1:(length(path)-1)){
    segments(x0 = graphe$x[path[i]],
             y0 = graphe$y[path[i]],
             x1 = graphe$x[path[i+1]],
             y1 = graphe$y[path[i+1]],
             lwd = 1,col=2)}
}
