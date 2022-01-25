min_dist_vertex <- function(Q,d){
  mini = Inf
  sommet = -1
  for (s in Q){
    if (d[s]<mini){
      mini = d[s]
      sommet = s
    }
  }
  return(sommet)
}


v_neighbours <- function(v1,edges){
  v1_edges = edges[edges$v1 == v1,]
  return(v1_edges$v2)
}


Dijkstra <- function(graphe){

  n_vertices = length(graphe$x) #nombre de sommets

  A = data.frame(graphe$edge) #arêtes
  A = A[!duplicated(A),] # on enlève les lignes dupliquées

  n_edges = nrow(A) #nombre d'arêtes

  s_deb = 1 # sommet début
  s_fin = n_vertices # sommet d'arrivé

  distances = rep(Inf,n_vertices)
  visited = rep(FALSE,n_vertices)
  distances[1] = 0

  v1 = 1
  P = c()

  while (length(P) != n_vertices){
    Q = setdiff(c(1:n_vertices),P)
    v1 = min_dist_vertex(Q,distances)
    P = c(P,v1)
    A_v1 = A[A$v1==v1,]
    for (v2 in v_neighbours(v1,A)){
      if (distances[v2] > distances[v1]+A_v1$distance[A_v1$v2==v2]){
        distances[v2] = distances[v1]+A_v1$distance[A_v1$v2==v2]
        visited[v2] = v1
      }
    }
  }

  shortest_path = c()
  s = s_fin
  while (s!=s_deb){
    shortest_path = c(s,shortest_path)
    s = visited[s]
  }
  shortest_path = c(s_deb,shortest_path)

  return(list(path = shortest_path, distance = distances[n_vertices]))
}
