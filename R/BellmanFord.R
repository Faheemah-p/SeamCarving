BellmanFord <- function(graphe){

  n_vertices = length(graphe$x) #nombre de sommets

  A = data.frame(graphe$edge) #arêtes
  A = A[!duplicated(A),] # on enlève les lignes dupliquées

  n_edges = nrow(A) #nombre d'arêtes

  s_deb = 1 # sommet début
  s_fin = n_vertices # sommet d'arrivé

  # Etape 1 : Initialisation du graphe

  distances = rep(Inf,n_vertices)
  visited = rep(FALSE,n_vertices)
  distances[1] = 0
  v1 = 1

  # Etape 2 : Itération de l'algorithme en répétant l'actualisation des distances

  for (k in 1:(n_vertices-1)){
    for (e in 1:nrow(A)){
      u=A$v1[e]
      v=A$v2[e]
      w=A$distance[e]
      if(distances[v]>distances[u]+w){
        distances[v]=distances[u]+w
        visited[v]=u
      }
      if (distances[v] > distances[u]+w){
        return("il existe un cycle absorbant")
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
