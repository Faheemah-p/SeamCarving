# Recherche du plus court chemin dans un graphe

### Groupe 3 : Chaïma Boughanmi, Faheemah Padavia, Pascale Zouein

### 26 Janvier 2022

## I- Introduction 
La recherche d'un plus "court" chemin d'un point à un autre est un problème de la vie quotidienne. Il a d’ailleurs donné lieu aux développements de sites Internet qui se proposent de déterminer pour vous le meilleur itinéraire que ce soit en distance, en temps ou en coût. Il suffit de taper "recherche d’itinéraire" sur un navigateur pour s’en convaincre !
On imagine aisément que ce type de problème est modélisable par un graphe et par la recherche sur ce graphe du "meilleur chemin".
Cette question, d'apparence assez simpliste, représente en réalité un problème dit **NP-complet** pour lequel on ne connaît pas de solution en un temps polynomial. Ce n'est pas le seul problème dans cette catégorie, et les utilisations connexes du plus court chemin sont loin d'être toutes aussi triviales qu'on peut l'imaginer.

## Exemples 
Le problème du plus court chemin est extrêmement commun en algorithmique. On le retrouve dans divers domaines  : 

**Transport :** 
Quand on livre un colis, il y a souvent plusieurs moyens de transport à notre disposition. Chacun permet de se rendre d'un point A à un point B, coûte une certaine somme d'argent et met une durée prédéfinie pour se déplacer. On peut représenter ce problème sous forme d'un graphe pondéré, et le but de l'entreprise serait de trouver un chemin entre A et B pouvant alterner les formes de transports et minimisant par exemple le temps de trajet (en général, une entreprise cherchera aussi à minimiser le coût de transport).

**Génétique :**  Lorsqu'on a deux chaînes d'ADN sous la forme d'une suite de nucléotides comme "AGGCTATGGC" et "ATGCAATGCC", et que l'on souhaite trouver le nombre minimum de mutations à appliquer à une chaîne pour se retrouver avec l'autre, on peut utiliser un algorithme de plus court chemin. Ce problème n'est pas restreint au domaine de la génétique, et il est assez fréquent de vouloir transformer une chaîne en une autre en un minimum d'opérations. Le graphe représenterait les différents états de la chaîne, et les arcs pourraient être une opération (avec comme pondération le coût de cette transformation). On peut aussi appliquer cette idée de graphe implicite à des nombres, ou tout autre forme de structure pouvant subir des modifications diverses (ajout, suppression, transformation, etc).

**Finance :**  On a vu les cycles améliorants comme un concept péjoratif, cependant ils sont parfois très utiles comme en finance, où trouver ce genre de cycle peut être avantageux et permet de gagner de l'argent facilement. Si l'on imagine un graphe pondéré de multiples transactions bancaires (par exemple lorsque vous convertissez des euros en dollars), si on arrive à trouver un cycle améliorant dans ce graphe, cela signifie qu'on peut convertir en "boucle" et gagner encore plus d'argent à chaque tour.

## Problématique et objectifs
L'objectif de ce projet est de créer un package permettant de déterminer le chemin le plus court entre deux noeuds grâce aux deux algorithmes **Bellman Ford** et **Dijkstra** plus précisemment avec les points de carré [0,1] x [0,1] on va étudier le nombre de points à traverser et la longueur du chemin (à comparer avec racine de √2) en fonction du nombre de points et d'arrêtes, en suite on enchaînera avec une application sur le problème de **réduction** d'image avec ces algorithmes. 

 Il existe différentes manières de répondre à cette question, et la réponse varie souvent en fonction du graphe donné en entrée de notre algorithme (pondéré positivement, négativement, etc.). Savoir reconnaître un problème de plus court chemin est donc important, et utiliser le bon algorithme l'est encore plus.

## II- L'algorithme de Bellmanford: Solution naïve 
Cet algorithme permet de calculer des plus courts chemins depuis un sommet source donné dans un graphe **orienté pondéré**(c'est à dire que les arêtes ont des poids entier ou flottant positif.

## Description 
L'algorithme utilise le principe de la programmation **dynamique**, étant donné un graphe G=(S,A) de sommets S et d'arêtes A et un sommet source s ∈ S, un plus court chemin de s à chaque sommet de G. Il permet en plus de trouver un tel chemin en retournant les prédécesseurs de chaque sommet dans un tel chemin. En outre, il permet de détecter les cas où  il existe un cycle de poids négatif dans lesquels il n'existe pas nécessairement de plus court chemin entre deux sommets. Dans notre cas le poids est la distance donc on ne peut pas parler de poids négatif. 

## Fonctionnement

## Implémentation de l'algorithme 
Pour coder l'algorithme on aura besoin d'une fonction qui simule un graphe aléatoirement de n sommets et p arêtes. Cette fonction est nommée **graph**. Elle retourne le sommet de départ **i**, le sommet d'arrivé **j** et le poids  entre ces deux sommets poids[i][j]. Cette matrice est symétrique.

```{r}
# Fonction pour représenter aléatoirement un graphe
graph <- function(n, nbPaths) 
{
  x <-  runif(n-2)
  y <-  runif(n-2)
  x <- c(0,x,1)
  y <- c(0,y,1)
  edge <- matrix(0, (n-1)*nbPaths, 3) # chaque élément de edge est défini par le sommet
                                      # de départ, le sommet d'arrivée, et le poids qui 
                                      #est la distance entre ces 2 sommets 
  colnames(edge) <- c("v1","v2", "distance") 
  
  for(i in 1:nbPaths)
  {
    pos <- ((i-1)*(n-1) +1):(i*(n-1))  
    U <- sample(2:(n-1)) 
    S <- c(1,U)
    E <- c(U,n)
    edge[pos, 1] <- S
    edge[pos, 2] <- E
    edge[pos, 3] <- sqrt((x[S] - x[E])^2 +  (y[S] - y[E])^2)  # distance euclidienne entre 2 points 
  }
  
  return(list(x = x, y = y, edge = edge))
}
```

## Exemple
Simulation d'un graphe de 10 sommets.

```{r}
### Exemple :
n <- 10 ## nombre total de sommets
nbPaths <- 2  ### nbre de chemins choisi
res <- graph(n,nbPaths)
res
#### affihcer les chemins
plot(res$x, res$y, xlim = c(0,1), ylim = c(0,1))
for(i in 1:dim(res$edge)[1]){segments(x0 = res$x[res$edge[i,1]],   
                                      y0 = res$y[res$edge[i,1]], 
                                      x1 = res$x[res$edge[i,2]], 
                                      y1 = res$y[res$edge[i,2]], lwd = 0.1)}
points(res$x[1], res$y[1], col = 2, cex = 2)
points(res$x[n], res$y[n], col = 2, cex = 2)

```

## Algorithme de Bellmanford
L'implémentation de l'algorithme comprend trois étapes : 

**1-** Initialisation du graphe de n sommets: Initialisation des distances à l'infini et du sommet initial à 0.

**2-** Relaxation de l'algorithme en répétant l'actualisation des distances : si (distance départ + poids arête < distance arrivée) on actualise la distance de l'arrivée et on arrête l'algorithme à n-1 itérations. 

**3-** Recherche de cycle de poids négatif en cas d'existence.

```{r}
BellmanFord <- function(graphe){
  
  n_vertices = length(graphe$x) #nombre de sommets
  n_edges = nrow(graphe$edge) #nombre d'arêtes
  
  A = data.frame(graphe$edge) #arêtes
  A = A[!duplicated(A),] # on enlève les lignes dupliquées
  S = data.frame(graphe$x,graphe$y) #sommets
  colnames(S) = c("x","y")
  
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
  #Déterminer le plus court chemin
  shortest_path = c()
  s = s_fin
  while (s!=s_deb){
    shortest_path = c(s,shortest_path)
    s = visited[s]
  }
  shortest_path = c(s_deb,shortest_path)
  
  return(list(path = shortest_path, distance = distances[n_vertices]))
}
```

## III- L'algorithme de Dijkstra: Solution améliorée
L'algorithme de Dijkstra permet de résoudre un problème algorithmique : le problème du plus court chemin. Ce problème a plusieurs variantes. La plus simple est la suivante : étant donné un graphe non-orienté, dont les arêtes sont munies de poids positifs, et deux sommets de ce graphe, trouver un chemin entre les deux sommets dans le graphe, de poids minimum. L'algorithme de Dijkstra permet de résoudre un problème plus général : le graphe peut être orienté, et l'on peut désigner un unique sommet, et demander d'avoir la liste des plus courts chemins pour tous les autres nœuds du graphe.

## Description 
L'algorithme prend en entrée un graphe orienté pondéré par des réels positifs et un sommet source. Il s'agit de construire progressivement un sous-graphe dans lequel sont classés les différents sommets par ordre croissant de leur distance minimale au sommet de départ. La distance correspond à la somme des poids des arcs empruntés.

## Fonctionnement 

Pour trouver le chemin le plus court entre a et b cet algorithme choisit le sommet non visité avec la distance la plus faible, calcule la distance à travers lui à chaque voisin non visité, et met à jour la distance du voisin si elle est plus petite. Il marque le sommet visité (en rouge) lorsque il termine avec les voisins…

## Implémentation de l'algorithme 

Au départ, on considère que les distances de chaque sommet au sommet de départ sont infinies, sauf pour le sommet de départ pour lequel la distance est nulle. Le sous-graphe de départ est l'ensemble vide.

Au cours de chaque itération, on choisit en dehors du sous-graphe un sommet de distance minimale et on l'ajoute au sous-graphe. Ensuite, on met à jour les distances des sommets voisins de celui ajouté. La mise à jour s'opère comme suit : la nouvelle distance du sommet voisin est le minimum entre la distance existante et celle obtenue en ajoutant le poids de l'arc entre sommet voisin et sommet ajouté à la distance du sommet ajouté.

On continue ainsi jusqu'à épuisement des sommets (ou jusqu'à sélection du sommet d'arrivée). 

Pour ce faire, nous commençons par créer une fonction **min_dist_vertex**  qui détecte le sommet le plus proche du sommet de départ v1 parmi un ensemble de sommets Q, puis une deuxième fonction **v_neighbours** est nécessaire pour donner la liste des sommets voisins au sommet choisi v1 


```{r}
# Fonction qui donne le sommet le plus proche du sommet v1 
# parmi un ensemble de sommets Q
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

# Fonction qui donne les sommets voisins à un sommet v1
v_neighbours <- function(v1,edges){
  v1_edges = edges[edges$v1 == v1,]
  return(v1_edges$v2) ### 
}

###

Dijkstra <- function(graphe){  # Partie commune avec bellmanford
  
  n_vertices = length(graphe$x) # nombre de sommets
  
  A = data.frame(graphe$edge) # arêtes
  A = A[!duplicated(A),] # on enlève les lignes dupliquées
  
  n_edges = nrow(A) # nombre d'arêtes
  
  s_deb = 1 # sommet début
  s_fin = n_vertices # sommet d'arrivée
  
  distances = rep(Inf,n_vertices)
  visited = rep(FALSE,n_vertices) 
  distances[1] = 0
  
  v1 = 1
  P = c()
  
  while (length(P) != n_vertices){
    Q = setdiff(c(1:n_vertices),P) # retourne les sommets qui sont qui ne soint pas dans p
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

```

## Exécution des deux algorithmes sur un graphe donné 

```{r}
n <- 100
nbPaths <- 20
res <- graph(n,nbPaths)

print("Bellman Ford")
BellmanFord(res)

print("Dijkstra")
Dijkstra(res)

```

Pour un graphe contenant 100 sommets les deux algorithmes donnent le même résultat: les points visités sont respectivement 1  67  7  25 100 et une distance presque égale à √2. 

## Afficher les chemins suivis par l'algorithme Dijkstra 

```{r}
# pour afficher le chemin avec les sommets
chemin <- function(graphe,path){
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

chemin(res,Dijkstra(res)$path)

```

## Comparaison des deux algorithmes :

L'algorithme de **Bellman-Ford** offre  une toute nouvelle approche au problème du plus court chemin grâce à la programmation **dynamique**. Ceci lui permet d'être applicable sur tous types de graphes pondérés, contrairement à l'algorithme de **Dijkstra** qui n'est employé que sur des graphes pondérés **positivement** 
