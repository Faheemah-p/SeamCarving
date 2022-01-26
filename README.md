# Recherche du plus court chemin dans un graphe

### Groupe 3 : Chaïma Boughanmi, Faheemah Padavia, Pascale Zouein

### 26 Janvier 2022

`ShortPath` est un package R qui permet de déterminer le chemin le plus court d'un graphe depuis un sommet source à partir de deux algorithmes: l'***algorithme de Bellman-Ford*** et l'***algorithme de Dijkstra***.

En particulier, ce package permet de déterminer le chemin le plus court sur un graphe compris dans le ***carré unité***, qui a pour sommet de départ le sommet de coordonnées (0,0) et pour sommet d'arrivé le sommet de coordonnées (1,1).

Un exemple d'application de ces algorithmes est le ***seam carving***. C'est un algorithme de redimensionnement d'image développé par Shai AVIDAN et Ariel SHAMIR, qui redimensionne l'image par suppression de chamins de pixels dits de moindre énergie.

Ce package comporte plusieurs fonctions:
- `BellmanFord` 
Cette fonction utilise l'algorithme de Bellman-Ford pour déterminer le chemin le plus court entre le premier et le dernier sommet d'un graphe donné. Elle nous renvoie le chemin avec sa distance. L'algorithme de Bellman-Ford est un algorithme naïf qui utilise le principe de programmation dynamique, et autorise la présence d'arcs à poids négatif. La complexité de l'algorithme est en O(na) avec n le nombre de sommets et a le nombre d'arcs.
- `Dijkstra`
Cett fonction utilise l'algorithme de Dijkstra pour déterminer le chemin le plus court entre le premier et le dernier sommet d'un graphe donné. Elle nous renvoie le chemin avec sa distance. L'algorithme de Dijkstra est un algorithme glouton, et considère les graphes orientés pondérés par des réels positifs. C'est un algoritme de complexité polynomiale, c'est-à-dire pour n sommets à a arcs le temps est en O((a+n)log n).
- `graph_square`
Cette fonction renvoie une simulation de graphe compris dans le carré unité, plus précisément elle nous renvoie deux vecteurs pour chaque coordonnée des sommets ainsi qu'un tableau donnant les arcs (sommet d'où est issu l'arc, le sommet vers lequel il pointe, le poids de l'arc). Le premier sommet de sera le sommet de départ (0,0) et le dernier sera le sommet d'arrivé (1,1). La fonction prend en entrée deux paramètres: le nombre de sommets et le nombre de chemins (pour définir le nombre d'arcs)
- `simulation_square`
Cette fonction nous renvoie la simulation complète du problème du plus court chemin pour un graphe compris dans le carré unité. Elle prend en entrée les deux paramètres pour déterminer le graphe, c'est-à-dire le nombre de sommets et le nombre de chemin, et l'algorithme (Bellman-Ford ou Dijkstra) que l'on souhaite appliqué, puis construit le graphe, détermine le chemin le plus cours et nous affiche le graphe en mettant en avant le chemin le plus court.
- `graph_image`
Cette fonction renvoie le graphe associé à une image donné, en prenant comme sommet chaque pixel et détermine les arc de cette façon: si x(i,j) est un sommet, on lui associe les arcs qui vont vers x(i-1,j+1),x(i,j+1) et x(i+1,j+1). Évidemment, les sommets sur les bords à droite et à gauche n'auront que deux arcs et la dernière dernière rangée de sommets n'aura pas d'arcs. Puis, deux sommets sont rajoutés à ce graphe: un sommet relié à tous les sommets de la première rangée avec un poids nul et un sommet relié à la dernière rangée avec un poids nul aussi. Ces deux sommets représentent le sommet de départ et le sommet d'arrivé, et les arcs sont nuls car ils n'appartiennent pas à l'image. Ces sommets sont très important car le sommet de départ et le sommet d'arrivé ne peux pas être déterminé à l'avance car ils dépendent du chemin.
- `remove_seam`
Cette fonction prend en entrée une image et un algorithme choisit (Bellman-Ford ou Dijkstra), et nous renvoie l'image après avoir enlevé le chemin de pixels le plus court, c'est-à-dire le chemin de moindre énergie, déterminé à partir de l'algorithme. 
- `seam_carving`
Cette fonction redimentionne l'image par la méthode de seam carving en réduisant le nombre de pixels par un nombre entré en paremètre avec l'image. Elle va faire tourner la fonction 'remove_seam' autant de fois que le nombre de pixels à enlever en largeur.

Pour ce package, nous avons choisit d'appliquer le seam carving uniquement pour réduire l'image en largeur, c'est-à-dire qu'on ne considère pas le redimensionnement en hauteur et l'agrandissement de l'image. De plus, pour le calcul de l'énergie, nous avons choisit de regarder la norme des intensités des pixels/sommets adjacents que nous considérons comme poids des arcs.

