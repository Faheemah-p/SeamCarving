#include <iostream>

using namespace std;

int main()
{
    #include <Rcpp.h>
using namespace Rcpp;


// Bellmanford

void bellman_ford(NumericMatrix edges,int vertex_source)
{
  int n;
  n=edges.ncol();
  NumericVector dis(n);

  for (int i=0; i<n; i++){
    dis[i]= INT_MAX;
  }


  /* Initialisation des distances à  l'infini */
  for(int k=0;k<n;k++)
  {
    dis[k]=INT_MAX;

  }
  dis[vertex_source]=0;

  /* relaxation de l'algorithme en itérant l'actualisation des distances n-1 fois */
  for(int k=0;k<n-1;k++){

    for(int i=0;i<n;i++)
    {
      for(int j=0;j<n;j++)
      {
        if(edges(i,j) !=0 && dis[i] != INT_MAX && dis[i]+edges(i,j) < dis[j] )
        {
          dis[j]=dis[i]+edges(i,j);
        }
      }
    }
  }



  /* vérification de l'existance d'un cycle négatif */
  for(int i=0;i<n;i++)
  {
    for(int j=0;j<n;j++)
    {
      if(edges(i,j) !=0 && dis[i]+edges(i,j) < dis[j])
      {
        Rcout<<"\n\nle graphe contient un cycle de poids négatif \n";
        return;
      }
    }
  }
  Rcout<<"\nVertex"<<"  Distance from source";
  for(int i=0;i<n;i++)
  {
    Rcout<<"\n"<<i<<"\t"<<dis[i];
  }

}

#include <Rcpp.h>
// 

using namespace Rcpp;
using namespace std;


int findMinVertex(NumericVector distance ,LogicalVector visited, int n){

  int minVertex=-1;
  for (int i=0 ;i< n; i++){
    if (!visited[i] && (minVertex==-1 || distance[i]< distance[minVertex])){
      minVertex = i;
    }

  }
  return minVertex;
}



// Function qui permet de trcaer le chemin
// de la source j en utilisant
//  parent
// 
void printPath(NumericVector parent, int j)
{

  
  if (parent[j] == - 1)
    return;

  printPath(parent, parent[j]);

  Rcout<< j;
}

// Fonction permettant de dessiner
// la distance
// 
// [[Rcpp::export]]
void printSolution(NumericVector dist, int n,NumericVector parent)
{
  int src = 0;
  Rcout<<"V  | Dist | Path";
  for (int i = 1; i < n; i++)
  {
    Rcout<<endl<<src<<"->"<<i<<"| "<<dist[i]<<" | "<<src;
    printPath(parent, i);
  }
}



// Algorithme de Dijkastra

void dijkstra_cpp(NumericMatrix edges){
  int n;
  n=edges.ncol();
  NumericVector distance(n);
  NumericVector parent(n);
  LogicalVector visited(n);

  for (int i=0; i<n; i++){
    parent[0]=-1;
    distance[i]= INT_MAX;
    visited[i]= false;
  }
  distance[0]=0;

  for (int i=0;i<n-1;i++){

    int minVertex=findMinVertex(distance,visited,n);

    visited[minVertex]=true;

    for (int j=0;j<n;j++){

      if(edges(minVertex,j) !=0 && !visited[j]){
        int dist = distance[minVertex] + edges(minVertex,j);
        if (dist< distance[j]){
          parent[j]=minVertex;
          distance[j] = dist;
        }
      }
    }

  }


  Rcout<<endl<<"les plus courts chemin partant du sommet d'origine : "<<endl;
  printSolution(distance, n, parent);


}

}
