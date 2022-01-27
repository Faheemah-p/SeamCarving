remove_seam <- function(image,algo){
  g_img = graph_image(image)
  seam = algo(g_img)$path
  seam = seam[2:(length(seam)-1)]
  n = dim(image)[1]
  p = dim(image)[2]
  sommets = matrix(c(2:(n*p+1)),nrow=n,byrow = TRUE)
  image2 = array(rep(0,n*(p-1)*3), c(n, p-1, 3))
  for(i in 1:length(seam)){
    indice = which(sommets == seam[i],arr.ind = TRUE)[2]
    image2[i,,1] = image[i,-indice,1]
    image2[i,,2] = image[i,-indice,2]
    image2[i,,3] = image[i,-indice,3]
  }
  return(image2)
}
