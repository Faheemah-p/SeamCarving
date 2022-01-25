seam_carving <- function(image,algo,nb){
  for (i in 1:nb){
    image = remove_seam(image,algo)
  }
  return(image)
}
