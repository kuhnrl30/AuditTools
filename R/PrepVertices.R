#' @export
PrepVertices<- function(x, endpoints){



  verts<- data.frame(name=unique(unlist(x)),
                     type="Event",
                     color="light blue",
                     stringsAsFactors = F)


  verts[grep("P[0-9]",verts$name, value=F),2:3]<-list("Place","gray")
  verts[verts$name %in% endpoints$Start,2:3]<- list("Start","green")
  verts[verts$name %in%  endpoints$End,2:3]<- list("End","red")

  return(verts)
}
