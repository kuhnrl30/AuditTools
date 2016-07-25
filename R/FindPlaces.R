#' Find the places between the events
#'
#' @param P dataframe of interactions with flow indicators (#,||,R,L)
#' @return dataframe
#' @importFrom dplyr "%>%"
#' @export
#'
FindPlaces<- function(P){
  AA<- P %>% filter(V3=="R")		# Find all events with succeeding events-  row has an R tag
  
  # For each unique left side event, make a list of all right side events
  AB<- unique(AA$V1)
  AC<-sapply(AB, function(x) (subset(AA, V1==x, V2, drop=F))) 	
  names(AC)<- gsub(".V2","", names(AC))
  
  # Find combinations that are parallel
  AD<- P %>% filter(V3=="||")

  
  PlacesTo<- data.frame()
  for(i in 1:length(AC)){		# Loop through each unique LHS
    if(length(AC[[i]])>1){		# Check if set is longer than 1 element
      for(j in 1:length(AC[[i]])){  # loop through RHS comparing each element
        for(k in 1:length(AC[[i]])){
          if(j<k){
            a<- nrow(subset(AD, V1==AC[[i]][j] & V2==AC[[i]][k])) # Check if element pair is in parallels
            if(a==0){  # if in the parallels, then add to places
              PlacesTo<- rbind(PlacesTo,
                               cbind(From=names(AC[i]),AC[[i]][j],AC[[i]][k]),
                               stringsAsFactors=F)
            }
          }
        }
      }
    }
  }


  PlacesTo$Place<- paste0("P",1:nrow(PlacesTo))

  CleanPlacesTo<- rbind(
    cbind(PlacesTo$From, PlacesTo$Place),
    cbind(PlacesTo$Place, PlacesTo$V2),
    cbind(PlacesTo$Place, PlacesTo$V3))



  # PlacesFrom ----
  # AA
  AE<- AB<- unique(AA$V2)
  AF<-sapply(AE, function(x) (subset(AA, V2==x, V1, drop=F)))
  names(AF)<- gsub(".V1","", names(AF))
  # AD

  # AC:AF
  PlacesFrom<- data.frame()
  for(i in 1:length(AF)){
    if(length(AF[[i]])>1){
      for(j in 1:length(AF[[i]])){
        for(k in 1:length(AF[[i]])){
          if(j<k){
            a<- nrow(subset(AD, V1==AF[[i]][j] & V2==AF[[i]][k]))
            if(a==0){
              PlacesFrom<- rbind(PlacesFrom,
                                 cbind(To=names(AF[i]),AF[[i]][j],AF[[i]][k]),
                                 stringsAsFactors=F)
            }
          }
        }
      }
    }
  }
  rm(a)

  NextPlace<- as.numeric(max(gsub("P","",PlacesTo$Place)))+1

  PlacesFrom$Place<- paste0("P", NextPlace:(nrow(PlacesFrom)+NextPlace-1))

  CleanPlacesFrom<- rbind(
    cbind(PlacesFrom$Place, PlacesFrom$To),
    cbind(PlacesFrom$V2, PlacesFrom$Place),
    cbind(PlacesFrom$V3, PlacesFrom$Place))

  CleanPlacesFinal<- as.data.frame(rbind(CleanPlacesTo, CleanPlacesFrom))

return(CleanPlacesFinal)

}
