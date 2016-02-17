health.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  health=lapply(df, split.task.results, task = "health")
  
  # function for converting weight data into kgs
  convertWeight <- function(weight){ 
    if(grepl("st ",weight)){ # if the variable 'weight' contains the characters "st "
      weight<-strsplit(weight, "st ") # split the string up into pre- "st " (stones) and post "st " (pounds)
      weight<-as.numeric(weight[[1]][1])*14+as.numeric(strsplit(weight[[1]][2], "lbs")[[1]][1]) # multiply the the stones by 14 to get pounds, and add the remaining pounds
      weight<-weight*0.45359} else{ # convert to kgs
        if(grepl("lbs",weight)){ # else, if weight just contains the string "lbs"
          weight<-as.numeric(strsplit(weight, "lbs")[[1]][1]) #then remove the units
          weight<-weight*0.45359} else { # and convert to kgs
            if(grepl("kg",weight)){ # else, if the weight is in kgs
              weight<-as.numeric(strsplit(weight, "kg")[[1]][1])}else{ # remove the units
                weight<-NA #else, the weight question has been skipped
              }
          }
      }
  }
  
  #function for converting height data into cms
  convertHeight <- function(height){
    if(grepl("\'",height)){ # the \ is used because ' and " are special characters, so extracting those from the string was a bit tricky
      height<-strsplit(height, "\'") # split height into feet and inches
      height<-as.numeric(height[[1]][1])*12+as.numeric(strsplit(height[[1]][2], '\"')[[1]][1]) # multiply feet by 12 and add to inches
      height<-height*2.54} else{ # convert to cms
        if(grepl("cm",height)){ # if it's already cms then remove the units
          height<-as.numeric(strsplit(height, "cm")[[1]][1])}else{
            height<-NA #else, the weight question has been skipped
          }
      }
  }
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(health, function(health){
    if(!is.null(health)){
      data.frame(
        health.illness	=	tail(health[health$id==1|health$id==1,"response"],n=1),
        health.psych	=	tail(health[health$id==2,"response"],n=1),
        health.healthRating	=	tail(5-health[health$id==3,"score"],n=1),
        health.height	=	ifelse(!is.null(tail(health[health$id==4,"height"])), 
                               convertHeight(tail(health[health$id==4,"height"],n=1)), NA),
        health.weight	=	ifelse(!is.null(tail(health[health$id==5,"weight"])), 
                               convertWeight(tail(health[health$id==5,"weight"],n=1)), NA),
        health.heaviest	=	ifelse(!is.null(tail(health[health$id==6,"weight"])), 
                                 convertWeight(tail(health[health$id==6,"weight"],n=1)), NA),
        health.weightStability	=	tail(health[health$id==7,"rating"],n=1),
        health.dieting	=	tail(health[health$id==8,"response"],n=1),
        health.sleep	=	tail(health[health$id==9,"response"],n=1),
        health.smoke	=	tail(health[health$id==10,"response"],n=1),
        health.nSmoke	=	ifelse(tail(health[health$id==10,"response"]=="Yes",n=1),tail(health[health$id==10,"followup"],n=1)[[1]]$score[1],NA),
        health.quitSmoke	=	ifelse(tail(health[health$id==10,"response"]=="Yes",n=1),tail(health[health$id==10,"followup"],n=1)[[1]]$response[2],NA),
        health.everSmoked	=	ifelse(tail(health[health$id==10,"response"]=="No",n=1),tail(health[health$id==10,"followup"],n=1)[[1]]$response[1],NA),
        health.alcohol	=	tail(health[health$id==11,"score"],n=1),
        health.alcoholUnits	=	ifelse(tail(health[health$id==11,"response"]=="Yes",n=1),tail(health[health$id==11,"followup"],n=1)[[1]]$response[1],NA),
        health.everAlcohol	=	ifelse(tail(health[health$id==11,"response"]=="No",n=1),tail(health[health$id==11,"followup"],n=1)[[1]]$response[1],NA),
        health.drugs	=	tail(health[health$id==12,"response"],n=1),
        health.freqExercise	=	tail(health[health$id==13,"score"],n=1),
        health.lenExercise	=	tail(health[health$id==14,"score"],n=1),
        health.tv	=	tail(health[health$id==15,"score"],n=1),
        health.computerGame	=	tail(health[health$id==16,"score"],n=1),
        health.reading	=	tail(health[health$id==17,"score"],n=1),
        health.socialMedia	=	tail(health[health$id==18,"score"],n=1),
        health.vegServings	=	tail(health[health$id==19,"score"],n=1),
        health.fruitServings	=	tail(health[health$id==20,"score"],n=1),
        stringsAsFactors = F)
    }
  })
}
