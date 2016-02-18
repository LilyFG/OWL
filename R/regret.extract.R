regret.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  regret=lapply(df, split.task.results, task = "regret")
  
# then apply a the data extraction function across that list where it is not empty
  lapply(regret, function(regret){
    if(!is.null(regret)){

      
      regret$trial.obtained[regret$wheelResponse=='left' & regret$wheels.left.outcome=='xv'] <-  
        regret$wheels.left.x[regret$wheelResponse=='left' & regret$wheels.left.outcome=='xv']
      regret$trial.obtained[regret$wheelResponse=='left' & regret$wheels.left.outcome=='yv'] <-  
        regret$wheels.left.y[regret$wheelResponse=='left' & regret$wheels.left.outcome=='yv']
      regret$trial.obtained[regret$wheelResponse=='right' & regret$wheels.right.outcome=='xv'] <-  
        regret$wheels.right.x[regret$wheelResponse=='right' & regret$wheels.right.outcome=='xv']
      regret$trial.obtained[regret$wheelResponse=='right' & regret$wheels.right.outcome=='yv'] <-  
        regret$wheels.right.y[regret$wheelResponse=='right' & regret$wheels.right.outcome=='yv']
      
      regret$trial.unobtained[regret$trialType=='partial' & regret$wheelResponse=='left' & regret$wheels.left.outcome=='xv'] <-  
        regret$wheels.left.y[regret$trialType=='partial' & regret$wheelResponse=='left' & regret$wheels.left.outcome=='xv']
      regret$trial.unobtained[regret$trialType=='partial' & regret$wheelResponse=='left' & regret$wheels.left.outcome=='yv'] <-  
        regret$wheels.left.x[regret$trialType=='partial' & regret$wheelResponse=='left' & regret$wheels.left.outcome=='yv']
      regret$trial.unobtained[regret$trialType=='partial' & regret$wheelResponse=='right' & regret$wheels.right.outcome=='xv'] <-  
        regret$wheels.right.y[regret$trialType=='partial' & regret$wheelResponse=='right' & regret$wheels.right.outcome=='xv']
      regret$trial.unobtained[regret$trialType=='partial' & regret$wheelResponse=='right' & regret$wheels.right.outcome=='yv'] <-  
        regret$wheels.right.x[regret$trialType=='partial' & regret$wheelResponse=='right' & regret$wheels.right.outcome=='yv']
      
      regret$trial.unobtained[regret$trialType=='complete' & regret$wheelResponse=='left' & regret$wheels.right.outcome=='xv'] <-  
        regret$wheels.right.x[regret$trialType=='complete' & regret$wheelResponse=='left' & regret$wheels.right.outcome=='xv']
      regret$trial.unobtained[regret$trialType=='complete' & regret$wheelResponse=='left' & regret$wheels.right.outcome=='yv'] <-  
        regret$wheels.right.y[regret$trialType=='complete' & regret$wheelResponse=='left' & regret$wheels.right.outcome=='yv']
      regret$trial.unobtained[regret$trialType=='complete' & regret$wheelResponse=='right' & regret$wheels.left.outcome=='xv'] <-  
        regret$wheels.left.x[regret$trialType=='complete' & regret$wheelResponse=='right' & regret$wheels.left.outcome=='xv']
      regret$trial.unobtained[regret$trialType=='complete' & regret$wheelResponse=='right' & regret$wheels.left.outcome=='yv'] <-  
        regret$wheels.left.y[regret$trialType=='complete' & regret$wheelResponse=='right' & regret$wheels.left.outcome=='yv']
      
      # scale emotion rating to participant's mean and sd
      regret$rating.original <- regret$rating
      regret$rating <- NA
      regret$rating[regret$phase=='test' & 
                      regret$trial.obtained %in% c(-50, 50) & 
                      regret$trial.unobtained %in% c(-200, 200)] <- 
        scale(regret[regret$phase=='test' & 
                       regret$trial.obtained %in% c(-50, 50) & 
                       regret$trial.unobtained %in% c(-200, 200), 
                     "rating.original"])
      
      #the difference in anticipated disappointment, anticipated regret, expected value and risk are calculated in accordance with Coricelli et al. 2004 and Burnett et al. 2010
      #we're not doing anything with these variables yet, but they would be available for modelling
      regret$d <- ((regret$wheels.right.y-regret$wheels.right.x)*(1-regret$wheels.right.p)-(regret$wheels.left.y-regret$wheels.left.x)*(1-regret$wheels.left.p))
      
      regret$r <- (regret$wheels.right.y-regret$wheels.left.x)-(regret$wheels.left.y-regret$wheels.right.x)
      
      regret$e <- ((regret$wheels.left.x * regret$wheels.left.p + regret$wheels.left.y * (1 - regret$wheels.left.p)) - 
                     (regret$wheels.right.x * regret$wheels.right.p + regret$wheels.right.y * (1-regret$wheels.right.p)))
      
      regret$risk <- (sqrt(regret$wheels.left.p * (regret$wheels.left.x - (regret$wheels.left.x * regret$wheels.left.p + regret$wheels.left.y * (1 - regret$wheels.left.p)))^2
                           + regret$wheels.left.p * (regret$wheels.left.y - (regret$wheels.left.x * regret$wheels.left.p + regret$wheels.left.y * (1 - regret$wheels.left.p)))^2)
                      -sqrt(regret$wheels.right.p * (regret$wheels.right.x - (regret$wheels.right.x * regret$wheels.right.p + regret$wheels.right.y * (1 - regret$wheels.right.p)))^2
                            + regret$wheels.right.p * (regret$wheels.right.y - (regret$wheels.right.x * regret$wheels.right.p + regret$wheels.right.y * (1 - regret$wheels.right.p)))^2))
      
      
      regret.rating <- reshape::melt(regret[regret$phase=='test' & regret$trial.obtained %in% c(-50, 50) & regret$trial.unobtained %in% c(-200, 200), 
                                   c("rating", "trialType", "trial.obtained", "trial.unobtained")], 
                            id=c("trialType", "trial.obtained", "trial.unobtained"),
                            na.rm=TRUE)
      regret.rating.unobtained <-  reshape::melt(regret[regret$phase=='test' & regret$trial.obtained %in% c(-50, 50) & regret$trial.unobtained %in% c(-200, 200), 
                                               c("rating", "trialType", "trial.unobtained")], 
                                        id=c("trialType", "trial.unobtained"),
                                        na.rm=TRUE)
      regret.trialCount <- as.data.frame(cast(regret.rating, trialType~trial.obtained~trial.unobtained~variable, length))
      regret.rating <- as.data.frame(cast(regret.rating, trialType~trial.obtained~trial.unobtained~variable, mean))
      regret.rating.unobtained <- as.data.frame(cast(regret.rating.unobtained, trialType~trial.unobtained~variable, mean))
      regret.rating.unobtained$diff <- regret.rating.unobtained$`-200.rating` - regret.rating.unobtained$`200.rating`
      regret.RT <- reshape::melt(regret[regret$phase=='test' & regret$trial.obtained %in% c(50, -50) & regret$trial.unobtained %in% c(200, -200), 
                               c("ratingResponseTime", "trialType", "trial.obtained", "trial.unobtained")], 
                        id=c("trialType", "trial.obtained", "trial.unobtained"),
                        na.rm=TRUE)
      regret.RT <- as.data.frame(cast(regret.RT, trialType~trial.obtained~trial.unobtained~variable, mean))
      
      data.frame(
        regret.c.m50.m200.rating	= regret.rating[1,1],
        regret.c.50.m200.rating	= regret.rating[1,2],
        regret.c.m50.200.rating	= regret.rating[1,3],
        regret.c.50.200.rating	= regret.rating[1,4],
        regret.p.m50.m200.rating	= regret.rating[2,1],
        regret.p.50.m200.rating	= regret.rating[2,2],
        regret.p.m50.200.rating	= regret.rating[2,3],
        regret.p.50.200.rating	= regret.rating[2,4],
        regret.c.m50.m200.rt	= regret.RT[1,1],
        regret.c.50.m200.rt	= regret.RT[1,2],
        regret.c.m50.200.rt	= regret.RT[1,3],
        regret.c.50.200.rt	= regret.RT[1,4],
        regret.p.m50.m200.rt	= regret.RT[2,1],
        regret.p.50.m200.rt	= regret.RT[2,2],
        regret.p.m50.200.rt	= regret.RT[2,3],
        regret.p.50.200.rt	= regret.RT[2,3],
        regret.c.m50.m200.count	= regret.trialCount[1,1],
        regret.c.50.m200.count	= regret.trialCount[1,2],
        regret.c.m50.200.count	= regret.trialCount[1,3],
        regret.c.50.200.count	= regret.trialCount[1,4],
        regret.p.m50.m200.count	= regret.trialCount[2,1],
        regret.p.50.m200.count	= regret.trialCount[2,2],
        regret.p.m50.200.count	= regret.trialCount[2,3],
        regret.p.50.200.count	= regret.trialCount[2,4],
        regret.c.upward = mean(regret.rating.unobtained["complete",]$`200.rating`),
        regret.c.downward = mean(regret.rating.unobtained["complete",]$`-200.rating`),
        regret.p.upward = mean(regret.rating.unobtained["partial",]$`200.rating`),
        regret.p.downward = mean(regret.rating.unobtained["partial",]$`-200.rating`),
        regret.upward = mean(regret.rating.unobtained$`200.rating`),
        regret.downward = mean(regret.rating.unobtained$`-200.rating`),
        regret.c.unobtained.diff = regret.rating.unobtained["complete",]$diff,
        regret.p.unobtained.diff = regret.rating.unobtained["partial",]$diff,
        tags = regret$tags[1]
      )
    }
  })
}
