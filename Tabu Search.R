rm(list = ls())


#the following lines read the current location of the code and sets it as the working directory. The user doesn't have to define the working directory
workingDirectory <- unlist(strsplit(unlist(rstudioapi::getActiveDocumentContext()[2]),"/"))
workingDirectory <- paste(workingDirectory[-length(workingDirectory)],collapse = '//')
setwd(workingDirectory)

library(readxl)

#enter the parameters
m = 40
iterationCount <- 20
tabuTenure <- c(10,15,20) #we test the different tabu tenures to see how it impacts the results
stoppingCriteria <- 5000 #if there are no improvements for these many iterations, stop the run
report <- data.frame(instance = character(0),
                     InitialDiversity = numeric(0),
                     FinalDiversity = numeric(0),
                     runTime = numeric(0),
                     TabuTenure = numeric(0))

#we read each instance and perform tabu search on them
for (instanceIter in c('a','b','c','d','e')){
  df <- read_excel(paste0('Instances/',instanceIter,'.xlsx'),col_names = F)
  colnames(df) <- c(1:nrow(df))
  #we create a random initial solution
  randomInitial <- sample(c(1:ncol(df)),1)
  BestSolution <- as.data.frame(df[,as.character(randomInitial)])
  BestSolution$candidate <- rownames(BestSolution)
  BestSolution <- BestSolution[order(BestSolution[,as.character(randomInitial)], decreasing = T),'candidate']
  BestSolution <- c(randomInitial,as.numeric(BestSolution[1:(m-1)]))
  
  initialDiversity <- sum(df[BestSolution,BestSolution])/2
  MaxDiversity <- initialDiversity
  
  #the following loop iterates for each tabu tenure
  for (tenureIter in tabuTenure) {
    tabuList <- data.frame(From = rep(NA,tenureIter),To = rep(NA,tenureIter))
    bestSwap <- tabuList[1,] 
    tic <- Sys.time()

    #for each element of our initial solution, we look for swap candidates and select the one that  gives the maximum diversity

    for (iterCount in c(1:iterationCount)) {
      noImprovement <- 0
      #the neighborhood of our current solution
      candidates <- as.numeric(colnames(df)[!colnames(df) %in% BestSolution])
      for (i in BestSolution) {
        for (j in candidates) {
          if (noImprovement >= stoppingCriteria) break
          if (i %in% tabuList$To | j %in% tabuList$From ) {
            next
          }
          iterSolution <- replace(BestSolution,which(BestSolution==i),as.numeric(j))
          newDiversity <- sum(df[iterSolution,iterSolution])/2
          
          Swap <- data.frame(From = i,To = j,Diversity = newDiversity)
          #we identify the best swap and update the current solution
          newSolution <- replace(BestSolution,which(BestSolution==Swap$From),Swap$To)
          
          # if the current solution is better than the best solution, replace the best solution
          if (newDiversity>MaxDiversity) {
            MaxDiversity = newDiversity
            bestSwap = Swap
            BestSolution = newSolution
          } else noImprovement = noImprovement+1

        }
      }
      # we update the tabu list based on the swaps we performed
      tabuList <- rbind.data.frame(tabuList[-1,],bestSwap[,c('From','To')])
      row.names(tabuList) <- c(1:tenureIter)
    }
    toc <- Sys.time()
    
    print(paste('For Instance',instanceIter,'the initial diversity is',initialDiversity,'and the final diversity is',MaxDiversity))
    
    #we collate the results of different tabu tenure for each instance in a table
    report <- rbind.data.frame(report,data.frame(instance = instanceIter,
                                                 InitialDiversity = initialDiversity,
                                                 FinalDiversity = MaxDiversity,
                                                 runTime = toc-tic,
                                                 TabuTenure = tenureIter))
    
  }
  
}


report

#exporting the report as a csv in the current directory
write.csv(report,'TabuSearch_MDP 05-08.csv',row.names = F)
















