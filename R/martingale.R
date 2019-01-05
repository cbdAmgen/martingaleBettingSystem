rm(list = ls())
cat("\014")

library(tidyverse)

# totalHands <- 1:600 # 1 minute per hand. Willing to play up to 10 hours
totalHands <- 1:30
pWin <- 0.4748 # probability of winning each hand
nSims <- 10000 # number of simulations for each number of hands
maxNumLosses <- 6
currentLosses <- 1:maxNumLosses

beginningBet <- 1
beginningBet * cumsum(2^(currentLosses-1))
maxDollarLoss <- 1000


output <- matrix(NA, ncol = maxNumLosses, nrow = max(totalHands))
avgWinnings <- matrix(NA, ncol = maxNumLosses, nrow = max(totalHands))
system.time(
for(i in totalHands){
  print(i)
  blah <- matrix(NA, ncol = maxNumLosses, nrow = nSims)
  winnings <- matrix(NA, ncol = maxNumLosses, nrow = nSims)
  for(j in 1:nSims){
    
    seqWinLoss <- rbinom(i, 1, pWin) # simulate a sequence of wins and losses 
    seqLengthAndValues <- rle(seqWinLoss) # The rle() function gives the length of
    # all sequences and the value (Win or Loss)
    # of each sequence
    
    allStreaks <- data.frame(length = seqLengthAndValues$lengths,
                             value = seqLengthAndValues$values)
    
    streaksOfLosses <- allStreaks %>% 
      filter(value == 0) # the sequences that were all losses
    
    
    
    numAtLeast <- vector(mode = "numeric", length = maxNumLosses)
    for(k in 1:maxNumLosses){
      numAtLeast[k] <- sum(streaksOfLosses$length >= k)
    }
    
    # for(k in 1:maxNumLosses){
    #   winnings[j, k] <- sum(allStreaks$length*allStreaks$value)*(numAtLeast[k] == 0) +
    #     -(2^k - 1)*(numAtLeast[k] > 0)
    # }
    
    # for(k in 1:maxNumLosses){
    #   
    #   tmp <- 0
    #   tmpVec <- vector(mode = "numeric", length = maxNumLosses)
    #   for(m in 1:nrow(allStreaks)){
    #     if(allStreaks$length[m] == k && allStreaks$value[m] == 0){
    #       tmp <- tmp - (2^k - 1)
    #       print(m)
    #       break
    #     } 
    #     else{
    #       print(m)
    #       tmp <- tmp + allStreaks$length[m]*allStreaks$value[m]
    #     }
    #   }
    #   
    #   winnings[j, k] <- tmp
    # }
    
    blah[j, ] <- numAtLeast
    
  }
  
  blah2 <- (blah > 0)
  output[i, ] <- colMeans(blah2)
  # avgWinnings[i, ] <- colMeans(winnings)
}
)
# output
# output <- round(output, 4)
# avgWinnings <- round(avgWinnings, 2)
# colnames(output) <- 1:ncol(output)
# colnames(avgWinnings) <- 1:ncol(avgWinnings)
# write.csv(output, file = "Output/output.csv")
# write.csv(avgWinnings, file = "Output/avgWinnings.csv")
