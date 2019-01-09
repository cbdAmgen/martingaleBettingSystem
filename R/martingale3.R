rm(list = ls())
cat("\014")

library(tidyverse)

totalHands <- 10    # 1 minute per hand. This is where you set up your bets/time restriction
bankroll <- 1000    # starting amount in pocket. This is where you set up your wealth restriction
B <- 1              # beginning bet

p <- 0.4748         # probability of winning each hand
q <- 1 - p          # probability of losing each hand

nSims <- 10000       # number of simulations for each number of hands


# maxNumLosses <- max(which(B * (2^(1:100) - 1) <= bankroll)) # maximum number of losses you can afford
maxNumLosses <- 10

goBustProb <- matrix(0, ncol = maxNumLosses, nrow = totalHands)
expectedWinnings <- matrix(0, ncol = maxNumLosses, nrow = totalHands)
expectedWinningsEnd <- matrix(0, ncol = maxNumLosses, nrow = totalHands)
seqWinLossList <- vector("list", length = totalHands)
winningsList <- vector("list", length = totalHands)


func1 <- function(k){
  
  tmp <- 0
  broken <- 0
  if(nRowAS > 1){
    for(m in 1:(nRowAS - 1)){
      if(allStreaks$length[m] >= k && allStreaks$value[m] == 0){
        tmp <- tmp - B*(2^k - 1)
        broken <- 1
        break
      }else if(allStreaks$length[m] < k && allStreaks$value[m] == 0){
        next
      }else{
        tmp <- tmp + B*allStreaks$length[m]
      }
    }
  }
  if(allStreaks$value[nRowAS] == 1 && !broken){
    tmp <- tmp + B*allStreaks$length[nRowAS]
    tmpEnd <- tmp
  }else if(allStreaks$value[nRowAS] == 0 && !broken){
    tmpEnd <- tmp
    numPlaysLeft <- max(0, k - allStreaks$length[nRowAS])
    endPlays <- rbinom(numPlaysLeft, 1, p)
    if(any(endPlays)){
      tmpEnd <- tmp + B
    }else{
      tmpEnd <- tmp - B*(2^k - 1)
    }
    howMany <- min(k, allStreaks$length[nRowAS])
    tmp <- tmp - B*(2^howMany - 1)
  }else{
    tmpEnd <- tmp
    # print("This loop is broken.")
  }
  
  # return(list(winnings = tmp, winningsEnd = tmpEnd))
  
  toReturn <- matrix(c(tmp, tmpEnd), ncol = 2, nrow =1)
  return(toReturn)
  
}


tictoc::tic()
for(i in 1:totalHands){
  print(i)
  blah <- matrix(0, ncol = maxNumLosses, nrow = nSims)
  blahEnd <- matrix(0, ncol = maxNumLosses, nrow = nSims)
  winnings <- matrix(NA, ncol = maxNumLosses, nrow = nSims)
  winningsEnd <- matrix(NA, ncol = maxNumLosses, nrow = nSims)
  seqWinLossList[[i]] <- matrix(NA, ncol = i, nrow = nSims)
  for(j in 1:nSims){
    
    seqWinLoss <- rbinom(i, 1, p)         # simulate a sequence of wins and losses 
    seqWinLossList[[i]][j,] <- seqWinLoss
    seqLengthAndValues <- rle(seqWinLoss) # The rle() function gives the length of
    # all sequences and the value (Win or Loss)
    # of each sequence
    
    allStreaks <- data.frame(length = seqLengthAndValues$lengths,
                             value = seqLengthAndValues$values)
    
    streaksOfLosses <- allStreaks %>% 
      filter(value == 0) # the sequences that were all losses
    
    numAtLeast <- sapply(1:min(i, maxNumLosses), 
                         function(x) sum(streaksOfLosses$length >= x))
    blah[j, 1:length(numAtLeast)] <- numAtLeast
    
    
    nRowAS <- nrow(allStreaks)
    
    
    allWinnings <- sapply(1:maxNumLosses, func1)
    
    # for(k in 1:maxNumLosses){
    #   
    #   winnings[j,k] <- allWinnings$winnings
    #   winningsEnd[j,k] <- allWinnings$winningsEnd
    #   
    # }
    winnings[j,] <- allWinnings[1,]
    winningsEnd[j,] <- allWinnings[2,]
  }
  
  blah2 <- (blah > 0)
  goBustProb[i, ] <- colMeans(blah2)
  expectedWinnings[i, ] <- colMeans(winnings)
  expectedWinningsEnd[i, ] <- colMeans(winningsEnd)
  winningsList[[i]] <- winnings
}
tictoc::toc()
goBustProb
expectedWinnings
expectedWinningsEnd
goBustProb <- round(goBustProb, 4)
expectedWinnings <- round(expectedWinnings, 4)
expectedWinningsEnd <- round(expectedWinningsEnd, 4)
colnames(goBustProb) <- 1:ncol(goBustProb)
colnames(expectedWinnings) <- 1:ncol(expectedWinnings)
colnames(expectedWinningsEnd) <- 1:ncol(expectedWinnings)
write.csv(goBustProb, file = "Output/goBustProb.csv")
write.csv(expectedWinnings, file = "Output/expectedWinnings.csv")
write.csv(expectedWinnings, file = "Output/expectedWinningsEnd.csv")
