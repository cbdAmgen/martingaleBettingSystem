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
system.time(
  for(i in 1:totalHands){
    print(i)
    blah <- matrix(0, ncol = maxNumLosses, nrow = nSims)
    blahEnd <- matrix(0, ncol = maxNumLosses, nrow = nSims)
    winnings <- matrix(NA, ncol = maxNumLosses, nrow = nSims)
    winningsEnd <- matrix(NA, ncol = maxNumLosses, nrow = nSims)
    for(j in 1:nSims){
      
      seqWinLoss <- rbinom(i, 1, p)         # simulate a sequence of wins and losses 
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
      
      # for(k in 1:maxNumLosses){
      #   winnings[j, k] <- sum(allStreaks$length*allStreaks$value)*(numAtLeast[k] == 0) +
      #     -(2^k - 1)*(numAtLeast[k] > 0)
      # }
      
      for(k in 1:maxNumLosses){
        
        tmp <- 0
        if(nrow(allStreaks) > 1){
          for(m in 1:(nrow(allStreaks) - 1)){
            if(allStreaks$length[m] >= k && allStreaks$value[m] == 0){
              tmp <- tmp - B*(2^k - 1)
              # print(tmp)
              # print(str_c("m = ", m, ", tmp = ", tmp))
              break
            }
            else if(allStreaks$length[m] < k && allStreaks$value[m] == 0){
              # tmp <- tmp - B*(2^allStreaks$length[m] - 1)
              # print(str_c("m = ", m, ", tmp = ", tmp))
              next
            }
            else{
              # print(m)
              tmp <- tmp + B*allStreaks$length[m]
              # print(str_c("m = ", m, ", tmp = ", tmp))
            }
          }
        }
        if(allStreaks$value[nrow(allStreaks)] == 1){
          tmp <- tmp + B*allStreaks$length[nrow(allStreaks)]
          # tmpEnd <- tmp
        }else{
          # tmpEnd <- tmp
          # numPlaysLeft <- max(0, k - allStreaks$length[nrow(allStreaks)])
          # endPlays <- rbinom(numPlaysLeft, 1, p)
          # if(any(endPlays)){
          #   tmpEnd <- tmp + B
          # }else{
          #   tmpEnd <- tmp - B*(2^k - 1)
          # }
          tmp <- tmp - B*(2^allStreaks$length[nrow(allStreaks)] - 1)
          
          
        }
        
        winnings[j, k] <- tmp
        # winningsEnd[j, k] <- tmpEnd
        
      }
      
      
      
      
    }
    
    blah2 <- (blah > 0)
    goBustProb[i, ] <- colMeans(blah2)
    expectedWinnings[i, ] <- colMeans(winnings)
    # expectedWinningsEnd[i, ] <- colMeans(winningsEnd)
  }
)
goBustProb
expectedWinnings
# expectedWinningsEnd
# goBustProb <- round(goBustProb, 4)
# expectedWinnings <- round(expectedWinnings, 2)
# colnames(goBustProb) <- 1:ncol(goBustProb)
# colnames(expectedWinnings) <- 1:ncol(expectedWinnings)
# write.csv(goBustProb, file = "Output/goBustProb.csv")
# write.csv(expectedWinnings, file = "Output/expectedWinnings.csv")
