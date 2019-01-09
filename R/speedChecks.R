streaksOfLosses <- allStreaks %>% 
  filter(value == 0) # the sequences that were all losses



numAtLeast <- vector(mode = "numeric", length = maxNumLosses)
for(k in 1:maxNumLosses){
  numAtLeast[k] <- sum(streaksOfLosses$length >= k)
}


tmp <- data.frame(cbind(rep(1:10, each = nrow(streaksOfLosses)), streaksOfLosses$length))
names(tmp) <- c("numLosses", "length")

tmp %>% 
  group_by(numLosses) %>% 
  mutate(out = map_dbl(numLosses, ~sum(length[length >= numLosses])) ) %>% 
  ungroup


sapply(1:maxNumLosses, function(x) sum(streaksOfLosses$length >= x))
map_int(1:maxNumLosses, function(x) sum(streaksOfLosses$length >= x))

library(data.table)
DT <- as.data.table(1:maxNumLosses)
DT[, sum(streaksOfLosses$length >= V1), by = V1]
DT[, .(numAtLeast = sum(streaksOfLosses$length >= V1)), by = V1][,numAtLeast]



microbenchmark::microbenchmark(SAPPLY = sapply(1:maxNumLosses, function(x) sum(streaksOfLosses$length >= x)),
                               LOOP = for(k in 1:maxNumLosses){
                                 sum(streaksOfLosses$length >= k)
                               },
                               DPLYR = map_int(1:maxNumLosses, function(x) sum(streaksOfLosses$length >= x)),
                               DATATABLE = DT[, .(numAtLeast = sum(streaksOfLosses$length >= V1)), by = V1][,numAtLeast])

zxc <- streaksOfLosses$length
microbenchmark::microbenchmark(SAPPLY = sapply(1:maxNumLosses, function(x) sum(zxc >= x)),
                               LOOP = for(k in 1:maxNumLosses){
                                 sum(zxc >= k)
                               },
                               DPLYR = map_int(1:maxNumLosses, function(x) sum(zxc >= x)),
                               DATATABLE = DT[, .(numAtLeast = sum(zxc >= V1)), by = V1][,numAtLeast])


############################################################################

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

j = 1

doSapply <- function(){
  allWinnings <- sapply(1:maxNumLosses, func1, simplify = TRUE)
  winnings[j,] <- allWinnings[1,]
  winningsEnd[j,] <- allWinnings[2,]
}


doLoop <- function(){
  for(k in 1:maxNumLosses){
    
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
    
    winnings[j, k] <- tmp
    winningsEnd[j, k] <- tmpEnd
    
  }
}
microbenchmark::microbenchmark(doSapply(),
                               doLoop())
