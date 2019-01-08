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


l.ex <- list(a = list(1:5, LETTERS[1:5]), b = "Z", c = NA)
unlist(l.ex, recursive = FALSE)
unlist(l.ex, recursive = TRUE)

l1 <- list(a = "a", b = 2, c = pi+2i)
unlist(l1) # a character vector
l2 <- list(a = "a", b = as.name("b"), c = pi+2i)
unlist(l2) # remains a list

ll <- list(as.name("sinc"), quote( a + b ), 1:10, letters, expression(1+x))
utils::str(ll)
for(x in ll)
  stopifnot(identical(x, unlist(x)))


for(k in 1:maxNumLosses){
  
  tmp <- 0
  # tmpVec <- vector(mode = "numeric", length = maxNumLosses)
  for(m in 1:nrow(allStreaks)){
    if(allStreaks$length[m] >= k && allStreaks$value[m] == 0){
      tmp <- tmp - B*(2^k - 1)
      # print(tmp)
      print(str_c("m = ", m, ", tmp = ", tmp))
      break
    }
    else if(allStreaks$length[m] < k && allStreaks$value[m] == 0){
      # tmp <- tmp - B*(2^allStreaks$length[m] - 1)
      print(str_c("m = ", m, ", tmp = ", tmp))
      next
    }
    else{
      # print(m)
      tmp <- tmp + B*allStreaks$length[m]
      print(str_c("m = ", m, ", tmp = ", tmp))
    }
  }
  
  winnings[j, k] <- tmp
}