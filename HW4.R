for(i in 1:1000){
  N_size <- 30
  N_bit <- 8
  Gen <- 30
  Population <- matrix(sample(c(1,-1),size = N_size*N_bit, replace = T),N_size,N_bit)
  mut_freq <- 0.25
  Po_value <- 4*Population[,1]+2*Population[,2]+1*Population[,3]+1/2*Population[,4]+1/4*Population[,5]+1/8*Population[,6]+1/16*Population[,7]+1/32*Population[,8]
  fit <- function(input){
    return(2*input^3 - 25*input^2 + 18*input + 3000 - input*sin(input))
  }
  fit_value <- fit(Po_value)
  idx <- 1:N_size
  max_Gen_value <- rep(0,Gen)
  max_Gen_value[1] <- max(fit_value)
  max_Gen_ind <- matrix(0,Gen,N_bit)
  max_Gen_ind[1,] <- Population[which(fit_value == max(fit_value))[1],]
  max_Gen_Po_value <- rep(0,Gen)
  max_Gen_Po_value[1] <- Po_value[which(fit_value == max(fit_value))[1]]
  
  for(now_Gen in 2:Gen){
    child <- matrix(0,N_size,N_bit)
    child[1,] <- Population[which(fit_value == max(fit_value))[1],]
    switch_bit <- sample(N_bit,1)
    child[2,] <- child[1,]
    child[2,switch_bit:N_bit] <- -(child[2,switch_bit:N_bit])
    child_size <- 2
    
    total_wheel <- sum(fit_value)
    select_frequency <- fit_value/total_wheel
    while(child_size < N_size){
      P_idx <- sample(idx,size = 2,replace = F,prob = select_frequency)
      P1 <- Population[P_idx[1],]
      P2 <- Population[P_idx[2],]
      switch_bit <- sample(N_bit,1)
      P1_new <- P1
      P1_new[switch_bit:N_bit] <- P2[switch_bit:N_bit]
      P2_new <- P2
      P2_new[switch_bit:N_bit] <- P1[switch_bit:N_bit]
      if(runif(1,0,1) < mut_freq){
        tar_bit <- sample(N_bit,1)
        P1_new[tar_bit] <- -(P1_new[tar_bit])
      }
      if(runif(1,0,1) < mut_freq){
        tar_bit <- sample(N_bit,1)
        P2_new[tar_bit] <- -(P2_new[tar_bit])
      }
      child[child_size+1,] <- P1_new
      child[child_size+2,] <- P2_new
      child_size <- child_size + 2
    }
    Population <- child
    Po_value <- 4*Population[,1]+2*Population[,2]+1*Population[,3]+1/2*Population[,4]+1/4*Population[,5]+1/8*Population[,6]+1/16*Population[,7]+1/32*Population[,8]
    fit_value <- fit(Po_value)
    max_Gen_value[now_Gen] <- max(fit_value)
    max_Gen_Po_value[now_Gen] <- Po_value[which(fit_value == max(fit_value))[1]]
    max_Gen_ind[now_Gen,] <- Population[which(fit_value == max(fit_value))[1],]
  }
  convergence[i] <- max_Gen_value[Gen]
}
plot(convergence, main = "mut_0.25")
