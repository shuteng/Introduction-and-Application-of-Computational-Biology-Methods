#請用之前作業撰寫的遞迴費氏數列及迴圈費式數列程式碼，分別跑F(10), F(20), F(30)。請用程式紀錄跑的時間。
Fibonacci <- function(input){
  if(input <= 2){
    return(1)
  }
  else{
    return(Fibonacci(input-1) + Fibonacci(input-2))
  }
}

Fibonacci_loop <- function(input){
  if(input <= 2){
    return(1)
  }
  else{
    sum1 <- 1
    sum2 <- 1
    for(i in 3:input){
      out <- sum1 + sum2
      if(i%%2 == 0){
        sum1 <- out
      }
      else{
        sum2 <- out
      }
    }
    return(out)
  }
}

time_Fib <- function(input){
  start_time <- Sys.time()
  Fibonacci(input)
  print(as.numeric(Sys.time() - start_time))
}
time_Fib_loop <- function(input){
  start_time <- Sys.time()
  Fibonacci_loop(input)
  print(as.numeric(Sys.time() - start_time))
}

Fibonacci_time <- c(time_Fib(10),time_Fib(20),time_Fib(30))
Fibonacci_loop_time <- c(time_Fib_loop(10),time_Fib_loop(20),time_Fib_loop(30))

#請撰寫選擇排序法及氣泡排序法兩個函數，輸出數列為遞增數列。
selection_sort <- function(input){
  sort_se <- c()
  aa <- input
  for(i in 1:length(input)){
    sort_se[i] <- min(aa)
    aa <- aa[-which(aa == min(aa))[1]]
  }
  return(sort_se)
}

bubble_sort <- function(input){
  xx <- input
  for(j in 1:(length(input) - 1)){
    flag <- 0
    for(i in 1:(length(input) - 1)){
      if(xx[i] > xx[i+1]){
        bb <- xx[i+1]
        xx[i+1] <- xx[i]
        xx[i] <- bb
        flag <- 1
      }
    }
    if(flag == 0){
      break
    }
  }
  return(xx)
}

#請讀入hw1_0426.csv檔案，並針對V1, V2, V3,V4,V5使用這兩個函數進行排序，並用程式紀錄跑的時間，
#比較一下兩個方法在input不同時候有甚麼差異
hw1_0426.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0426.csv")
time_selection <- function(input){
  start_time <- as.numeric(Sys.time())
  selection_sort(input)
  print(as.numeric(Sys.time()) - start_time)
}
time_bubble <- function(input){
  start_time <- as.numeric(Sys.time())
  bubble_sort(input)
  print(as.numeric(Sys.time()) - start_time)
}

se_time <- c()
bub_time <- c()
for(k in 1:5){
  se_time[k] <- time_selection(na.omit(hw1_0426.csv[,k]))
  bub_time[k] <- time_bubble(na.omit(hw1_0426.csv[,k]))
}
  

