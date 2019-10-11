#首先請用runif 產生0-1之間的隨機數值,
#接著寫出一個自訂函數,其功能為模擬一個公正的六面骰子輸出函數的輸出結果為1-6的數值,且機率皆相同。
#請用此函數模擬丟骰子1000次, 並紀錄1-6出現的次數
dice_count <- function(x){
  while(T){
    dice <- runif(x,0,1)
    if(all(dice != 0)){
      break
    }
  }
  summary(as.factor(ceiling(dice*6)))
}
dice_count(1000)

#請讀入hw1_0322.csv檔案
#請撰寫for迴圈計算G1-G5兩兩間的Pearson correlation數值, 建立一個名稱為output的data.frame結構,
#第一個column為Gene A的名字, 第二個column為Gene B的名字, 第三個column為Pearson correlation數值,
#將其儲存為output.csv
Pearson_correlation <- matrix(,(choose(5,2)),3)
colnames(Pearson_correlation) <- c("Gene A","Gene B","Pearson correlation")
t <- 1
hw1_0322.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0322.csv")
for(i in 1:4){
  x <-  paste0("G",as.character(i))
  for(j in (i+1):5){
    y <- paste0("G",as.character(j))
    Pearson_correlation[t,1] <- x
    Pearson_correlation[t,2] <- y
    Pearson_correlation[t,3] <- cor(hw1_0322.csv[x],hw1_0322.csv[y])
    t <- t + 1
  }
}
output <- data.frame(Pearson_correlation)
write.csv(output,"output.csv")

#請讀入hw2_0322.csv檔案
#請將樣本根據Case與Control 分群後計算 G1-G10分別在Additive model, Dominant model, Recessive model 下
#哪個Gene的Fisher exact test p-value 最顯著
hw2_0322.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw2_0322.csv")
dominant <- c()
for(i in 1:10){
  case <- hw2_0322.csv[which(hw2_0322.csv$Type == "Case"),-1:-2]
  control <- hw2_0322.csv[which(hw2_0322.csv$Type == "Control"),-1:-2]
  case <- c(length(which(case[,i] == 0)),length(which(case[,i] != 0)))
  control <- c(length(which(control[,i] == 0)),length(which(control[,i] != 0)))
  dominant[i] <- fisher.test(matrix(c(case,control),2,2))$p.value
}

recessive <- c()
for(i in 1:10){
  case <- hw2_0322.csv[which(hw2_0322.csv$Type == "Case"),-1:-2]
  control <- hw2_0322.csv[which(hw2_0322.csv$Type == "Control"),-1:-2]
  case <- c(length(which(case[,i] == 2)),length(which(case[,i] != 2)))
  control <- c(length(which(control[,i] == 2)),length(which(control[,i] != 2)))
  recessive[i] <- fisher.test(matrix(c(case,control),2,2))$p.value
}

additive <- c()
for(i in 1:10){
  case <- hw2_0322.csv[which(hw2_0322.csv$Type == "Case"),-1:-2]
  control <- hw2_0322.csv[which(hw2_0322.csv$Type == "Control"),-1:-2]
  case <- c(length(which(case[,i] == 0)),length(which(case[,i] == 1)),length(which(case[,i] == 2)))
  control <- c(length(which(control[,i] == 0)),length(which(control[,i] == 1)),length(which(control[,i] == 2)))
  additive[i] <- fisher.test(matrix(c(case,control),3,2))$p.value
}

genetic_fisher <- data.frame(dominant,recessive,additive)
row.names(genetic_fisher) <- (paste0("Gene",1:10))
significant_gene <- c(which.min(genetic_fisher$dominant),which.min(genetic_fisher$recessive),which.min(genetic_fisher$additive))
p_value <- c(min(genetic_fisher$dominant),min(genetic_fisher$recessive),min(genetic_fisher$additive))
final_table <- data.frame(significant_gene,p_value)
row.names(final_table) <- (c("dominant","recessive","additive"))
print(final_table)

#請撰寫一個函數透過迴圈來計算數字階層
for_digital_class <- function(startx,endy){
  t <- 1
  for(i in startx:endy){
    t <- t*i  
  }
  print(t)
}

#請撰寫一個函數透過遞迴方法來計算數字階層
Fibonacci <- function(input){
  if(input == 1){
    return(1)
  }
  else{
    return(input * Fibonacci(input-1))
  }
}



