#�����Х�runif ����0-1�������H���ƭ�,
#���ۼg�X�@�Ӧۭq���,��\�ର�����@�Ӥ�����������l��X��ƪ���X���G��1-6���ƭ�,�B���v�ҬۦP�C
#�ХΦ���Ƽ������l1000��, �ì���1-6�X�{������
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

#��Ū�Jhw1_0322.csv�ɮ�
#�м��gfor�j��p��G1-G5��ⶡ��Pearson correlation�ƭ�, �إߤ@�ӦW�٬�output��data.frame���c,
#�Ĥ@��column��Gene A���W�r, �ĤG��column��Gene B���W�r, �ĤT��column��Pearson correlation�ƭ�,
#�N���x�s��output.csv
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

#��Ū�Jhw2_0322.csv�ɮ�
#�бN�˥��ھ�Case�PControl ���s��p�� G1-G10���O�bAdditive model, Dominant model, Recessive model �U
#����Gene��Fisher exact test p-value �����
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

#�м��g�@�Ө�Ƴz�L�j��ӭp��Ʀr���h
for_digital_class <- function(startx,endy){
  t <- 1
  for(i in startx:endy){
    t <- t*i  
  }
  print(t)
}

#�м��g�@�Ө�Ƴz�L���j��k�ӭp��Ʀr���h
Fibonacci <- function(input){
  if(input == 1){
    return(1)
  }
  else{
    return(input * Fibonacci(input-1))
  }
}


