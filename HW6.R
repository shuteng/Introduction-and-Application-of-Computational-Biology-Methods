#請讀入hw1_0524.csv
hw1_0524.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0524.csv",sep = ",")

#請針對Dose利用G1進行線性迴歸，進行兩倍交叉驗證(Two-fold cross-validation)。
#我們假設回歸模型預測出來的Dose與實際數據的Dose相差在0.1以內時為正確預測，請計算預測率
split_hw1 <- split(hw1_0524.csv, sample(rep(1:2, 50)))
split_hw1_1 <- data.frame(split_hw1[1])
colnames(split_hw1_1) <- c("Sample","Dose","G1")
split_hw1_2 <- data.frame(split_hw1[2])
colnames(split_hw1_2) <- c("Sample","Dose","G1")
linear_reg_1 <- lm(Dose~G1,data = split_hw1_1)
linear_reg_2 <- lm(Dose~G1,data = split_hw1_2)
G1_1 <- data.frame(split_hw1_1$G1)
G1_2 <- data.frame(split_hw1_2$G1)
colnames(G1_1) <- "G1"
colnames(G1_2) <- "G1"
pred_G1_2_DOSE <- predict(linear_reg_1,G1_2)
pred_G1_1_DOSE <- predict(linear_reg_2,G1_1)
length(which(abs(pred_G1_2_DOSE - split_hw1_2$Dose) <= 0.1))/length(split_hw1_2$Dose)
length(which(abs(pred_G1_1_DOSE - split_hw1_1$Dose) <= 0.1))/length(split_hw1_1$Dose)
#預測率100%

#請讀入hw2_0524.csv
hw2_0524.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw2_0524.csv",sep = ",")

#我們進行了一次微陣列實驗，假設人類擁有20000個基因(編碼為1:20000)，其中Geneset A為Column A所標記，
#在我們找到的100個Identified genes中Geneset A佔了4個，請計算找到的基因在Geneset A的超幾何機率
choo_P <- choose(30,4)*choose(19970,96)/choose(20000,100)

#請透過排列法隨機抽取100個基因100000次並測試Geneset A顯著(p<0.05)的次數是? 與這個次數比較後，
#你認為Geneset A與實驗是否有顯著相關?
Identified_genes <- hw2_0524.csv$Identified.genes
Geneset_A <- hw2_0524.csv$Geneset.A
testGSA_Iden <- c()
choo_P_test <- c()
for(i in 1:100000){
  test_Iden <- sample(1:20000,100,replace = F)
  testGSA_Iden[i] <- length(intersect(Geneset_A,test_Iden))
  c <- testGSA_Iden[i]
  choo_P_test[i] <- choose(30,c)*choose(19970,(100 - c))/choose(20000,100)
}
length(which(choo_P_test < 0.05))
#989次
#有顯著

#請讀入Data_file.csv
Data_file.csv <- read.csv("C:\\Users\\Karma\\Desktop\\Data_file.csv",sep = ",",row.names = 1)

# 檔案第一行是樣本的型態 (Cancer, Normal)
#�H檔案共有54675個探針
#�H請自由發揮篩選方法選出你認為最有用的探針並透過SVM和CART建立預測器
#�H最後請透過兩倍交互驗證測試預測器的準確度
Data_file_cancer <- Data_file.csv[,1:60]
Data_file_normal <- Data_file.csv[,-1:-60]
fold <- c()
for(i in 1:54675){
  if(sum(Data_file_cancer[i,])/sum(Data_file_normal[i,]) < 1){
    fold[i] <- 1/(sum(Data_file_cancer[i,])/sum(Data_file_normal[i,]))
  }
  else{
    fold[i] <- sum(Data_file_cancer[i,])/sum(Data_file_normal[i,])
  }
}
probe <- which(fold == max(fold))
#37892_at

selected <- Data_file.csv[probe,]
a <- sample(rep(1:60),30)
b <- sample(rep(61:120),30)
split_1 <- rbind(t(selected[,a]),t(selected[,b]))
split_selected_1 <- data.frame(split_1,gsub("[[:punct:]][0-9]*",replacement="",row.names(split_1)))
split_2 <- (t(selected[,-c(a,b)]))
split_selected_2 <- data.frame(split_2,gsub("[[:punct:]][0-9]*",replacement="",row.names(split_2)))
colnames(split_selected_1) <- c("Express","Type")
colnames(split_selected_2) <- c("Express","Type")
library(e1071)
library(rpart)
SVM_model_1 <- svm(split_selected_1$Type~split_selected_1$Express,kernel="linear")
SVM_predict_1 <- predict(SVM_model_1,split_selected_2[,1])
accuracy_svm_1 <- length(which((SVM_predict_1 == split_selected_2[,2])==T))/60
#準確度0.83333
SVM_model_2 <- svm(split_selected_2$Type~split_selected_2$Express,kernel="linear")
SVM_predict_2 <- predict(SVM_model_2,split_selected_1[,1])
accuracy_svm_2 <- length(which((SVM_predict_2 == split_selected_1[,2])==T))/60
#準確度0.9
CART_model_1 <- rpart(split_selected_1$Type~split_selected_1$Express)
aaa <- data.frame(split_selected_2[,1])
colnames(aaa) <- "Express"
CART_predict_1 <- predict(CART_model_1,aaa,type="class")
accuracy_cart_1 <- length(which((CART_predict_1 == split_selected_2[,2])==T))/60
#準確度0.95
CART_model_2 <- rpart(split_selected_2$Type~split_selected_2$Express)
bbb <- data.frame(split_selected_1[,1])
colnames(bbb) <- "Express"
CART_predict_2 <- predict(CART_model_2,bbb,type="class")
accuracy_cart_2 <- length(which((CART_predict_2 == split_selected_1[,2])==T))/60
#準確度0.95
