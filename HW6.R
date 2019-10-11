#��Ū�Jhw1_0524.csv
hw1_0524.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0524.csv",sep = ",")

#�аw��Dose�Q��G1�i��u�ʰj�k�A�i��⭿��e����(Two-fold cross-validation)�C
#�ڭ̰��]�^�k�ҫ��w���X�Ӫ�Dose�P��ڼƾڪ�Dose�ۮt�b0.1�H���ɬ����T�w���A�Эp��w���v
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
#�w���v100%

#��Ū�Jhw2_0524.csv
hw2_0524.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw2_0524.csv",sep = ",")

#�ڭ̶i��F�@���L�}�C����A���]�H���֦�20000�Ӱ�](�s�X��1:20000)�A�䤤Geneset A��Column A�ҼаO�A
#�b�ڭ̧�쪺100��Identified genes��Geneset A���F4�ӡA�Эp���쪺��]�bGeneset A���W�X����v
choo_P <- choose(30,4)*choose(19970,96)/choose(20000,100)

#�гz�L�ƦC�k�H�����100�Ӱ�]100000���ô���Geneset A���(p<0.05)�����ƬO? �P�o�Ӧ��Ƥ����A
#�A�{��Geneset A�P����O�_����۬���?
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
#989��
#�����

#��Ū�JData_file.csv
Data_file.csv <- read.csv("C:\\Users\\Karma\\Desktop\\Data_file.csv",sep = ",",row.names = 1)

# �ɮײĤ@��O�˥������A (Cancer, Normal)
#�H�ɮצ@��54675�ӱ��w
#�H�Цۥѵo���z���k��X�A�{���̦��Ϊ����w�óz�LSVM�MCART�إ߹w����
#�H�̫�гz�L�⭿�椬���Ҵ��չw�������ǽT��
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
#�ǽT��0.83333
SVM_model_2 <- svm(split_selected_2$Type~split_selected_2$Express,kernel="linear")
SVM_predict_2 <- predict(SVM_model_2,split_selected_1[,1])
accuracy_svm_2 <- length(which((SVM_predict_2 == split_selected_1[,2])==T))/60
#�ǽT��0.9
CART_model_1 <- rpart(split_selected_1$Type~split_selected_1$Express)
aaa <- data.frame(split_selected_2[,1])
colnames(aaa) <- "Express"
CART_predict_1 <- predict(CART_model_1,aaa,type="class")
accuracy_cart_1 <- length(which((CART_predict_1 == split_selected_2[,2])==T))/60
#�ǽT��0.95
CART_model_2 <- rpart(split_selected_2$Type~split_selected_2$Express)
bbb <- data.frame(split_selected_1[,1])
colnames(bbb) <- "Express"
CART_predict_2 <- predict(CART_model_2,bbb,type="class")
accuracy_cart_2 <- length(which((CART_predict_2 == split_selected_1[,2])==T))/60
#�ǽT��0.95