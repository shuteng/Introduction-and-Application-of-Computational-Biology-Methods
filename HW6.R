#½ÐÅª¤Jhw1_0524.csv
hw1_0524.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0524.csv",sep = ",")

#½Ð°w¹ïDose§Q¥ÎG1¶i¦æ½u©Ê°jÂk¡A¶i¦æ¨â­¿¥æ¤eÅçÃÒ(Two-fold cross-validation)¡C
#§Ú­Ì°²³]¦^Âk¼Ò«¬¹w´ú¥X¨ÓªºDose»P¹ê»Ú¼Æ¾ÚªºDose¬Û®t¦b0.1¥H¤º®É¬°¥¿½T¹w´ú¡A½Ð­pºâ¹w´ú²v
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
#¹w´ú²v100%

#½ÐÅª¤Jhw2_0524.csv
hw2_0524.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw2_0524.csv",sep = ",")

#§Ú­Ì¶i¦æ¤F¤@¦¸·L°}¦C¹êÅç¡A°²³]¤HÃþ¾Ö¦³20000­Ó°ò¦](½s½X¬°1:20000)¡A¨ä¤¤Geneset A¬°Column A©Ò¼Ð°O¡A
#¦b§Ú­Ì§ä¨ìªº100­ÓIdentified genes¤¤Geneset A¦û¤F4­Ó¡A½Ð­pºâ§ä¨ìªº°ò¦]¦bGeneset Aªº¶W´X¦ó¾÷²v
choo_P <- choose(30,4)*choose(19970,96)/choose(20000,100)

#½Ð³z¹L±Æ¦CªkÀH¾÷©â¨ú100­Ó°ò¦]100000¦¸¨Ã´ú¸ÕGeneset AÅãµÛ(p<0.05)ªº¦¸¼Æ¬O? »P³o­Ó¦¸¼Æ¤ñ¸û«á¡A
#§A»{¬°Geneset A»P¹êÅç¬O§_¦³ÅãµÛ¬ÛÃö?
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
#989¦¸
#¦³ÅãµÛ

#½ÐÅª¤JData_file.csv
Data_file.csv <- read.csv("C:\\Users\\Karma\\Desktop\\Data_file.csv",sep = ",",row.names = 1)

# ÀÉ®×²Ä¤@¦æ¬O¼Ë¥»ªº«¬ºA (Cancer, Normal)
#„HÀÉ®×¦@¦³54675­Ó±´°w
#„H½Ð¦Û¥Ñµo´§¿z¿ï¤èªk¿ï¥X§A»{¬°³Ì¦³¥Îªº±´°w¨Ã³z¹LSVM©MCART«Ø¥ß¹w´ú¾¹
#„H³Ì«á½Ð³z¹L¨â­¿¥æ¤¬ÅçÃÒ´ú¸Õ¹w´ú¾¹ªº·Ç½T«×
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
#·Ç½T«×0.83333
SVM_model_2 <- svm(split_selected_2$Type~split_selected_2$Express,kernel="linear")
SVM_predict_2 <- predict(SVM_model_2,split_selected_1[,1])
accuracy_svm_2 <- length(which((SVM_predict_2 == split_selected_1[,2])==T))/60
#·Ç½T«×0.9
CART_model_1 <- rpart(split_selected_1$Type~split_selected_1$Express)
aaa <- data.frame(split_selected_2[,1])
colnames(aaa) <- "Express"
CART_predict_1 <- predict(CART_model_1,aaa,type="class")
accuracy_cart_1 <- length(which((CART_predict_1 == split_selected_2[,2])==T))/60
#·Ç½T«×0.95
CART_model_2 <- rpart(split_selected_2$Type~split_selected_2$Express)
bbb <- data.frame(split_selected_1[,1])
colnames(bbb) <- "Express"
CART_predict_2 <- predict(CART_model_2,bbb,type="class")
accuracy_cart_2 <- length(which((CART_predict_2 == split_selected_1[,2])==T))/60
#·Ç½T«×0.95
