#請讀入hw1_0329.csv，並將樣本依Gender分類，針對G1-G5進行t-test，選擇其中具有顯著差異(P < 0.01)的基因進行後續分析：
#隨機挑選Gender為0及Gender為1的樣本各250人，並再次進行t-test檢定後儲存其p-value，重複上次讀隨機取樣及檢定過程100次，
#測試當樣本數減半時仍能發現顯著差異的次數有多少次，將檢定結果依P < 0.05分為顯著或不顯著後利用圓餅圖及長條圖呈現
#(分別儲存為output1_pie.JPG及output1_barplot.PNG)
hw1_0329.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0329.csv")
Gender0 <- hw1_0329.csv[which(hw1_0329.csv$Gender == 0),]
Gender1 <- hw1_0329.csv[which(hw1_0329.csv$Gender == 1),]
Gender_t_test <- c()
for(i in 1:5){
  x <- paste0("G",i)
  Gender_t_test[i] <- t.test(Gender0[,x],Gender1[,x])[3]
}
#G4 < 0.01
G4_Gender_t_test <- c()
for(i in 1:100){
  #Gender0_250 <- Gender0[rank(runif(500,0,1))[1:250],]
  #Gender1_250 <- Gender1[rank(runif(500,0,1))[1:250],]
  Gender0_250 <- Gender0[sample(1:500,250),]
  Gender1_250 <- Gender1[sample(1:500,250),]
  G4_Gender_t_test[i] <- t.test(Gender0_250$G4,Gender1_250$G4)[3]
}
length(G4_Gender_t_test[which(G4_Gender_t_test < 0.05)])
length(G4_Gender_t_test[which(G4_Gender_t_test > 0.05)])
picture <- data.frame(c("p<0.05","p=>0.05"),c(length(G4_Gender_t_test[which(G4_Gender_t_test <= 0.05)]),length(G4_Gender_t_test[which(G4_Gender_t_test > 0.05)])))
colnames(picture) <- c("p-value","count")
jpeg(filename = "output1_pie.JPG")
pie(picture[,2],labels = c("p<0.05","p=>0.05"))
dev.off()
png(filename = "output1_barplot.PNG")
text(barplot(picture[,2],names.arg = picture[,1],xlab = "p-value",ylab = "count",col = blues9,ylim = c(0,80)),picture[,2],picture[,2])
dev.off()

#請讀入hw1_0329.csv，並將樣本依Hospital分類，針對G1-G5進行anova檢定，選擇其中具有顯著差異(P < 0.01)的基因進行後續分析：
#將基因數值利用盒鬚圖呈現，請做出一張圖包含三張子圖形，x軸請顯示Hospital 1 or 2 or 3，y軸請固定最小值為7，最大值為10，
#將圖形儲存為output2.PDF
hospital1 <- hw1_0329.csv[which(hw1_0329.csv$Hospital == 1),]
hospital2 <- hw1_0329.csv[which(hw1_0329.csv$Hospital == 2),]
hospital3 <- hw1_0329.csv[which(hw1_0329.csv$Hospital == 3),]
hospital_anova <- c()
for(i in 1:5){
  x <- paste0("G",i)
  hospital_anova[i] <- summary(aov(hw1_0329.csv[,x]~as.factor(hw1_0329.csv$Hospital)))
}
#G4 < 0.01
G4_hospital <- list(hospital1$G4,hospital2$G4,hospital3$G4)
names(G4_hospital) <- c("hospital1","hospital2","hospital3")
pdf(file = "output2.PDF")
boxplot(G4_hospital,ylim = c(7,10))
dev.off()

#請讀入hw2_0329.csv，並將樣本依Virus分類，針對G1-G5進行wilcoxon rank sum檢定，選擇其中具有顯著差異(P < 0.01)的基因進行後續分析：
#請在同一張圖形上將該基因對Virus變數分類樣本後以不同形狀及不同顏色的資料點繪出，請固定y軸最小值為0，最大值為15，請針對這兩群數值加上迴歸線，
#並放上文字說明其迴歸線公式及圖說說明兩群樣本為Virus 0或Virus 1，將圖形儲存為output3.bmp (x軸請用Fake_index->1:10)
hw2_0329.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw2_0329.csv")
virus0 <- hw2_0329.csv[which(hw2_0329.csv$Virus == 0),]
virus1 <- hw2_0329.csv[which(hw2_0329.csv$Virus == 1),]
virus_wilcox.test <- c()
for(i in 1:5){
  x <- paste0("G",i)
  virus_wilcox.test[i] <- wilcox.test(virus0[,x],virus1[,x])[3]
}
#G3 < 0.01
bmp(filename = "output3.bmp")
plot(virus0$G3,ylim = c(0,15),ylab = "value",col = "red",pch = 1)
abline(lm(virus0$G3~c(1:10)),lty = 3)
text(3,12,paste0("y = ",round(lm(virus0$G3~c(1:10))$coefficients[2],4),"x"," + ",round(lm(virus0$G3~c(1:10))$coefficients[1],4)))
points(virus1$G3,,col = "blue",pch = 2)
abline(lm(virus1$G3~c(1:10)),lty = 4)
text(3,5,paste0("y = ",round(lm(virus1$G3~c(1:10))$coefficients[2],4),"x"," + ",round(lm(virus1$G3~c(1:10))$coefficients[1],4)))
legend(7.3,15,legend = c("virus0_G3","virus1_G3"),pch = c(1,2),col = c("red","blue"))
dev.off()

#請讀入hw3_0329.csv，並將樣本依Gender與Virus分類，針對G1-G5進行線性迴歸，並加入Gender與Virus的交互作用項，
#選擇其中具有顯著差異(P < 0.01)的基因畫出下圖：將圖形儲存為output4.pdf
hw3_0329.csv <- read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw3_0329.csv")
gender0_virus0 <- hw3_0329.csv[which(hw3_0329.csv$Gender == 0 & hw3_0329.csv$Virus == 0),]
gender0_virus1 <- hw3_0329.csv[which(hw3_0329.csv$Gender == 0 & hw3_0329.csv$Virus == 1),]
gender1_virus0 <- hw3_0329.csv[which(hw3_0329.csv$Gender == 1 & hw3_0329.csv$Virus == 0),]
gender1_virus1 <- hw3_0329.csv[which(hw3_0329.csv$Gender == 1 & hw3_0329.csv$Virus == 1),]
Gender_Virus <- cbind(hw3_0329.csv$Gender,hw3_0329.csv$Virus,(hw3_0329.csv$Gender*hw3_0329.csv$Virus))
lm_G_Gender_virus <- c()
for(i in 1:5){
  x <- paste0("G",i)
  lm_G_Gender_virus[i] <- summary(lm(hw3_0329.csv[,x]~Gender_Virus))$coefficients[4,4]
}
#G1 < 0.01
pdf(file = "output4.pdf")
plot(c(mean(gender0_virus0$G1),mean(gender0_virus1$G1)), main="Interaction Effect", col = "red", pch = 2, ylim = c(5,6), xlim = c(0,3), xlab = "Virus Infection", ylab = "Gene Expression", xaxt = "n")
axis(1, at=c(1,2), labels=c(0,1))
lines(c(mean(gender0_virus0$G1),mean(gender0_virus1$G1)), col = "red")
points(c(mean(gender1_virus0$G1),mean(gender1_virus1$G1)),col = "blue",pch = 1)
lines(c(mean(gender1_virus0$G1),mean(gender1_virus1$G1)),col = "blue")
legend(0,6,legend = c("Gender 1","Gender 0"),pch = c(1,2),col = c("blue","red"))
dev.off()

