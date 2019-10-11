#��Ū�Jhw1_0329.csv�A�ñN�˥���Gender�����A�w��G1-G5�i��t-test�A��ܨ䤤�㦳��ۮt��(P < 0.01)����]�i�������R�G
#�H���D��Gender��0��Gender��1���˥��U250�H�A�æA���i��t-test�˩w���x�s��p-value�A���ƤW��Ū�H�����ˤ��˩w�L�{100���A
#���շ��˥��ƴ�b�ɤ���o�{��ۮt�������Ʀ��h�֦��A�N�˩w���G��P < 0.05������۩Τ���۫�Q�ζ��ϤΪ����ϧe�{
#(���O�x�s��output1_pie.JPG��output1_barplot.PNG)
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

#��Ū�Jhw1_0329.csv�A�ñN�˥���Hospital�����A�w��G1-G5�i��anova�˩w�A��ܨ䤤�㦳��ۮt��(P < 0.01)����]�i�������R�G
#�N��]�ƭȧQ�β�Ž�ϧe�{�A�а��X�@�i�ϥ]�t�T�i�l�ϧΡAx�b�����Hospital 1 or 2 or 3�Ay�b�ЩT�w�̤p�Ȭ�7�A�̤j�Ȭ�10�A
#�N�ϧ��x�s��output2.PDF
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

#��Ū�Jhw2_0329.csv�A�ñN�˥���Virus�����A�w��G1-G5�i��wilcoxon rank sum�˩w�A��ܨ䤤�㦳��ۮt��(P < 0.01)����]�i�������R�G
#�Цb�P�@�i�ϧΤW�N�Ӱ�]��Virus�ܼƤ����˥���H���P�Ϊ��Τ��P�C�⪺����Iø�X�A�ЩT�wy�b�̤p�Ȭ�0�A�̤j�Ȭ�15�A�аw��o��s�ƭȥ[�W�j�k�u�A
#�é�W��r������j�k�u�����ιϻ�������s�˥���Virus 0��Virus 1�A�N�ϧ��x�s��output3.bmp (x�b�Х�Fake_index->1:10)
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

#��Ū�Jhw3_0329.csv�A�ñN�˥���Gender�PVirus�����A�w��G1-G5�i��u�ʰj�k�A�å[�JGender�PVirus���椬�@�ζ��A
#��ܨ䤤�㦳��ۮt��(P < 0.01)����]�e�X�U�ϡG�N�ϧ��x�s��output4.pdf
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
