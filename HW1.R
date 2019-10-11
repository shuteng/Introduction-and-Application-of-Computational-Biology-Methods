#請建立一個矩陣並進行Fisher’s exact test，取出p-value數值，及其 95% CI與OR
fisher = matrix(c(1,100,9,91),2,2)
fisher_exact = fisher.test(fisher)
fisher_exact_test = c(fisher_exact$p.value,fisher_exact$conf.int,fisher_exact$estimate)

#請撰寫一個函數，其功能為將輸入的數字乘以2倍後加負號輸出
minus2 = function(num){
  print(num * 2 * -1)
}

#請讀入hw1_0315.csv，計算G2的平均值，並尋找G2的最大及最小值, 並說明各在哪個樣本
#請讀入hw2_0315.csv, 合併兩個檔案並消除重複的數據後輸出為output1_0315.csv
hw1_0315.csv = read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0315.csv")
hw2_0315.csv = read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw2_0315.csv")
hw1_mean_max_min = c(mean(hw1_0315.csv$G2),max(hw1_0315.csv$G2),min(hw1_0315.csv$G2))
hw2_0315.csv_new = hw2_0315.csv[,-1:-4]
bind_hw1_2 = cbind(hw1_0315.csv,hw2_0315.csv_new)
write.csv(bind_hw1_2,"output1_0315.csv")

#請讀入hw3_0315.csv，請除去NA值，將其合併後輸出為output2_0315.csv
hw3_0315.csv = read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw3_0315.csv")
hw3_rmNA <- cbind(hw3_0315.csv[which(hw3_0315.csv$Hospital != "NA"),1:4],hw3_0315.csv[which(hw3_0315.csv$G1 != "NA"),5:9])
write.csv(hw3_rmNA,"output2_0315.csv")

#請讀入hw1_0315.csv，請建立一個vector 儲存G1-G5依Virus變數分隔樣本後的t-test p-value, 並以逗點分隔後輸出為output2.txt
virus_vector0 <- matrix()
virus_vector0 = hw1_0315.csv[which(hw1_0315.csv$Virus == 0),5:9]
virus_vector1 <- matrix()
virus_vector1 = hw1_0315.csv[which(hw1_0315.csv$Virus == 1),5:9]
virus_pvalue = c()
for(i in 1:5){
  x = t.test(virus_vector0[,i],virus_vector1[,i])
  virus_pvalue[i] = x$p.value
}
write(virus_pvalue,"output2.txt", sep = ",")

#在以Hospital, Gender, Virus 三個變數同時進行線性回歸下, G5是否與任一變數有顯著相關
G5_lm <- summary(lm(G5~Hospital+Gender+Virus,data = hw1_0315.csv))

#將G2針對Hospital變數進行anova分析，兩者是否有顯著相關 (注意Hospital變數為factor)
G2_anova <- summary(aov(hw1_0315.csv$G2~as.factor(hw1_0315.csv$Hospital),data = hw1_0315.csv))

