#�Ыإߤ@�ӯx�}�öi��Fisher��s exact test�A���Xp-value�ƭȡA�Ψ� 95% CI�POR
fisher = matrix(c(1,100,9,91),2,2)
fisher_exact = fisher.test(fisher)
fisher_exact_test = c(fisher_exact$p.value,fisher_exact$conf.int,fisher_exact$estimate)

#�м��g�@�Ө�ơA��\�ର�N��J���Ʀr���H2����[�t����X
minus2 = function(num){
  print(num * 2 * -1)
}

#��Ū�Jhw1_0315.csv�A�p��G2�������ȡA�ôM��G2���̤j�γ̤p��, �û����U�b���Ӽ˥�
#��Ū�Jhw2_0315.csv, �X�֨���ɮרî������ƪ��ƾګ��X��output1_0315.csv
hw1_0315.csv = read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw1_0315.csv")
hw2_0315.csv = read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw2_0315.csv")
hw1_mean_max_min = c(mean(hw1_0315.csv$G2),max(hw1_0315.csv$G2),min(hw1_0315.csv$G2))
hw2_0315.csv_new = hw2_0315.csv[,-1:-4]
bind_hw1_2 = cbind(hw1_0315.csv,hw2_0315.csv_new)
write.csv(bind_hw1_2,"output1_0315.csv")

#��Ū�Jhw3_0315.csv�A�а��hNA�ȡA�N��X�֫��X��output2_0315.csv
hw3_0315.csv = read.csv("https://ceiba.ntu.edu.tw/course/c9c950/content/hw3_0315.csv")
hw3_rmNA <- cbind(hw3_0315.csv[which(hw3_0315.csv$Hospital != "NA"),1:4],hw3_0315.csv[which(hw3_0315.csv$G1 != "NA"),5:9])
write.csv(hw3_rmNA,"output2_0315.csv")

#��Ū�Jhw1_0315.csv�A�Ыإߤ@��vector �x�sG1-G5��Virus�ܼƤ��j�˥��᪺t-test p-value, �åH�r�I���j���X��output2.txt
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

#�b�HHospital, Gender, Virus �T���ܼƦP�ɶi��u�ʦ^�k�U, G5�O�_�P���@�ܼƦ���۬���
G5_lm <- summary(lm(G5~Hospital+Gender+Virus,data = hw1_0315.csv))

#�NG2�w��Hospital�ܼƶi��anova���R�A��̬O�_����۬��� (�`�NHospital�ܼƬ�factor)
G2_anova <- summary(aov(hw1_0315.csv$G2~as.factor(hw1_0315.csv$Hospital),data = hw1_0315.csv))
