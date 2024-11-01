#Box Plot
library(data.table)
library(ROCR)
library(pROC)
library(RNOmni)
library(caret)
par(mfrow=c(2,2)) # for multiple plots
df <- fread ("/Volumes/ATUL_6TB/Work/Projects/Nicholas_Ashton/Data_SAA-Ctrl_vs_Atypical.txt")
par(cex.axis = 1.5)
#av <- aov(CRH__Oncology_o2csf__NPX ~ Group, data = df)
#summary (av)
boxplot(CRH__Oncology_o2csf__NPX ~ Group, data = df, lwd = 2, ylab = 'CSF CRH', notch = TRUE, cex.lab = 1.5, main = "A) SAA- CTR vs Atypical PS", cex.main = 1.5)
stripchart(CRH__Oncology_o2csf__NPX ~ Group, vertical = TRUE, data = df, 
           method = "jitter", add = TRUE, pch = 20, col = c('#E69F00', '#CC79A7'), cex = 2, jitter = 0.3)
mtext("BioFINDER-2 Cohort", side = 3, line = -1.75, outer = TRUE, cex = 1.75, col = '#D55E00')

df_1 <- fread ("/Volumes/ATUL_6TB/Work/Projects/Nicholas_Ashton/Data_SAA-Ctrl_vs_MSA.txt")
par(cex.axis=1.5)
boxplot(CRH__Oncology_o2csf__NPX ~ Group, data = df_1, lwd = 2, ylab = 'CSF CRH', notch = TRUE, cex.lab = 1.5, main = "B) SAA- CTR vs MSA", cex.main = 1.5)
stripchart(CRH__Oncology_o2csf__NPX ~ Group, vertical = TRUE, data = df_1, 
           method = "jitter", add = TRUE, pch = 20, col = c('#E69F00', '#CC79A7'), cex = 2, jitter = 0.3)

df_2 <- fread ("/Volumes/ATUL_6TB/Work/Projects/Nicholas_Ashton/Data_SAA-Ctrl_vs_PSP.txt")
par(cex.axis=1.5)
boxplot(CRH__Oncology_o2csf__NPX ~ Group, data = df_2, lwd = 2, ylab = 'CSF CRH', notch = TRUE, cex.lab = 1.5, main = "C) SAA- CTR vs PSP", cex.main = 1.5)
stripchart(CRH__Oncology_o2csf__NPX ~ Group, vertical = TRUE, data = df_2, 
           method = "jitter", add = TRUE, pch = 20, col = c('#E69F00', '#CC79A7'), cex = 2, jitter = 0.3)
