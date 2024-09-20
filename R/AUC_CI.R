#Plotting ROC Curve
library(data.table)
library(ROCR)
library(pROC)
df <- fread ("/Volumes/ATUL_6TB/Work/Projects/PD_CRH/Data_SAA-Ctrl_vs_SAA+LBD.txt")
modeldata <- glm (Diagnosis ~ 1, family=binomial (link = 'logit'), data = df)
N_P <- df$CRH__Oncology_o2csf__NPX
model <- glm (Diagnosis ~ N_P + age + gender_baseline_variable, data = df, family = binomial (link = 'logit'))
obs = model$y
pred = model$fitted.values
auc (obs, pred)
ci.auc (obs, pred)
aa <- roc (obs, pred)
best.coords <- coords(aa, "best", best.method="youden")
