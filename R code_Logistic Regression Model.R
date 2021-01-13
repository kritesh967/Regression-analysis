install.packages("ROCR")
install.packages("pscl")
library(ROCR)
library(pscl)



claimants <- read.csv("claimants.csv", header=TRUE)

#CASENUM Case number to identify the claim
#ATTORNEY Whether the claimant is represented by an attorney (=0 if yes and =1 if no)
#CLMSEX Claimant's gender (=0 if male and =1 if female)
#CLMINSUR Whether or not the driver of the claimant's vehicle was insured (=0 if yes, =1 if no)
#SEATBELT Whether or not the claimant was wearing a seatbelt/child restraint (=0 if no, =1 if yes)
#CLMAGE Claimant's age
#LOSS The claimant's total economic loss (in thousands)


# Linear Regression

fit=lm(ATTORNEY ~ factor(CLMSEX) + factor(CLMINSUR) + factor(SEATBELT)	+ CLMAGE + LOSS,data=claimants)
summary(fit)

plot(fit)


# Logistic Regression

logit=glm(ATTORNEY ~ factor(CLMSEX) + factor(CLMINSUR) + factor(SEATBELT) + CLMAGE + LOSS,family= "binomial",data=claimants)
summary(logit)


# Confusion Matrix Table

prob=predict(logit,claimants,type = 'response')
confusion<-table(prob>0.5,claimants$ATTORNEY)
rownames(confusion) <- c("0", "1")
confusion
see<- as.data.frame(prob)

# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy

# ROC Curve and Area under the curve

predictTest <- data.frame("Probability"=predict(logit,claimants, type = 'response'))
RCRTest <- prediction(predictTest$Probability,claimants$ATTORNEY) 
ROCRTestPerf <- performance(RCRTest,"tpr","fpr")
plot(ROCRTestPerf,main="ROC Curve")
auc = performance(RCRTest,"auc")
auc <- paste(c("AUC ="),round(as.numeric(performance(RCRTest,"auc")@y.values),digits=2),sep="")
legend("topleft",auc, bty="n")

