past<-read.table("past.tsv",header=T,sep='\t',stringsAsFactors = F)
#plot(Outcome~diff,past)
indexes<-1:nrow(past);
get_model<-function(){
train_set_indexes<-sample(indexes,12)
test_set_indexes<-indexes[!indexes %in% train_set_indexes]
train<-past[sample(train_set_indexes,100,replace = T),3:4];
model <- glm(Outcome ~diff-1,family=binomial(link='logit'),data=train)
}
sample_models<-replicate(5000,get_model()$coefficients[1])
hist(sample_models[abs(sample_models)<1],breaks=seq(-0.2,1.1,0.02),main='K en 5000 modelos',xlab = 'K')
model<-get_model()
summary(model)
ano<-anova(model, test="Chisq")
ano
fitted.results <- predict(model,newdata=past[test_set_indexes,3:4],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != past[test_set_indexes,"Outcome"])
print(paste('Accuracy',1-misClasificError))
diff_range=-15:15;
pvals<-1/(1+exp(-(-15:15*model$coefficients[1])))
test_vals=past[test_set_indexes,3:4]
hits=ifelse(fitted.results==past[test_set_indexes,"Outcome"],1,2)
main_title=paste("K=",round(model$coefficients[1],dig=3)," p-val=",ano$`Pr(>Chi)`[2]);
plot(diff_range,pvals,
     t='l',
     xlab='diferencia de puntos',
     ylab='prob victoria local',
     ylim=c(0,1),
     main=main_title);
points(test_vals,col=hits,pch=19)
library(ROCR)
p <- predict(model,newdata=past[test_set_indexes,3:4],type='response')
pr <- prediction(p, past[test_set_indexes,"Outcome"])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
