# Do the House of Commons politicians fall into clusters when voting on Brexit, where the politicians in each cluster tend to have similar voting behaviour?

#load packages
library(mltools)
library(data.table)
library (dplyr)
library(BayesLCA)
library(mixtools)
library(flexmix)
library(BayesLCA)
library(janitor)
library(viridis)
library(mixtools)
library (foreign)
library (MASS)
library (scatterplot3d)
library (poLCA)
library (readxl)
library(ggplot2)


#process data
dat1<-as.data.frame(divisions)
rownames(dat1)<- c()
str(dat1)

#convert characters to factors
dat1 <- as.data.frame(unclass(dat1),stringsAsFactors=TRUE)
str(dat1)
colnames(dat1)<-c('V1','V2','V3','V4','V5','V6','V7','V8','V9', 'V10','V11','V12','V13','V14')

#one hot encode data
newdata <- one_hot(as.data.table(dat1),stringsAsFactors=TRUE)
str(newdata)

#remove no vote
select(-contains(c("no_vote")))
remove<-grep("no_vote", colnames(newdata))

newdata2<-newdata[, -remove]
newdata[,-3]

#convert to matrix
newdata2<-as.matrix(newdata2)

#Fit a G component mixture model
fit<-multmixEM(as.matrix(dat1[,-1]),k=2)
summary(fit)
print(fit)
fit$lambda

#find optimum G
llvec <- rep(NA,50)
bicvec <- rep(NA,50)
fitlist <- list()
N<-length(newdata2)

#Fit a G component mixture model (G=1,...,10) and find optimum G
for (g in 2:50)
{
  fit <- multmixEM(newdata2,k=g)
  print(summary(fit))
  fitlist[[g]] <- fit
  llvec[g] <- fit$logl 
  bicvec[g] <- 2*fit$logl - log(N)*((g-1)+g)
}

#make figure
png(file="Figure1_BIC.png",width=2000,height = 1000, units= "px", res=200)
par(mar=c(5,5,4,4))
par(mfrow=c(1,2)) 

#plot the results
plot(bicvec, col="dodgerblue", ylab="BIC", xlab="Number of Model Components", )
abline(col="blue",v=6)
plot(llvec, col="magenta", ylab="Loglikelihood", xlab="Number of Model Components")
abline(col="blue",v=6)
dev.off()

#fit latent class model
fit3<- blca.em(newdata2, 10, restarts=25)
print(fit3)

#make figure
plot(fit3$classprob, col="red", ylab="Probability", xlab="Group")
plot(fit3)
plot.blca(fit3)

fit3$classprob

# summaries of the clusters 

#process data
#dat1<-as.data.frame(divisions)
#rownames(dat1)<- c()

#convert characters to factors
#dat1 <- as.data.frame(unclass(dat1),stringsAsFactors=TRUE)
#str(dat1)
#colnames(dat1)<-c('V1','V2','V3','V4','V5','V6','V7','V8','V9', 'V10','V11','V12','V13','V14')

# Latent class models with one (loglinear independence) to 6 classes
f1<-cbind(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14)~1
fit3 <- poLCA(f1,dat1,nclass=6, graphs=FALSE) 

#Vote level probabilities and se - process data
se<-fit3$probs.se
p<- fit3$probs  #extract probabilities and se
table1 <- as.data.frame<-fit3$probs
write.csv(table1, file="table1.csv")
table2 <- as.data.frame<-fit3$probs.se
write.csv(table2, file="table2.csv")

#population level probabilities and se
sePop<-fit3$P.se
PPop<-fit3$P

dev.off()

#make heat maps for vote level probabilities and se
table1 <- read_excel("table1.xlsx", sheet = "Sheet5")
YesT<-table1
T1<-as.matrix((YesT))

table2 <- read_excel("table1.xlsx", sheet = "Sheet4")
absentT<-table2
T2<-as.matrix((absentT[,-1]))

#make figure 3a
png(file="Figure3a_Heatmap_Probability_Yes.png",width=1100,height = 500, units= "px", res=110)
par(mar=c(1,1,1,9))
heatmap(t(T1), Rowv=NA, Colv=NA, col = viridis(256),scale="none",xlab="Class", keep.dendro = TRUE,margins=c(5,7))
legend(x="right",y=NULL, legend=c("0", "0.25","0.50","0.75", "1"), bty="n",title="Probability of Yes Vote", cex=.8, 
       fill=colorRampPalette(viridis(256))(5))
dev.off()

#make figure 3b
png(file = "Figure3b_Heatmap_Probability_Absent.png", width = 1100, height = 500, units = "px", res = 110) #make hi res file
par(mar=c(1,1,1,9))
heatmap(t(T2), Rowv=NA, Colv=NA, col = viridis(256),scale="none",xlab="Class", keep.dendro = TRUE,margins=c(5,7))
legend(x="right", legend=c("0", "0.25","0.50","0.75", "1"), title="Probability of Being Absent", cex=.8, bty="n",
       fill=colorRampPalette(viridis(256))(5))
dev.off()

#heatmaps of se
table3 <- read_excel("table2.xlsx", sheet = "absent")
absentTse<-table3
T3<-as.matrix((absentTse[,-1]))

table4 <- read_excel("table2.xlsx", sheet = "yes")
yesTse<-table4
T4<-as.matrix((yesTse[,-1]))

#make figure
png(file = "Figure4a_Heatmap_SE_Probability_Yes.png", width = 1100, height = 500, units = "px", res = 110) #make hi res file
par(mar=c(1,1,1,9))
heatmap(t(T3), Rowv=NA, Colv=NA, col = viridis(256),scale="none",xlab="Class", keep.dendro = TRUE,margins=c(4,9))
legend(x="right", legend=c("0", "0.25","0.50","0.75", "1"),bty="n", title="SE of Probability of Being Absent", cex=.8, 
       fill=colorRampPalette(viridis(256))(5))
dev.off()

#make figure
png(file = "Figure4b_Heatmap_SE_Probability_Absent.png", width = 1100, height = 500, units = "px", res = 110) #make hi res file
par(mar=c(1,1,1,9))
heatmap(t(T4), Rowv=NA, Colv=NA, col = viridis(256),scale="none",xlab="Class", keep.dendro = TRUE,margins=c(4,8))
legend(x="right", legend=c("0", "0.25","0.50","0.75", "1"), bty="n",title="SE of Probability of Voting Yes", cex=.8, 
       fill=colorRampPalette(viridis(256))(5))
dev.off()

#plots of population level probabilities and se
sePop<-fit3$P.se
PPop<-fit3$P
table5<-data.frame(cbind(sePop,PPop))
table5$Class<-1:6
table5$Class<-factor(table5$Class)
col=viridis(6,option="E")

#make figure
tiff(file = "temp.tiff", width = 3200, height = 3200, units = "px", res = 800) #make hi res file
p<-ggplot(table5, aes(x=Class, y=PPop, fill=Class))+ 
  geom_bar(stat="identity", color=col, 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=PPop-sePop, ymax=PPop+sePop), width=.2,
                position=position_dodge(.9)) +
  labs(x="Class", y = "Probability of Belonging to Class g\n")+
  theme_classic() +
  scale_fill_viridis(discrete=TRUE,option="D")
p

