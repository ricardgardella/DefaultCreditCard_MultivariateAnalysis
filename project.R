#database link: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients


#####LIBRARIES######
if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(mice)) install.packages("mice")
if(!require(mclust)) install.packages("mclust") #Nice clustering library, guide: http://rstudio-pubs-static.s3.amazonaws.com/154174_78c021bc71ab42f8add0b2966938a3b8.html
if(!require(plyr)) install.packages("plyr") #"Library for the creation of the new variables."
if(!require(ggthemes))install.packages("ggthemes") # visualization
if(!require(scales))install.packages("scales") # visualization
if(!require(corrplot))install.packages("corrplot") # plots
if(!require(FactoMineR)) install.packages("fpc")
if(!require(cluster)) install.packages("cluster")
if(!require(mice)) install.packages("mice")

set.seed(101)
# Setting working directory as path of current file
setwd(dirname(getActiveDocumentContext()$path))
####LOAD ENVIROMENT#####
load("Default_dataset_preprocessed.Rdata")
#######BEGIN PREPROCESED#######
Default_Dataset <- read.table("default.csv", header=TRUE, na.strings="?", sep=";") #Importing the data set
Default_Dataset$ID <- NULL #Prescindible

#####DESCRIPTION OF THE DATA SET########
# ID: ID of each client
# LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit
# SEX: Gender (1=male, 2=female)
# EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
# MARRIAGE: Marital status (1=married, 2=single, 3=others)
# AGE: Age in years
# PAY_0: Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, ... 8=payment delay for eight months, 9=payment delay for nine months and above)
# PAY_2: Repayment status in August, 2005 (scale same as above)
# PAY_3: Repayment status in July, 2005 (scale same as above)
# PAY_4: Repayment status in June, 2005 (scale same as above)
# PAY_5: Repayment status in May, 2005 (scale same as above)
# PAY_6: Repayment status in April, 2005 (scale same as above)
# BILL_AMT1: Amount of bill statement in September, 2005 (NT dollar)
# BILL_AMT2: Amount of bill statement in August, 2005 (NT dollar)
# BILL_AMT3: Amount of bill statement in July, 2005 (NT dollar)
# BILL_AMT4: Amount of bill statement in June, 2005 (NT dollar)
# BILL_AMT5: Amount of bill statement in May, 2005 (NT dollar)
# BILL_AMT6: Amount of bill statement in April, 2005 (NT dollar)
# PAY_AMT1: Amount of previous payment in September, 2005 (NT dollar)
# PAY_AMT2: Amount of previous payment in August, 2005 (NT dollar)
# PAY_AMT3: Amount of previous payment in July, 2005 (NT dollar)
# PAY_AMT4: Amount of previous payment in June, 2005 (NT dollar)
# PAY_AMT5: Amount of previous payment in May, 2005 (NT dollar)
# PAY_AMT6: Amount of previous payment in April, 2005 (NT dollar)
# default.payment.next.month: Default payment (1=yes, 0=no)

#Pre processing missing values.
sum(is.na(Default_Dataset))
#There are no missing values in the whole data set. 
plot(Default_Dataset$PAY_AMT6)

summary(Default_Dataset)

#######PRE PROCESSING ##########

# Set SEX as factor and uhochange levels for more clarity
Default_Dataset$SEX <- as.factor(Default_Dataset$SEX)
levels(Default_Dataset$SEX) <- c("male","female")

# MARRIAGE can't be 0, so it's wrong data. Set it as NA and then perform imputation
Default_Dataset$MARRIAGE[Default_Dataset$MARRIAGE == 0] <- NA

# EDUCATION should be in range [1,4], so any other value is wrong. 
Default_Dataset$EDUCATION[Default_Dataset$EDUCATION < 1 | Default_Dataset$EDUCATION > 4] <- NA

# Imputing NAs
imp <- mice::mice(Default_Dataset, m = 1)
Default_Dataset <- complete(imp)

# Set MARRIAGE as factor and change levels for more clarity
Default_Dataset$MARRIAGE <- as.factor(Default_Dataset$MARRIAGE)
levels(Default_Dataset$MARRIAGE) <- c("married","single","other")

# Set EDUCATION as factor and change levels for more clarity
Default_Dataset$EDUCATION <- as.factor(Default_Dataset$EDUCATION)
levels(Default_Dataset$EDUCATION) <- c("graduate school", "university", "high school", "other")

# Set default.payment.next.month as factor and change levels for more clarity
Default_Dataset$default.payment.next.month <- as.factor(Default_Dataset$default.payment.next.month)
levels(Default_Dataset$default.payment.next.month) <- c("no","yes")
#NEW VARIABLES
#We create 6 new variables that tell if an individual have good account status, paid duly, paid the minimum or have some delay in the payments.
#Those variables can have 4 values. After a very long investigation, even posting on the data set forum, we have made some assumption after inspecting the data.
#-2 : Good Account Status
#-1 : Pay-duly
# 0 : Pay minimum
# else: Delay
#Those new variables will be a factor.
Default_Dataset$PAYSTATUS_0 <- NA
Default_Dataset$PAYSTATUS_2 <- NA
Default_Dataset$PAYSTATUS_3 <- NA
Default_Dataset$PAYSTATUS_4 <- NA
Default_Dataset$PAYSTATUS_5 <- NA
Default_Dataset$PAYSTATUS_6 <- NA

getPaymentStatus <- function(row) {
  for (n in c(0,2,3,4,5,6)) {
    varPay <- paste("PAY_", n, sep = '')
    varStatus <- paste("PAYSTATUS_", n, sep = '')
    if(row[varPay]==-2){
      row[varStatus] = "Good account status" 
      row[varPay]=0
    }
    else if(row[varPay]==-1){
      row[varStatus] = "Pay-duly" 
      row[varPay]=0
    }
    else if(row[varPay]==0){
      row[varStatus] = "Pay minimum" 
    }
    else {
      row[varStatus] = "Delay" 
    }
  }
  return(row)
}
Default_Dataset <- adply(Default_Dataset, 1, getPaymentStatus)
for(i in c(0,2,3,4,5,6))
{
  varStatus <- paste("PAYSTATUS_", i, sep = '')
  Default_Dataset[,varStatus] = as.factor(Default_Dataset[,varStatus])
}
# We can load data set directly and skip above given steps
summary(Default_Dataset)

#Let's make a quick PCA to have some idea on how the data is 
PCADefault <- PCA(Default_Dataset,quali.sup = c(2,3,4,24,25,26,27,28,29,30,32,35))
PCADefault
#We can see that the PCA shows that all the variables PAY_X are correlated in an inmense way, so, we could unify this variables into one unic variable.
#We will unify the variables in this way:
#The variables PAYSTATUS_X, as those variables are related with PAY_X will be transformed to
#The variables will be transformed into:
#PAY_X --> Delay. This variable will contain the MEAN delay of this individual in the payments.
#PAY_STATUSX--> AccountStatus. This variable will contain the MOST COMMON status in the account for this individual.
#As the data have to be refactored, we will inspec the PCA once the data is refactored.
######DATA REFACTOR######
Default_Dataset$Delay = ""
Default_Dataset$AccountStatus = ""
refactorDataset <- function(row) {
  valuePay = 0
  valueStatus = 0
  for (n in c(0,2,3,4,5,6)) {
    varPay <- paste("PAY_", n, sep = '')
    varStatus <- paste("PAYSTATUS_", n, sep = '')
    #We assign more value to a good condition and less to a bad condition. In the end we will do a floor of the mean to pick the worst escenario.
    if(as.numeric(row[varStatus])==1){
      valueStatus = valueStatus +1
    }
    else if(as.numeric(row[varStatus])==2){
      valueStatus = valueStatus +4
      
    }
    else if(as.numeric(row[varStatus])==3){
      valueStatus = valueStatus +2
    }
    else {
      valueStatus = valueStatus +3
    }
    valuePay = as.numeric(row[varPay]) + valuePay
  }
  row["Delay"] = ceiling(valuePay/6)
  row["AccountStatus"] = floor(valueStatus/6) #We assign an status that is mean of their status. Using the floor function.
  return(row)
}
Default_Dataset <- adply(Default_Dataset, 1, refactorDataset)#Apply as before

#Make factor account status
Default_Dataset$AccountStatus = as.factor(Default_Dataset$AccountStatus) 
levels(Default_Dataset$AccountStatus) <- c("Delay","Paid minimum","Paid Duly","Good Status")

#Create means for payments and bills for PCA, so we can unify more the data.
Default_Dataset$MeanBill = rowMeans(Default_Dataset[,12:17])
Default_Dataset$MeanPay = rowMeans(Default_Dataset[,18:23])

#We will add a new variable that will contain a buckets of age 20,30,40,50,etc.
Default_Dataset$AGE.decade<-cut(Default_Dataset$AGE,c(10,20,30,40,50,60,70,80))


# Detect outliers using robust mahalanobis distance and remove them
library(chemometrics)
moutlier= Moutlier(X = Default_Dataset[,-c((2:30), 31,32, 35)],quantile = 0.999)
outliers = sort(x = moutlier$rd,decreasing = TRUE, index.return=TRUE)
outlierstodelete = outliers$ix[1:300] #DELETE 0.001% --> 30 INDIVIDUALS
Default_Dataset = Default_Dataset[-outlierstodelete,]

# Saved pre-processed data set for future preprocessing
save(Default_Dataset, file = "Default_dataset_preprocessed.Rdata")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "Default_dataset_preprocessed.Rdata")

if(!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)
#####SOME PLOTS AND OTHER THINGS IN ORDER TO HAVE A CLEAR IDEA OF THE DATA#######
#Detect difference between bill atm with boxplots
boxplot(Default_Dataset$BILL_AMT1, Default_Dataset$BILL_AMT2, Default_Dataset$BILL_AMT3, Default_Dataset$BILL_AMT4, 
        Default_Dataset$BILL_AMT5, Default_Dataset$BILL_AMT6,main="Boxplot Bill variables")
#Same with pay
boxplot(Default_Dataset$PAY_AMT1, Default_Dataset$PAY_AMT2, Default_Dataset$PAY_AMT3, Default_Dataset$PAY_AMT4, 
        Default_Dataset$PAY_AMT5, Default_Dataset$PAY_AMT6,main="Boxplot Bill variables")
#The second column are the individuals that will default. As we can see, individuals with any education can default.
ggplot(Default_Dataset, aes(default.payment.next.month, EDUCATION)) +
  geom_jitter(aes(color = EDUCATION), size = 0.5)+ theme_classic()
#In this plot we can see that the marital status really do not make a difference and dont tell anything in particular relating the default of the credit card.
# Here we can see that there are some values for 0 marriage, those values are incorrect and must be corrected.
ggplot(Default_Dataset, aes(default.payment.next.month, MARRIAGE)) +
  geom_jitter(aes(color = MARRIAGE), size = 0.5)+ theme_classic()
#Same with Age
ggplot(Default_Dataset, aes(default.payment.next.month, AGE.decade)) +
  geom_jitter(aes(color = SEX), size = 0.5)+ theme_classic()
#Now with density
qplot(AGE, data = Default_Dataset, geom = "density", fill = default.payment.next.month)
#Same that before but with sex
ggplot(Default_Dataset, aes(default.payment.next.month, SEX)) +
  geom_jitter(aes(color = SEX), size = 0.5)+ theme_classic()
#Same with the AccountStatus
ggplot(Default_Dataset, aes(default.payment.next.month, AccountStatus)) +
  geom_jitter(aes(color = SEX), size = 0.5)+ theme_classic()
#Same but with the color scale of marriage
ggplot(Default_Dataset, aes(default.payment.next.month, AccountStatus)) +
  geom_jitter(aes(color = MARRIAGE), size = 0.5) + theme_classic()
#same buy with education scale
ggplot(Default_Dataset, aes(default.payment.next.month, AccountStatus)) +
  geom_jitter(aes(color = EDUCATION), size = 0.5) + theme_classic()
#Here we can see the relation between account status and delay. We can see that the account with the status of "good status" do not have any delay, "Paid Duly"
#have some delay like paid minimum (but more) and the accounts of Delay always have delay.
ggplot(Default_Dataset, aes(Delay, AccountStatus)) +
  geom_jitter(aes(color = MARRIAGE), size = 0.5)+ theme_classic()
#Here we can see in a different way the relation of education with default. 
ggplot(Default_Dataset, aes(x=EDUCATION, fill = default.payment.next.month))+
  geom_bar(width = 1)+
  coord_polar()+
  theme_classic()
#RELATION BTWN LIMIT_BAL AND EDUCATION
ggplot(Default_Dataset, aes(x = EDUCATION, fill = LIMIT_BAL)) +
  geom_bar() +
  labs(x = 'Education') +
  labs(y = 'Limit crédit') +
  theme_few()
#Relation between age and default
ggplot(Default_Dataset, aes(AGE, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 6) + 
  facet_grid(.~EDUCATION) + 
  theme_fivethirtyeight()
#Correlations Between Limit Balance, Bill Amounts & Payments: 
#When we reflect the correlations between limit balances, bill amounts and payments amounts; it presents us that there’s a low correlation between the 
#limit balances and payments and bill amounts. Howevet it can be seen that bill amounts has high correlation
#between each other as expected since the bills a reflecting the cumulative amounts.
cor <- cor(subset(Default_Dataset, select = c(LIMIT_BAL,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)))
corrplot(cor, method="number")
#Personal Balance Limits Probabilities & Given Limits By Age

#In this plot we explore the probability of having a higher balance limit by age and comparing the results of the limit of credit by default or not default
#We can see that the no default individuals have more credit limit, but not that more, we can see also, that the blue line, which is the mean, it is 200k$ of limit
#of credit for all individuals, but with the age advance, the limit is lower. 
#From this plot we can interpret that the bank preffers to give more credit and risk more, because we can see how people with more than 200k$ of credit limit defaults.
#It will be a decission of the bank to change the politics of credit limit, we an suggest to decrease the credit limit ntil the 40's.
ggplot(aes(x=AGE,y=LIMIT_BAL/1000),data=subset(Default_Dataset,!is.na(AGE.decade)))+
  xlab("Age") + 
  ylab("Balance Limit x1000") +
  coord_cartesian(xlim = c(20,60),ylim = c(0,700))+
  scale_color_brewer(palette = "Pastel2")+
  geom_jitter(alpha=0.5, position = position_jitter(h=0), aes(color=AGE.decade)) +
  geom_smooth(stat='summary', fun.y=mean) + #The blue line.
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.05), color = 'green', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.3), color = 'yellow', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.5), color = 'red', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.8), color = 'black', linetype=2)+
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.95), color = 'purple', linetype=2)+
  facet_wrap(~default.payment.next.month)
########PCA#########
PCADefault = PCA(Default_Dataset[,-c((2:4),(6:30), 32, 35)],ncp = 10)
PCADefault
#That PCA make a lot of sense, the payments are inversely correlated with the Delay, it makes sense, because if you have more delay, mean that you pay less
#Also inversely correlate with the limit of credit.
#The variables BILL_ATMX and PAY_ATMX are very correlated with themselves, Also make sense.
#Best and worst represented.
cos1 = PCADefault$ind$cos2 #Return the cos of the individuals
which.max(cos1[,1]) 
which.min(cos1[,1]) 

#Contributions
contribution <- PCADefault$ind$contrib

bestinfirstPC <- sort(contribution[,1],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(Contribution) in the first principal component
bestinfirstPC

bestinsecondPC <- sort(contribution[,2],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(contribution) in the second principal component
bestinsecondPC

#Best represented variables
cos2 = PCADefault$var$cos2 #Returns the cos of the variables.
which.max(cos2[,1]) #BILL_AMT4 is the best represented variable in the first factorial plame
which.min(cos2[,1]) #DELAY is the worst represented variable in the first factorial plame

#Most influencial variables
contribution2 <- PCADefault$var$contrib

bestinfirstPCvar <- sort(contribution2[,1],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(Contribution) in the first principal component
bestinfirstPCvar

bestinsecondPCvar <- sort(contribution2[,2],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(contribution) in the second principal component
bestinsecondPCvar

#Significant dimensions
dim <- sum(as.numeric(PCADefault$eig[,3] <= 80)) #5 significant dimensions

#######Clustering ###### 
#Kmeans
#Here, as we are working on a large data set, we will cut the tree.
# Select significant dimensions whose cumulative percentage of variance <= 80%
dim <- sum(as.numeric(PCADefault$eig[,3] <= 90))
Psi <- PCADefault$ind$coord[,1:dim]
centers = 20
defaultKmeans1 <- kmeans(Psi, centers =centers, iter.max = 50)
defaultKmeans2 <- kmeans(Psi, centers =centers, iter.max = 50)
table(defaultKmeans1$cluster,defaultKmeans2$cluster)
clas <- (defaultKmeans2$cluster-1)*centers+defaultKmeans1$cluster
freq <- table(clas)
freq
cdclas <- aggregate(as.data.frame(Psi),list(clas),mean)[,2:(dim+1)]
d2 <- dist(cdclas) #matrix of distances
h2 <- hclust(d2,method="ward.D2",members=freq) #Hiretical clustering, members = freq because not all the centroids have the same importance.
plot(h2)
barplot(h2$height[(nrow(cdclas)-40):(nrow(cdclas)-1)]) #Plot last 40 aggregations
nc = 3 # for instance, number of clusters.
c2 <- cutree(h2,nc)
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(dim+1)] 
finalKmeans <- kmeans(Psi,centers=cdg)
Bss <- sum(rowSums(finalKmeans$centers^2)*finalKmeans$size)
Wss <- sum(finalKmeans$withinss)
Ib6 <- 100*Bss/(Bss+Wss)
Ib6
#Analysis of the clustering
finalKmeans$cluster <- as.factor(finalKmeans$cluster)
#How many individuals per cluster? 
#Needs interpretation depend of PCA...
finalKmeans$size
finalKmeans$cluster
Default_Dataset <- cbind(Default_Dataset, clusterNum = finalKmeans$cluster)
Datacluste1 = Default_Dataset[Default_Dataset$clusterNum==1,]
sum(Datacluste1$default.payment.next.month=='no') 
summary(Datacluste1)
Datacluster2 = Default_Dataset[Default_Dataset$clusterNum==2,]
summary(Datacluster2)
Datacluster3 = Default_Dataset[Default_Dataset$clusterNum==3,]
summary(Datacluster3)

clusplot(Psi, finalKmeans$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

#DECISION TREE
library(rpart)
# grow tree
require(caTools)

#Randomly shuffle the data
Default_shuffled<-Default_Dataset[sample(nrow(Default_Dataset)),]

train_indexes <- sample(nrow(Default_shuffled), size = floor(nrow(Default_shuffled)*0.75))

train <- Default_Dataset[train_indexes,]
test <- Default_shuffled[-train_indexes,]

fit <-  rpart(default.payment.next.month ~ ., data = train, method = "class",control = rpart.control(minsplit = 10,cp = 0.001))
print(fit)
summary(fit)

fit$cptable

plotcp(fit)

fit.pruned <- prune(fit, cp = 0.01)

library(rpart.plot)


prp(fit, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")
fit.pred <- predict(fit.pruned, test, type="class")
fit.perf <- table(test$default.payment.next.month, fit.pred,
                  dnn=c("Actual", "Predicted"))
fit.perf
## We estimate the % of error of the model
errortree = (fit.perf[2,1] + fit.perf[1,2]) /nrow(test)
print(errortree) #18%
confusionMatrixtree <- table(test$default.payment.next.month, fit.pred,
                             dnn=c("Actual", "Predicted"))
## We estimate the % of error of the model
accuracy <- computeAccuracy(list(confusionMatrixtree))
precision <- computePrecision(list(confusionMatrixtree))
recall <- computeRecall(list(confusionMatrixtree))
# Random Forest

library(randomForest)

# Find optimal number of trees based on lowest OOB

(ntrees <- round(10^seq(1,3.7,by=0.2)))
# prepare the structure to store the partial results
rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1
for (nt in ntrees)
{ 
  print(nt)
  model.rf <- randomForest(default.payment.next.month~ ., data=train, ntree=nt, proximity=FALSE)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  # Free memory
  gc()
  ii <- ii+1
}
#Perform random forest with 100 tree which is optimal
rfpredictor <- randomForest(default.payment.next.month ~ ., data = train, ntree = 100, mtry=2)
plot(rfpredictor)
prediction <- predict(rfpredictor, test)
confusionMatrixForest <- table(data=test$default.payment.next.month, predicted=prediction)
accuracy <- computeAccuracy(list(confusionMatrixForest))
precision <- computePrecision(list(confusionMatrixForest))
recall <- computeRecall(list(confusionMatrixForest))

print(sprintf("Accuracy: %.2f%% Precision: %.2f%% Recall: %.2f%%", 
              100*accuracy, 100*precision, 100*recall))
# Perform cross validation for RandomForest

library(TunePareto)

model.CV <- function (k) {
  CV.folds <- generateCVRuns(train$default.payment.next.month, ntimes=1, nfold=k, stratified=TRUE)
  train_data <- train
  
  cv.results <- matrix (rep(0,4*k),nrow=k)
  colnames (cv.results) <- c("k","fold","TR error","VA error")
  
  all.cv.results <- list()
  cv.results[,"TR error"] <- 0
  cv.results[,"VA error"] <- 0
  
  for (j in 1:k) {
    print(j)
    # get VA data
    va <- unlist(CV.folds[[1]][[j]])
    #print(length(va))
    # train on TR data
    my.model.TR <- randomForest(default.payment.next.month ~ ., data=train_data[-va,], ntree=100, proximity=FALSE) 
    
    # predict TR data
    pred.va <- my.model.TR$predicted
    tab <- table(Truth = train[-va,]$default.payment.next.month, Pred = pred.va)
    
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    # predict VA data
    pred.va <- predict (my.model.TR, train_data[va, -which(colnames(train_data) == "default.payment.next.month")], type="class")
    tab <- table(Truth = train_data[va,]$default.payment.next.month, Pred = pred.va)
    
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    all.cv.results[[j]] <- cv.results
  }
  
  return(all.cv.results)
  
}

k <- 10
rf.cv <- model.CV(k)
rf.cv.df <- as.data.frame(rf.cv[10])

rf.cv.df <- rf.cv.df[,c(-1,-2)]

pe.hat <- 0.1859556
dev <- sqrt(pe.hat*(1-pe.hat)/floor(length(train_indexes)))*1.967

sprintf("(%f,%f)", pe.hat-dev,pe.hat+dev)