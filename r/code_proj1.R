NSR_M<-read.csv("C:\\Users\\pc\\OneDrive\\Documents\\project data\\NSR_MAHD.csv",header = TRUE)
NSR_M

attach(NSR_M)
#descriptive statistics
summary(NSR_M)
str(NSR_M)
names(NSR_M)
dim(NSR_M)
des_NSR_M<-NSR_M[,sapply(NSR_M,is.numeric)]
des_NSR_M
as.Date(DATE)
factor(PHASE)
#min
minf<-function(x){
    r<-min(x)
    return(r)
  }
  for(i in 1:13){
    res<-minf(des_NSR_M[i])
    cat("Min of",names(des_NSR_M[i]),"is",res,'\n')
  }

#max
maxf<-function(x){
  r<-max(x)
  return(r)
}
for(i in 1:13){
  res<-maxf(des_NSR_M[i])
  cat("Max of",names(des_NSR_M[i]),"is",res,'\n')
}

#qunatile
quaf<-function(x){
  r<-quantile(x)
  return(r)
}
for(i in 1:13){
  res<-quaf(unlist(des_NSR_M[i]))
  for(j in 1:4){
  cat(j,"th quantile of",names(des_NSR_M[i]),"is",res[j],'\n')
}
}

#idr
iqrf<-function(x){
  r<-IQR(x)
  return(r)
}
for(i in 1:13){
  res<-iqrf(unlist(des_NSR_M[i]))
  cat("Interquartile Range  of",names(des_NSR_M[i]),"is",res,'\n')
}

#qda
qdaf<-function(x){
  r<-IQR(x)/2
  return(r)
}
for(i in 1:13){
  res<-qdaf(unlist(des_NSR_M[i]))
  cat("Quartile Deviation of",names(des_NSR_M[i]),"is",res,'\n')
}

#range
rangef<-function(x){
  r<-range(x)
  return(r)
}
for(i in 1:13){
  res<-rangef(s[i])
  cat("Range of",names(des_NSR_M[i]),"is",res,'\n')
}


#sum
sumf<-function(x){
  r<-sum(x)
  return(r)
}
for(i in 1:13){
  res<-sumf(des_NSR_M[i])
  cat("Sum of",names(des_NSR_M[i]),"is",res,'\n')
}

#mean
meanf<-function(x){
  r<-mean(x)
  return(r)
}
for(i in 1:13){
  res<-meanf(unlist(des_NSR_M[i]))
  cat("Mean of",names(des_NSR_M[i]),"is",res,'\n')
}

#median
medianf<-function(x){
  r<-median(x)
  return(r)
}
for(i in 1:13){
  res<-medianf(unlist(des_NSR_M[i]))
  cat("Median of",names(des_NSR_M[i]),"is",res,'\n')
}


#mean deviation
madf<-function(x){
  r<-mad(x)
  return(r)
}
for(i in 1:13){
  res<-madf(unlist(des_NSR_M[i]))
  cat("Mean deviation of",names(des_NSR_M[i]),"is",res,'\n')
}


#sda
sdaf<-function(x){
  r<-sd(x)
  return(r)
}
for(i in 1:13){
  res<-sdaf(unlist(des_NSR_M[i]))
  cat("Standard Deviation of",names(des_NSR_M[i]),"is",res,'\n')
}

#deciles
decff<-function(x){
  r<-quantile(x,seq(0,1,by=0.1))
  return(r)
}
for(i in 1:13){
  res<-decff(unlist(des_NSR_M[i]))
  for(j in 1:10){
  cat(j,"th Decile of",names(des_NSR_M[i]),"is",res[j],'\n')
  }
}

#octiles
octf<-function(x){
  r<-quantile(x,seq(0,1,by=0.125))
  return(r)
}
for(i in 1:13){
  res<-octf(unlist(des_NSR_M[i]))
  for(j in 1:8){
    cat(j,"th Octile of",names((des_NSR_M[i])),"is",res[j],'\n')
  }
}


#mode
NSR_MM<-NSR_M[c(-1,-3,-11,-19,-26)]
getmode<- function(v){
  x<-table(v)
  y<-max(x)
  a<-names(x)[which(x==y)]
  return(a)
}
for(i in 1:21){
  res<-getmode(unlist(NSR_MM[i]))
  cat("mode of",names((NSR_MM[i])),"is",res,'\n')
}


#cv
cvfun<-function(a,b){
  cv<-cov(a,b)
  return(cv)
}
for (i in 1:12) {
  for(j in 2:13){
  res<-cvfun(unlist(des_NSR_M[i]),unlist(des_NSR_M[j]))
  cat("covariance of",names(des_NSR_M[i]),"and",names(des_NSR_M[j]),"is",res,'\n')
}
}


#cor
corfun<-function(a,b){
  cv<-cor(a,b)
  return(cv)
}
for (i in 1:12) {
  for(j in 2:13){
  res<-corfun(unlist(des_NSR_M[i]),unlist(des_NSR_M[j]))
  cat("correlation of",names(des_NSR_M[i]),"and",names(des_NSR_M[j]),"is",res,'\n')
}
}



#UNIVARIATE TABLE
NSR_UT<-NSR_MM[c(-1,-2,-3,-4,-16,-17)]
dim(NSR_UT)
utf<-function(x){
  r<-table(x)
  return(r)
}
for(i in 1:15){
  res<-utf(NSR_UT[i])
  print(res)
}

#BIVAR
NSR_BT<-NSR_MM[c(-1,-3,-7,-16,-17,-18,-19,-20,-21)]
dim(NSR_BT)
names(NSR_MM)
btf<-function(x,y){
  r<-table(x,y)
  return(r)
}
for(i in 1:11){
  for(j in 2:12)
  res<-btf(unlist(NSR_BT[i]),unlist(NSR_BT[j]))
  print(res)
}


#pie chart
pie(table(NSR_M$CATEGORY),main=" PIE CHART OF CATEGORY",col=rainbow(3))
pie(table(NSR_M$PHASE),main=" PIE CHART OF PHASE",col=rainbow(5))
pie(table(NSR_M$NATURE.OF.SPPLY),main=" PIE CHART OF NATURE OF SUPPLY",col=rainbow(3))
pie(table(NSR_M$MAKE),main=" PIE CHART OF MAKE",col=rainbow(2),labels = c("HIMACHAL ENERGY","HPL - ELECTRIC"))


#hist
hist(NSR_M$APP.FEE)
hist(NSR_M$SEC.DEP)

#barplot
barplot(table(NSR_M$DEV.CHG))
barplot(table(NSR_M$DATE))
barplot(table(NSR_M$AREA.CODE))
barplot(table(NSR_M$MANDAL.NAME))




#Predictive statistics
install.packages("forecast")
library(forecast)
ts_data <- ts(NSR_M$CATEGORY, frequency = 12)  # Assuming monthly data
fit <- auto.arima(ts_data)
forecast(fit, h = 12)


NSR_MH<-head(NSR_M)
NSR_MH

#load prediction
lm_model <- lm(LOAD ~ NATURE.OF.SPPLY + PHASE + AREA.CODE, data = NSR_MH)
lm_model
summary(lm_model)
test_data <- data.frame(
  NATURE.OF.SPPLY = c("DOMESTIC", "NON-DOMESTIC AND COMMERCIAL", "AGRICULTURAL", "AGRICULTURAL", "DOMESTIC",
               "NON-DOMESTIC AND COMMERCIAL"),
  PHASE = c(1, 3, 1, 2, 3,1),
  AREA.CODE = c(101, 102, 103, 104, 101, 102)

)
predicted_load<-predict(lm_model, newdata = test_data)
predicted_load
test_data$Predicted_load <- predicted_load
print(test_data)

lm_model <- lm(LOAD ~ MAKE, data = NSR_MH)
lm_model
coef(summary(lm_model))
predict(lm_model, newdata = data.frame(MAKE="HPL - ELECTRIC"))
predict(lm_model, newdata = data.frame(MAKE="HIMACHAL ENERGY"))

lm_model <- lm(LOAD ~ 0+MAKE, data = NSR_MH)
lm_model


#dev.chg
lm_model <- lm(DEV.CHG ~ as.character(AREA.CODE) , data = NSR_MH)      
lm_model
predict(lm_model, newdata = data.frame(AREA.CODE=302))


#chi square
k<-NSR_M[c(9,13)]
k
ct<-chisq.test((k))
ct


p<-NSR_M[c(9,13,24)]
p
ct1<-chisq.test((k))
ct1


#PRESCRIPTIVE
t.test(NSR_M$LOAD, mu = 10)
t.test(NSR_M$SEC.DEP, mu = 12)
t.test(NSR_M$SEC.DEP, NSR_M$DEV.CHG)
t.test(NSR_M$SEC.DEP, NSR_M$APP.CHG)
t.test(LOAD ~ PHASE, data = NSR_M)
t.test(CATEGORY ~ PHASE, data = NSR_M)


prop.test(x = sum(NSR_M$NATURE.OF.SPPLY == "Domestic"), n = nrow(NSR_M), p = 0.3)
prop.test(x = sum(NSR_M$NATURE.OF.SPPLY == "AGRICULTURAL"), n = nrow(NSR_M), p = 0.2)
prop.test(x = sum(NSR_M$MAKE == "HPL - ELECTRIC"), n = nrow(NSR_M), p = 0.2)
prop.test(x = sum(NSR_M$MAKE == "HIMACHAL"), n = nrow(NSR_M), p = 0.7)



prop.test(c(sum(NSR_M$MANDAL.NAME == "MOHAMMADABAD "), sum(NSR_M$MANDAL.NAME == "	MOKARLABAD")),
          c(nrow(NSR_M[NSR_M$MANDAL.NAME == "MOHAMMADABAD ", ]), nrow(NSR_M[NSR_M$MANDAL.NAME == "MOKARLABAD", ])))

var.test(NSR_M$LOAD,NSR_M$SEC.DEP, alternative = "two.sided", ratio = 1)
var.test(NSR_M$LOAD,NSR_M$DEV.CHG, alternative = "two.sided", ratio = 1)
var.test(NSR_M$DEV.CHG,NSR_M$SEC.DEP)
var.test(NSR_M$DEV.CHG,NSR_M$APP.FEE)
var.test(LOAD ~ PHASE, data = NSR_M)
var.test(SEC.DEP ~ PHASE, data = NSR_M)

cor.test(NSR_M$SEC.DEP, NSR_M$DEV.CHG)
cor.test(NSR_M$SEC.DEP, NSR_M$APP.FEE)
cor.test(NSR_M$APP.FEE, NSR_M$DEV.CHG)



install.packages("randtests")
library(randtests)
runs.test(NSR_M$LOAD)
runs.test(NSR_M$SEC.DEP)
runs.test(NSR_M$APP.FEE)
runs.test(NSR_M$DEV.CHG)


wilcox.test(LOAD ~ PHASE, data = NSR_M)
wilcox.test(SEC.DEP ~ PHASE, data = NSR_M)

library(BSDA)
SIGN.test(NSR_M$SEC.DEP, md = 15)
SIGN.test(NSR_M$APP.FEE, md = 1000)





#PRESCRIPTIVE STATISTICS
anova_model <- aov(LOAD ~ NATURE.OF.SPPLY, data = NSR_M)
#PRESCRIPTIVE STATISTICS
anova_model 
summary(anova_model)
anova_model <- aov(LOAD+SEC.DEP ~ NATURE.OF.SPPLY, data = NSR_M)
#PRESCRIPTIVE STATISTICS
anova_model 
summary(anova_model)
anova_model <- aov(LOAD ~ MAKE*NATURE.OF.SPPLY, data = NSR_M)
#PRESCRIPTIVE STATISTICS
anova_model 
summary(anova_model)

TukeyHSD(anova_model)




#logistic
glmm<-glm(CATEGORY~LOAD,data=NSR_M)
glmm

glmm1<-glm(CATEGORY~0+LOAD,data=NSR_M)
glmm1

#mlr
lm_model <- lm(LOAD ~ MAKE+PHASE+CAPACITY, data = NSR_MH)      
lm_model
predict(lm_model, newdata = data.frame(MAKE="HPL - ELECTRIC",PHASE=1,CAPACITY="100/5A"))
predict(lm_model, newdata = data.frame(MAKE="HIMACHAL ENERGY",PHASE=1,CAPACITY="10-40A"))

lm_model <- lm(LOAD ~ 0+MAKE+PHASE+CAPACITY, data = NSR_MH)      
lm_model


install.packages("psych")
library(psych)

# Select numeric columns for factor analysis
numeric_data <- NSR_M[, sapply(NSR_M, is.numeric)]

# Factor analysis with 2 factors (modify number as needed)
factor_analysis <- fa(numeric_data, nfactors = 3, rotate = "varimax")
print(factor_analysis)

names(NSR_M)
# Example: PCA on numerical data
numeric_data <- NSR_M[c(10,16,17,18)]

# Standardizing the data
scaled_data <- scale(numeric_data)

# PCA
pca_result <- prcomp(scaled_data)
summary(pca_result)

# Scree plot
plot(pca_result, type = "l")
biplot(pca_result)

dist_matrix <- dist(scaled_data, method = "euclidean")
hclust_model <- hclust(dist_matrix, method = "ward.D2")
plot(hclust_model)

