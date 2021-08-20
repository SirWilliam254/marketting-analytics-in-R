# marketting-analytics-in-R
some code to perform marketting analytics in R, descriptions and outputs in the blog
```r
## Setting the working directory

## LOADING ALL REQUIRED PACKAGES
library(corrplot)
library(gplots)
library(nFactors)
library(xtable)
library(knitr)
library(ggplot2)
library(mlogit)
library(caret)
library(dfidx)
library(arules)
library(grid)
library(plotly)
library(arulesViz)
library(car)


seg.df <- read.csv("1_demographics.csv",stringsAsFactors = TRUE)
head(seg.df, n=8)
summary(seg.df)
seg.df.sc <- seg.df[,-1] # removing the first row --ID
summary(seg.df.sc) # Observing the summary some values lie quite far than others
#in some variables hence we standardize
seg.df.sc[,c(3,5,6,7,8)] <- scale(seg.df[,c(3,5,6,7,8)])
summary(seg.df.sc)
#Distance
seg.dict <- dist(seg.df.sc)
as.matrix(seg.dict)[1:5, 1:5]
seg.hc <- hclust(seg.dict, method = "complete")
plot(seg.hc) # too much going on.
plot(cut(as.dendrogram(seg.hc), h = 6)$lower[[1]])
plot(seg.hc)
rect.hclust(seg.hc,k=4, border = "red")
seg.hc.segment <- cutree(seg.hc,k=4)
table(seg.hc.segment)
#Describing clusters
aggregate(seg.df, list(seg.hc.segment), mean)
boxplot(seg.df$Salary ~ seg.hc.segment, ylab = "salary", xlab = "Cluster")
boxplot(seg.df$Gender ~ seg.hc.segment, ylab = "Gender", xlab = "Cluster")
boxplot(seg.df$Education ~ seg.hc.segment, ylab = "Education", xlab = "Cluster")
boxplot(seg.df$Age_Group ~ seg.hc.segment, ylab = "Age_Group", xlab = "Cluster")
##RATING
brand.ratings <- read.csv("2_chocolate_rating.csv", stringsAsFactors = TRUE)
head(brand.ratings)
summary(brand.ratings)
str(brand.ratings) # structure of the data.
brand.ratings1 <- brand.ratings[,c(2,6,7,8,9,10,11,12,13,14)] # brand, ratings and ingredients
summary(brand.ratings1)
brand.sc <- brand.ratings1
brand.sc[,2:4] <- scale(brand.ratings1[,2:4])
summary(brand.sc)
cor(brand.sc[,2:10]) # correlations

corrplot.mixed(cor(brand.sc[,2:10]))
corrplot(cor(brand.sc[,2:10]),order="hclust")
#Mean rating by brand
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
brand.mean
rownames(brand.mean) <- brand.mean[,1]
brand.mean <- brand.mean[,-1]
brand.mean
#
heatmap.2(as.matrix(brand.mean),main = "Brand attributes",
          trace = "none", key = FALSE, dend = "none")
#PCA
brand.pc<- princomp(brand.mean, cor = TRUE)
summary(brand.pc)
plot(brand.pc,type="l")
loadings(brand.pc)
brand.pc$scores
biplot(brand.pc,main="Brand positioning")
#
summary(brand.ratings1)
df500 <- brand.ratings1[,c(2:10)]
summary(df500)
nScree(df500)
eigen(cor(df500))$Values
na.omit(df500)
nScree(df500)
eigen(cor(df500))$Values

##MARKETTING ANALYSIS
#
cbc.df <- read.csv("5_conjoint.csv",stringsAsFactors = TRUE)
head(cbc.df)
str(cbc.df)
summary(cbc.df)
xtabs(Choice~Price,data = cbc.df)
xtabs(Choice~Salary, data=cbc.df)
str(cbc.df1)
is.na("cbc.df1")
cbc.df1 <- cbc.df[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]
cbc.df1$Origin <- relevel(cbc.df1$Origin, ref ="Venezuela")
cbc.df1$Manufacture <- relevel(cbc.df1$Manufacture, ref = "Developed")
cbc.df1$Energy <- relevel(cbc.df1$Energy, ref = "Low")
cbc.df1$Nuts <- relevel(cbc.df1$Nuts, ref = "Nuts and Fruit")
cbc.df1$Tokens <- relevel(cbc.df1$Tokens, ref = "No")
cbc.df1$Organic <- relevel(cbc.df1$Organic, ref = "No")
cbc.df1$Premium <- relevel(cbc.df1$Premium, ref = "No")
cbc.df1$Fairtrade <- relevel(cbc.df1$Fairtrade, ref = "No")
cbc.df1$Sugar <- relevel(cbc.df1$Sugar,ref = "High")
#

cbc.mlogit <- dfidx(cbc.df1, choice="Choice",
                    idx=list(c("Choice_id", "Consumer_id"), "Alternative"))

model<-mlogit(Choice ~ 0+Origin+Manufacture+Energy+Nuts+
                Tokens+Organic+Premium+Fairtrade+Sugar+Price, data=cbc.mlogit)
kable(summary(model)$CoefTable)
#Meaning of parameters
model.constraint <- mlogit(Choice ~ 0+Origin, data = cbc.mlogit)
lrtest(model,model.constraint)
#Predicted Market Share
kable(head(predict(model,cbc.mlogit)))
predicted_alternative <- apply(predict(model,cbc.mlogit),1,which.max)
selected_alternative <- cbc.mlogit$Alternative[cbc.mlogit$Choice>0]
confusionMatrix(table(predicted_alternative,selected_alternative),positive = "1")
(coef(model)["OriginEcuador"]-coef(model)["OriginPeru"]) / (-coef(model)["Price"])
coef(model)["EnergyHigh"] /(-coef(model)["Price"])
                             coef(model)["EnergyHigh"] /(-coef(model)["Price"])
                             coef(model)["NutsNo"] /(-coef(model)["Price"])
                             coef(model)["OrganicYes"] /(-coef(model)["Price"])                             
                             coef(model)["PremiumYes"] /(-coef(model)["Price"])
                             
                            
                           
                              #
  retail.raw <- readLines("6_groceries.dat")
   head(retail.raw)
  tail(retail.raw)
 summary(retail.raw)
retail.list <- strsplit(retail.raw, " ")
names(retail.list) <- paste("Trans", 1:length(retail.list))
str(retail.list)
some(retail.list)
rm(retail.raw)
 retail.trans <- as(retail.list, "transactions") 
 summary(retail.trans)
 rm(retail.list)
inspect(head(retail.trans,3))
 groc.rules <- apriori(retail.trans, parameter = list(supp=0.03, conf=0.3, target="rules"))
  inspect(subset(groc.rules, lift > 3))
   plot(groc.rules, jitter = 0)
 plot(groc.rules, engine = "plotly", jitter = 0)
   groc.hi <- head(sort(groc.rules, by="lift"), 30)
  inspect(groc.hi)
   plot(groc.hi, method="graph")
      ```
