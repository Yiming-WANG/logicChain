#install.packages("bnlearn")
library(bnlearn)

df <- read.csv("e:/Case3Data.csv")
df <- df[2:14]
df

hist(df$USDindex)

par(mfrow=c(4,3))
for(i in 2:13) {
  hist(df[,i], main=names(df)[i],cex=5)
}

par(mfrow=c(1,1))
boxplot(df$BudgetDeficit_YoY,main="BudgetDeficit_YoY")
boxplot(df$LongtermSecurityHoldingByInternationalInvestor,main="LongtermSecurityHoldingByInternationalInvestor")
boxplot(df$InternationalCapitalFlows_MoM,main="InternationalCapitalFlows_MoM")

#install.packages("corrplot")
library(corrplot)
M<-cor(df)
corrplot(M, method="pie", type = "lower") 

df1=cbind(df[1:7],df[9],df[11:12])
df1
sum(is.na(df1))



hc1 <-hc(df1,score="loglik-g")
plot(hc1)
score(hc1,df1)

hc2 <-hc(df1,score="aic-g")
plot(hc2)
score(hc2,df1)

hc3 <-hc(df1,score="bic-g")
plot(hc3)
score(hc3,df1)

hc4 <-hc(df1,score="bge")
plot(hc4)
score(hc4,df1)



tabu1 <-tabu(df1,score="loglik-g")
plot(tabu1)
score(tabu1,df1)

tabu2 <-tabu(df1,score="aic-g")
plot(tabu2)
score(tabu2,df1)

tabu3 <-tabu(df1,score="bic-g")
plot(tabu3)
score(tabu3,df1)

tabu4 <-tabu(df1,score="bge")
plot(tabu4)
score(tabu4,df1)


help(gs)
bngs <-gs(df1)
plot(bngs)

bn2 <- iamb(df1)
plot(bn2)

bn3 <- fast.iamb(df1)
plot(bn3)

bn4 <- inter.iamb(df1)
plot(bn4)
