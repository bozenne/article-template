library(riskRegression)
library(data.table)
library(multcomp)
library(ggplot2)
library(survival)
library(Epi)

## * load data
df.data <- read.csv("data/data-bissau-processed.csv")

## * descriptive
## ** outcome
outcome <- c("dead","alive","lost")[(df.data$status=="dead") + 2*(df.data$status=="censored" & df.data$time==183)+3*(df.data$status=="censored" & df.data$time<183)]
table(outcome)
## alive  dead  lost 
##  2548   222  2504 
round(100*prop.table(table(outcome)),2)
## alive  dead  lost 
## 48.31  4.21 47.48 
quantile(df.data$time[df.data$status=="censored" & df.data$time<183], prob = c(0,0.05,0.1))
## 0%  5% 10% 
##  2 124 128 
## really starts from day 124, i.e. month 4

## ** exposure
table(df.data$bcg)
##    0    1 
## 1973 3301 
round(100*prop.table(table(df.data$bcg)),2)
##     0     1 
## 37.41 62.59 

## * main analysis
## ** fitted survival model
e.cox <- coxph(Surv(time, status=="dead") ~ factor(agem) + bcg,  data = df.data, x = TRUE )
logLik(e.cox)
## 'log Lik.' -1876.713 (df=7)

res.cox <- cbind(summary(e.cox)$coef["bcg",c("exp(coef)","Pr(>|z|)"),drop=FALSE],
                 exp(confint(e.cox))["bcg",,drop=FALSE])
colnames(res.cox) <- c("OR","p.value","lower","upper")
res.cox 
##            OR    p.value     lower    upper
## bcg 0.7066667 0.01744539 0.5307545 0.940883

## ** fitted survival curves
seqTime <- sort(unique(df.data$time))
ls.fitCox <- predictCox(e.cox, newdata = unique(df.data[,c("bcg","agem")]), type = "survival", times = seqTime, keep.newdata = TRUE,
                        se = TRUE, band = FALSE)
df.fitCox <- as.data.frame(as.data.table(ls.fitCox))

df.fitCox <- df.fitCox[order(df.fitCox$agem,df.fitCox$bcg),,drop=FALSE]
df.fitCox$survivalCI <- paste0(round(100*df.fitCox$survival,2),"[",round(100*df.fitCox$survival.lower,2),";",round(100*df.fitCox$survival.upper,2),"]")

dfW.fitCox <- reshape(df.fitCox[df.fitCox$times==183,c("bcg","agem","survival")],
                     direction = "wide", timevar = "bcg", idvar = "agem")

dfW.fitCox2 <- reshape(df.fitCox[df.fitCox$times==183,c("bcg","agem","survivalCI")],
                       direction = "wide", timevar = "bcg", idvar = "agem")
cbind(dfW.fitCox2, difference = round(100*(dfW.fitCox$survival.1 - dfW.fitCox$survival.0),2))
##      agem       survivalCI.0       survivalCI.1 difference
## 2253    0 95.01[93.13;96.39] 96.45[94.86;97.56]       1.44
## 2252    1    94.42[92.24;96] 96.02[94.47;97.15]       1.60
## 2243    2 96.12[94.16;97.43] 97.24[95.97;98.12]       1.12
## 2247    3  93.95[91.32;95.8] 95.68[93.99;96.91]       1.74
## 2254    4 93.11[90.33;95.12]  95.08[93.2;96.46]       1.97
## 2248    5 93.13[90.26;95.17] 95.09[93.09;96.53]       1.97
## 2250    6 95.06[90.67;97.42] 96.49[93.79;98.02]       1.42

## * sensitivity analysis
e.cox2 <- coxph(Surv(time, status=="dead") ~ pspline(agem) + bcg,  data = df.data, x = TRUE )
logLik(e.cox2)
## 'log Lik.' -1877.406 (df=5.090615)

res.cox2 <- cbind(exp(coef(e.cox2))["bcg"],
                  summary(e.cox2)$coef["bcg","p"],
                  exp(confint(e.cox2))["bcg",,drop=FALSE])
colnames(res.cox2) <- c("OR","p.value","lower","upper")
res.cox2
##           OR    p.value     lower     upper
## bcg 0.707524 0.01787094 0.5313596 0.9420929

## * secondary analysis
e.coxI <- coxph(Surv(time, status == "dead") ~ factor(agem) + bcg * dtp, data = df.data)

testI <- glht(e.coxI, linfct = c("bcg=0","dtp=0","bcg+dtp+bcg:dtp=0"))

cbind(OR = exp(coef(testI)),
      exp(confint(testI)$confint[,c("lwr","upr")]),
      p.value = summary(testI)$test$pvalues)
##                            OR       lwr       upr    p.value
## bcg                 0.5655749 0.3475224 0.9204439 0.01570103
## dtp                 1.1904099 0.2133455 6.6421655 0.99263289
## bcg + dtp + bcg:dtp 0.8321130 0.5488971 1.2614607 0.63619656


## * export
if("export" %in% ls() && export){
    saveRDS(e.cox, file = "data/analysis-main-cox.rds")
    saveRDS(df.fitCox, file = "data/analysis-main-fitcox.rds")
    saveRDS(e.cox2, file = "data/analysis-sensitivity-cox.rds")
}


## * old [not used]
if(FALSE){
    e.glm <- glm(status=="dead" ~ bcg + factor(agem), 
                 offset = log(time), family = poisson, data = df.data)
    ci.exp(e.glm)["bcg",] ## odds ratioa
    ## exp(Est.)      2.5%     97.5% 
    ## 0.7080062 0.5317787 0.9426343 
}
