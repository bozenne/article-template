library(riskRegression)
library(data.table)
library(multcomp)
library(ggplot2)
library(survival)
library(Epi)

## * load data
df.data <- read.csv("data/data-bissau-processed.csv")

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
                           se = TRUE, band = TRUE)
df.fitCox <- as.data.table(ls.fitCox)


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


## * sensitivity analysis
e.glm <- glm(status=="dead" ~ bcg + factor(agem), 
             offset = log(time), family = poisson, data = df.data)
ci.exp(e.glm)["bcg",] ## odds ratioa
## exp(Est.)      2.5%     97.5% 
## 0.7080062 0.5317787 0.9426343 

## * export
if("export" %in% ls() && export){
    saveRDS(e.cox, file = "data/analysis-main-cox.rds")
    saveRDS(df.fitCox, file = "data/analysis-main-fitcox.rds")
    saveRDS(e.glm, file = "data/analysis-sensitivity-glm.rds")
}
