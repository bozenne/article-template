library(ggplot2)
library(survival)
library(Epi)

## * set path
setwd("~/Github/article-template")

## if(Sys.info()["login"] == "mycomputer1"){
##   setwd("~/Github/article-template")
## }else if(Sys.info()["login"] == "mycomputer2"){
##   setwd("~/article-template")
## }
export <- FALSE

## * load data
df.data <- read.csv("data/bissau-processed.csv")

## * main analysis
e.cox <- coxph(Surv(time, status=="dead") ~ factor(agem) + bcg,  data = df.data, x = TRUE )
logLik(e.cox)
## 'log Lik.' -1876.713 (df=7)

res.cox <- cbind(summary(e.cox)$coef["bcg",c("exp(coef)","Pr(>|z|)"),drop=FALSE],
                 exp(confint(e.cox))["bcg",,drop=FALSE])
colnames(res.cox) <- c("OR","p.value","lower","upper")
res.cox 
##            OR    p.value     lower    upper
## bcg 0.7066667 0.01744539 0.5307545 0.940883

## * secondary analysis
e.cox <- coxph(Surv(time, status == "dead") ~ factor(agem) + bcg + dtpany, data = bissau )
summary(e.coxTime2)

library(multcomp)
e.glht <- glht(e.coxTime3, 
               linfct = c("bcgyes=0",
                          "dtpanyTRUE=0",
                          "bcgyes+dtpanyTRUE+bcgyes:dtpanyTRUE=0"))
summary(e.glht)


## * sensitivity analysis
e.glm <- glm(status=="dead" ~ bcg + factor(agem), 
             offset = log(time), family = poisson, data = df.data)
ci.exp(e.glm)["bcg",] ## odds ratioa
## exp(Est.)      2.5%     97.5% 
## 0.7080062 0.5317787 0.9426343 

## * export
if(export){
    saveRDS(e.cox, file = file.path("data","sensitivity-cox.rds"))
    saveRDS(e.glm, file = file.path("data","sensitivity-glm.rds"))
}
