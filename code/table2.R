library(officer)
library(survival)
library(Epi)

export <- FALSE
digit <- 3

## * set path
setwd("~/Github/article-template")

## * load data
e.cox <- readRDS(file.path("data","sensitivity-cox.rds"))
e.glm <- readRDS(file.path("data","sensitivity-glm.rds"))

## * create table 2
table2 <- data.frame(matrix(NA, nrow = 2, ncol = 3,
                     dimnames = list(c("Cox","Poisson"),c("OR","CI(OR)","p.value"))), check.names = FALSE)
table2["Poisson","OR"] <- round(ci.exp(e.glm)["bcg",1], digits = digit)
table2["Poisson","CI(OR)"] <- paste(round(ci.exp(e.glm)["bcg",2:3], digits = digit), collapse = ";")
table2["Poisson","p.value"] <- summary(e.glm)$coef["bcg","Pr(>|z|)"]

table2["Cox","OR"] <- round(exp(coef(e.cox)["bcg"]), digits = digit)
table2["Cox","CI(OR)"] <- paste(round(exp(confint(e.glm)["bcg",]), digits = digit), collapse = ";")
table2["Cox","p.value"] <- summary(e.cox)$coef["bcg","Pr(>|z|)"]

## * export table 2
table2.doc <- body_add_table(x = read_docx(), 
                             value =  table2)
print(table2.doc, target = file.path("tables","/table2.docx"))
 
