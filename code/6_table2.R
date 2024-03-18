library(officer)
library(survival)
library(Epi)

digit <- 3

## * load data
e.cox <- readRDS("data/analysis-main-cox.rds")
e.glm <- readRDS("data/analysis-sensitivity-glm.rds")

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
if("export" %in% ls() && export){
    table2.doc <- body_add_table(x = read_docx(), 
                                 value =  table2)
    print(table2.doc, target = "tables/table2.docx")
} 
