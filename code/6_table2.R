library(officer)
library(survival)
library(Epi)

digit <- 3

## * load data
e.cox <- readRDS("data/analysis-main-cox.rds")
e.cox2 <- readRDS("data/analysis-sensitivity-cox.rds")

## * create table 2
table2 <- data.frame(matrix(NA, nrow = 2, ncol = 3,
                     dimnames = list(c("Age (categorical)","Age (splines)"),c("OR","CI(OR)","p.value"))), check.names = FALSE)
table2["Age (splines)","OR"] <- round(ci.exp(e.cox2)["bcg",1], digits = digit)
table2["Age (splines)","CI(OR)"] <- paste(round(ci.exp(e.cox2)["bcg",2:3], digits = digit), collapse = ";")
table2["Age (splines)","p.value"] <- format.pval(summary(e.cox2)$coef["bcg","p"], digits = digit)

table2["Age (categorical)","OR"] <- round(exp(coef(e.cox)["bcg"]), digits = digit)
table2["Age (categorical)","CI(OR)"] <- paste(round(exp(confint(e.cox)["bcg",]), digits = digit), collapse = ";")
table2["Age (categorical)","p.value"] <- format.pval(summary(e.cox)$coef["bcg","Pr(>|z|)"], digits = digit)


## * export table 2
if("export" %in% ls() && export){
    table2.doc <- body_add_table(x = read_docx(), 
                                 value =  cbind(model = rownames(table2),table2), first_column = TRUE)
    print(table2.doc, target = "tables/table2.docx")
} 
