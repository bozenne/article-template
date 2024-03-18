library(ggplot2)
library(survival)
library(officer)
library(Epi)
library(Publish)

## * load data
df.data <- read.csv("data/data-bissau-processed.csv")

## * create table 1
uniTable1 <- univariateTable(bcg~status+agem+dtp, data = df.data, compare.groups = FALSE)
capture.output(table1 <- print(uniTable1))

## * export table 1
if("export" %in% ls() && export){
    table1.doc <- body_add_table(x = read_docx(), 
                                 value = table1)
    print(table1.doc, target = "tables/table1.docx")
}
