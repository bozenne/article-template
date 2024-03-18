library(ggplot2)
library(survival)
library(Epi)
library(Publish)

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

## * create table 1
table1 <- univariateTable(bcg~status+agem+dtp, data = df.data, compare.groups = FALSE)

## * export table 1
table1.doc <- body_add_table(x = read_docx(), 
                             value =  print(table1))
print(table1.doc, target = file.path("tables","/table1.docx"))
