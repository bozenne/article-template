rm(list=ls())
export <- TRUE

## * set path
setwd("~/Github/article-template")
## if(Sys.info()["login"] == "mycomputer1"){
##   setwd("~/Github/article-template")
## }else if(Sys.info()["login"] == "mycomputer2"){
##   setwd("~/article-template")
## }

## * clean directories
file.remove(list.files("data", full.names = TRUE))
file.remove(list.files("figures", full.names = TRUE))
file.remove(list.files("tables", full.names = TRUE))

## * run analysis
## note file.path("code","XXXX.R") is be more portable than "code/XXXX.R"

source("code/1_data-management.R")
system.time(
    source("code/2_analysis.R")
)
## bruger   system forløbet 
## 105.37     0.63   107.97 
source("code/3_figure1.R")
source("code/4_figure2.R")
source("code/5_table1.R")
source("code/6_table2.R")

