library(testthat)

## * load data
df.data <- read.table("source/bissau.txt", header = TRUE)

## * process data

## ** only keep relevant columns
df.data.red <- df.data[,c("id","fuptime","dead","bcg","dtpany","age","agem")]

## ** uniformize name and format
df.data.red$dtpany <- as.numeric(df.data.red$dtpany)
df.data.red$bcg <- as.numeric(df.data.red$bcg=="yes")
df.data.red$dead <- factor(df.data.red$dead, c("censored","dead"))

## ** rename column
old2new <- c("fuptime" = "time", "dead" = "status", "dtpany" = "dtp")
names(df.data.red)[match(names(old2new),names(df.data.red))] <- old2new

## ** reshape data 
dfC.data.red <- aggregate(cbind(dead = status=="dead", censored = status=="censored") ~ bcg + dtp + agem, data = df.data.red, FUN = sum)
dfC.data.red$n <- dfC.data.red$dead + dfC.data.red$censored

## * quality check
expect_equal(sum(is.na(df.data.red)),0) ## no missing values
expect_true(all(df.data.red$bcg %in% 0:1)) ## bcg is a binary variable
expect_true(all(df.data.red$dtp %in% 0:1)) ## bcg is a binary variable
expect_true(all(df.data.red$time>0)) ## time variable is strictly positive

## * export
if("export" %in% ls() && export){
    write.csv(df.data.red, file = "data/data-bissau-processed.csv", row.names = FALSE)
    write.csv(dfC.data.red, file = "data/data-bissau-aggregated.csv", row.names = FALSE)
}
