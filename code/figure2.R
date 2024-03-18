library(ggplot2)

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

## * prepare figure
df.data0 <- df.data
df.data0$time <- 0

## * generate figure
figure1 <- ggplot(mapping = aes(x = time, y = id))
figure1 <- figure1 + geom_line(data = rbind(df.data0,df.data), aes(group = id))
figure1 <- figure1 + geom_point(data = df.data, aes(color = status, shape = status))
figure1 <- figure1 + facet_grid(bcg~dtp, labeller = label_both)
figure1 <- figure1 + theme(text = element_text(size=15), 
                           axis.line = element_line(linewidth = 1.25),
                           axis.ticks = element_line(linewidth = 2),
                           axis.ticks.length=unit(.25, "cm"),
                           legend.key.size = unit(3,"line"),
                           legend.position = "bottom")
figure1

## * export figure
if(export){
    ggsave(figure1, filename = file.path("figures","figure1.pdf"), width = 5, height = 5)
    ggsave(figure1, filename = file.path("figures","figure1.png"), width = 5, height = 5)
}
