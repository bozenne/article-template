library(ggplot2)
library(riskRegression)
library(data.table)

## * load data
data2plot <- readRDS("data/analysis-main-fitcox.rds")
data2plot$bcg <- factor(data2plot$bcg, 0:1, c("no","yes"))
data2plot$agem.txt <- paste0("[",data2plot$agem,";",data2plot$agem+1,"[ month-old")

## * figure 2
figure2 <- ggplot(data2plot, aes(x = times, y = survival, group = bcg))
figure2 <- figure2 + geom_ribbon(aes(ymin = survival.lower, ymax = survival.upper, fill = bcg), alpha = 0.1) + geom_step(aes(color = bcg, linetype = bcg))
figure2 <- figure2 + facet_wrap(~agem.txt)
figure2 <- figure2 + theme(text = element_text(size=15), 
                           axis.line = element_line(linewidth = 1.25),
                           axis.ticks = element_line(linewidth = 2),
                           axis.ticks.length=unit(.25, "cm"),
                           legend.key.size = unit(3,"line"),
                           legend.position = "bottom")
figure2

## * export figure
if("export" %in% ls() && export){
    ggsave(figure2, filename = "figures/figure2.pdf", width = 7, height = 7)
    ggsave(figure2, filename = "figures/figure2.png", width = 7, height = 7)
}
