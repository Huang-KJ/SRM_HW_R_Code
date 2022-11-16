library(haven)
library(ggplot2)
library(data.table)
tscs212 <- read_dta("~/Desktop/NCCU/111-1/三1234社會研究方法/HW/tscs212.dta") |> setDT()
tscs212 <- (tscs212[!(e24c > 5 | e24d > 5 | e24e > 5), ][, ":="(MH = e24c + e24d + e24e)])
cor(tscs212[, .(e24c, e24d, e24e)])
data1 <- tscs212[, .(e24c, e24d, e24e, e24f, e24g, MH)]
table(data1[e24c==1, .(MH = as.factor(MH))])
ggplot(data1[e24c==1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()
