library(haven)
library(dplyr)
library(ggplot2)
library(data.table)

tscs212 <- read_dta("~/Desktop/NCCU/111-1/三1234社會研究方法/HW/tscs212.dta") |> setDT()
tscs212 <- (tscs212[!(e24c > 5 | e24d > 5 | e24e > 5), ][, ":="(MH = e24c + e24d + e24e)])

cor(tscs212[, .(e24c, e24d, e24e)])
data1 <- tscs212[, .(e24c, e24d, e24e, e24f, e24g, MH = as.factor(MH))]

# MH #
tableA <- table(data1[, MH])
tableB <- cumsum(tableA)
Table1 <- cbind(Freq. = tableA,
                Percent = round(tableA / nrow(data1) * 100, 2),
                Cum. = round(tableB / nrow(data1) * 100, 2)) |> data.frame()
Table1 <- rbind(Table1, Total = c(sum(Table1$Freq.), sum(Table1$Percent),''))
Table1

# e24c #
tableA <- table(data1[e24c == 1, MH])
tableB <- cumsum(tableA)
Table1 <- cbind(Freq. = tableA,
                Percent = tableA / nrow(data1[e24c == 1]) * 100,
                Cum. = tableB / nrow(data1[e24c == 1]) * 100) |> data.frame()
Table1
ggplot(data1[e24c == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24d #
tableA <- table(data1[e24d == 1, MH])
tableB <- cumsum(tableA)
Table1 <- cbind(Freq. = tableA,
                Percent = tableA / nrow(data1[e24d == 1]) * 100,
                Cum. = tableB / nrow(data1[e24d == 1]) * 100) |> data.frame()
Table1
ggplot(data1[e24d == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24e #
tableA <- table(data1[e24e == 1, MH])
tableB <- cumsum(tableA)
Table1 <- cbind(Freq. = tableA,
                Percent = tableA / nrow(data1[e24e == 1]) * 100,
                Cum. = tableB / nrow(data1[e24e == 1]) * 100) |> data.frame()
Table1
ggplot(data1[e24e == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24f #
tableA <- table(data1[e24f == 5, MH])
tableB <- cumsum(tableA)
Table1 <- cbind(Freq. = tableA,
                Percent = tableA / nrow(data1[e24f == 5]) * 100,
                Cum. = tableB / nrow(data1[e24f == 5]) * 100) |> data.frame()
Table1
ggplot(data1[e24f == 5, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24g #
tableA <- table(data1[e24g == 1, MH])
tableB <- cumsum(tableA)
Table1 <- cbind(Freq. = tableA,
                Percent = tableA / nrow(data1[e24g == 1]) * 100,
                Cum. = tableB / nrow(data1[e24g == 1]) * 100) |> data.frame()
Table1
ggplot(data1[e24g == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()


#----------------------------------------#
library(gmodels)
with(data1, CrossTable(e24c))
Tab <- function(data, var1, code, var2) {
    table1 <- table(data[data$var1 == code, data$var2]) |> data.frame()
    table2 <- (cumsum(table1[, 2]) / nrow(data[data$var1 == code,])) |> data.frame()
    table3 <- data.frame(table1,
                         table1[, 2] / nrow(data[data$var1 == code, ]), 
                         table2)
    names(table3)[1:4] <- c(var2, 'Freq.', 'Percent', 'Cum.')
}
