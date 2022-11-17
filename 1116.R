library(haven)
library(ggplot2)
library(data.table)
Tab <- function(data, var1, code, var2) {
    data <- as.data.frame(data)
    i <- which(names(data) == var1)
    j <- which(names(data) == var2)
    n <- nrow(data[data[, i] == code, ])
    tableA <- table(data[data[, i] == code, j])
    Table1 <- cbind(Freq. = tableA,
                    Percent = round(tableA / n * 100, 3),
                    Cum. = round(cumsum(tableA) / n * 100,  3)) |> data.frame()
    Table1 <- rbind(Table1, Total = c(sum(Table1$Freq.), sum(Table1$Percent), ''))
    return(Table1)
}

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
Table1 <- rbind(Table1, Total = c(sum(Table1$Freq.), sum(Table1$Percent), ''))
Table1

# e24c #
Tab(data1, 'e24c', 1, 'MH')
ggplot(data1[e24c == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24d #
Tab(data1, 'e24d', 1, 'MH')
ggplot(data1[e24d == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24e #
Tab(data1, 'e24e', 1, 'MH')
ggplot(data1[e24e == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24f #
Tab(data1, 'e24f', 5, 'MH')
ggplot(data1[e24f == 5, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24g #
Tab(data1, 'e24g', 1, 'MH')
ggplot(data1[e24g == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

#----------------------------#
#library(gmodels)
#with(data1, CrossTable(e24c))
