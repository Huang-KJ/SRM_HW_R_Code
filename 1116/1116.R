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
                    Percent = tableA / n * 100,
                    Cum. = cumsum(tableA) / n * 100) |> data.frame()
    Table1 <- rbind(Table1, Total = c(sum(Table1$Freq.), sum(Table1$Percent), ''))
    Table1[,c(1:ncol(Table1))] <- Table1[,c(1:ncol(Table1))] |> unlist() |> as.numeric()
    Table1 <- round(Table1, 3)
    return(Table1)
}

tscs212 <- read_dta("../../HW/tscs212.dta") |> setDT()
tscs212 <- (tscs212[!(e24a > 5 | e24b > 5 | e24c > 5 | e24d >  5 | e24e > 5 | e24f > 5 | e24g > 5 | e24h > 5), ]
                   [, ":="(MH = e24a + e24b + e24c+ e24d + e24e)])
data1 <- tscs212[, .(e24a, e24b, e24c, e24d, e24e, e24f, e24g, e24h, MH = as.factor(MH))]

cor(data1[, .(e24a, e24b, e24c, e24d, e24e)])

# MH #
tableA <- table(data1[, MH])
tableB <- cumsum(tableA)
Table1 <- cbind(Freq. = tableA,
                Percent = round(tableA / nrow(data1) * 100, 2),
                Cum. = round(tableB / nrow(data1) * 100, 2)) |> data.frame()
Table1 <- rbind(Table1, Total = c(sum(Table1$Freq.), sum(Table1$Percent), ''))
Table1

# e24a #
Tab(data1, 'e24a', 1, 'MH')
ggplot(data1[e24a == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

# e24b #
Tab(data1, 'e24b', 1, 'MH')
ggplot(data1[e24b == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

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

# e24h #
Tab(data1, 'e24h', 1, 'MH')
ggplot(data1[e24h == 1, .(MH = as.factor(MH))], aes(MH))+
    geom_bar()

#----------------------------#
#library(gmodels)
#with(data1, CrossTable(e24c))
#knitr::stitch_rmd("1116.R")
