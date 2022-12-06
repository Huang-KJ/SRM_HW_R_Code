library(haven)
library(ggplot2)
library(data.table)
Tab <- function(data, var1, code, var2) {
    data <- as.data.frame(data)
    i <- which(names(data) %in% var1)
    j <- which(names(data) %in% var2)
    n <- nrow(data[data[, i] %in% code, ])
    tableA <- table(data[data[, i] %in% code, j])
    Table1 <- cbind(Freq. = tableA,
                    Percent = tableA / n * 100,
                    Cum. = cumsum(tableA) / n * 100) |> data.frame()
    Table1 <- rbind(Table1, Total = c(sum(Table1$Freq.), sum(Table1$Percent), ''))
    Table1[,c(1:ncol(Table1))] <- Table1[,c(1:ncol(Table1))] |> unlist() |> as.numeric()
    Table1 <- round(Table1, 3)
    return(Table1)
}
tscs161 <- read_dta('../../HW/tscs161.dta', encoding = 'big5')
indexName <- c('a1', paste0('d1', letters[1:6]), paste0('d2', letters[1:4]))

data1 <- tscs161[, indexName] |> setDT()
data1 <- data1[data1[, Reduce(`&`, lapply(.SD, `<=`, 5)), .SDcols = indexName], ..indexName]

data2 <- data1[a1 == 2]
data2$d2a <- car::recode(data2$d2a, '1=5;2=4;4=2;5=1')
data2$d2b <- car::recode(data2$d2b, '1=5;2=4;4=2;5=1')
data2$d2c <- car::recode(data2$d2c, '1=5;2=4;4=2;5=1')
data2$d2d <- car::recode(data2$d2d, '1=5;2=4;4=2;5=1')
data3 <- rbind(data1[a1 == 1], data2)

data3 <- data3[, MH := d1a + d1b + d1c + d1d + d1e + d1f] |> data.frame()

Tab(data3, 'MH', c(7:28), 'MH')
ggplot()+
    geom_bar(data = data3, aes(x = MH))

Tab(data3, 'd1a', 1, 'MH')
ggplot()+
    geom_line(data = data3, aes(x = MH), stat = "count")+
    geom_bar(data = data3[data3$d1a == 1, ], aes(x = MH))





#--------------------------------------------------#
reCols <- paste0('d2', letters[1:4])
data1[, c(8:11)] <- lapply(as.data.frame(data1)[, reCols],
                           function(x) {car::recode(x, '1=5;2=4;3=3;4=2;5=1;6=6;7=7;8=8;9=9')})
data1 <- data1[.(from = c(1:9), to = c(5:1, 6:9)), on = paste0(reCols, "==from"),  (reCols) := i.to]
