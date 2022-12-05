library(haven)
library(dplyr)
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
tscs161 <- read_dta('../../HW/tscs161.dta', encoding = 'big5')
indexName <- c('a1', paste0('d1', letters[1:6]), paste0('d2', letters[1:4]))

reCols <- paste0('d2', letters[1:4])
data1 <- tscs161[, indexName] |> setDT()
data1 <- data1[data1[, Reduce(`&`, lapply(.SD, `<`, 10)), .SDcols = indexName], ..indexName]
#table(data1$d2a)

data1 <- data1[.(from = c(1:9), to = c(5:1,6:9)), on = paste0(reCols, "==from"),  (reCols) := i.to]

data1[, c(8:11)] <- lapply(as.data.frame(data1)[, reCols],
                           function(x) {car::recode(x, '1=5;2=4;3=3;4=2;5=1;6=6;7=7;8=8;9=9')})
data1 <- data1[a1 == 2, (reCols) := car::recode(factor(.SD), '1=5;2=4;3=3;4=2;5=1;6=6;7=7;8=8;9=9'), .SDcols = reCols]


my_var_name <- "V2"
DT[.(from = LETTERS[1:3], to = c("T", "K", "D")), on = paste0(my_var_name, "==from"), 
   (my_var_name) := i.to]


