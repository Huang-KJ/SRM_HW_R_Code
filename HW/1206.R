library(haven)
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
