library(haven)
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
WVS <- read_dta("../../HW/WVS2019_TW.dta")
WVS <- WVS[, paste0('Q', 132:138)]
WVS <- lapply(WVS[, paste0('Q', 132:138)],
              function(x) {ifelse(!(x %in% c(1, 2, 3, 4)), NA, x)}) |> data.frame()
WVS <- na.omit(WVS)







