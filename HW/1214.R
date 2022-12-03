library (haven)
library (data.table)

tscs212 <- read_dta("../../HW/tscs212.dta") |> setDT()
tscs212 <- tscs212 [a2y < 100,]
tscs212$age <- 110 - tscs212$a2y
tscs212$ageGroups <- cut(tscs212$age,
                         breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                         labels = c('小於10', '18-20', '21-30', '31-40', '41-50',
                                    '51-60', '61-70', '71-80', '81-90', '超過90'))
data1 <- tscs212[, .(age, ageGroups)]
