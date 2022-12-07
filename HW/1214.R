library(haven)
library(dplyr)
library(data.table)

tscs212 <- read_dta("../../HW/tscs212.dta") |> setDT()
tscs212 <- tscs212 [a2y < 100,]
tscs212$age <- 110 - tscs212$a2y
tscs212$ageGroups <- cut(tscs212$age,
                         breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                         labels = c('小於10', '18-20', '21-30', '31-40', '41-50',
                                    '51-60', '61-70', '71-80', '81-90', '超過90'))

tscs212 <- filter(tscs212, a4acity <= 22)
tscs212$a4acity <- factor(tscs212$a4acity, levels = c(1:22),
                          labels = c('基隆市', '台北市', '新北市', '桃園縣', '新竹市',
                                     '新竹縣', '苗栗縣', '台中市', '彰化縣', '南投縣',
                                     '雲林縣', '嘉義市', '嘉義縣', '台南市', '高雄市',
                                     '屏東縣', '台東縣', '花蓮縣', '宜蘭縣', '澎湖縣',
                                     '金門縣', '連江縣'))
data1 <- tscs212[, .(age, ageGroups, a4acity)]
