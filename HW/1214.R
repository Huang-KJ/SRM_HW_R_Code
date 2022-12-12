library(dplyr)
library(haven)
library(data.table)
options(digits = 3)

tscs212 <- read_dta('../../HW/tscs212.dta') |> setDT()
tscs212 <- as_factor(tscs212, levels = "default")

# 年齡-全國 #
table(tscs212$a2y)
ageSet <- tscs212 [!(a2y %in% c('拒答', '不知道')), ]
ageSet[, a2y := as.numeric(as.character(a2y))][, age := 110 - a2y]
ageSet$age <- 110 - as.numeric(ageSet$a2y)
ageSet <- ageSet[age >= 20, ]
ageSet$ageGroups <- cut(ageSet$age,
                        breaks = c(-Inf, 29, 39, 49, 59, 69, 79, 89, 99, Inf),
                        labels = c('20-29', '30-39', '40-49', '50-59', '60-69',
                                   '70-79', '80-89', '90-99', '100以上'))
prop.table(table(ageSet$ageGroups)) * 100
# 年齡-縣市 #
ageSet <- (ageSet[a4acity %in% c('臺北市', '臺中市', '高雄市'), .(groupsCount = .N), by = .(a4acity, ageGroups)]
                 [, groupsFreq := groupsCount / sum(groupsCount), by = .(a4acity)]
                 [, .(groupsFreq = groupsFreq * 100), keyby = .(a4acity, ageGroups)])
dcast(ageSet, a4acity ~ ageGroups, value.var = 'groupsFreq')

# 投票-全國 #
table(tscs212$j28)
opt <- (table(tscs212$j28) |> data.frame())[, 1]
voteSet <- tscs212 [!(j28 %in% opt[7:10]), ]
voteSet[, .(投票率 = sum(j28 %in% opt[1:5]) / sum(j28 %in% opt[1:6]),
            宋楚瑜得票率 = sum(j28 %in% opt[1]) / sum(j28 %in% opt[1:5]),
            韓國瑜得票率 = sum(j28 %in% opt[2]) / sum(j28 %in% opt[1:5]),
            蔡英文得票率 = sum(j28 %in% opt[3]) / sum(j28 %in% opt[1:5]))]
# 投票-縣市 #
(voteSet[a4acity %in% c('臺北市', '臺中市', '高雄市'),
         .(投票率 = sum(j28 %in% opt[1:5]) / sum(j28 %in% opt[1:6]),
           宋楚瑜得票率 = sum(j28 %in% opt[1]) / sum(j28 %in% opt[1:5]),
           韓國瑜得票率 = sum(j28 %in% opt[2]) / sum(j28 %in% opt[1:5]),
           蔡英文得票率 = sum(j28 %in% opt[3]) / sum(j28 %in% opt[1:5])), by = .(a4acity)])

data.frame(縣市 = c('臺北市', '臺中市', '高雄市'),
           投票率 = c(76.31, 76.36, 77.44),
           宋楚瑜得票率 = c(4.34, 4.99, 3.14), 
           韓國瑜得票率 = c(42.01, 38.06, 34.63), 
           蔡英文得票率 = c(53.64, 56.95, 62.23))






prop.table(table(ageSet[a4acity == '臺北市', .(ageGroups)]))
prop.table(table(ageSet[a4acity == '臺中市', .(ageGroups)]))
prop.table(table(ageSet[a4acity == '高雄市', .(ageGroups)]))
county <- c('基隆市', '台北市', '新北市', '桃園縣', '新竹市', '新竹縣', '苗栗縣',
            '台中市', '彰化縣', '南投縣', '雲林縣', '嘉義市', '嘉義縣', '台南市',
            '高雄市', '屏東縣', '台東縣', '花蓮縣', '宜蘭縣', '澎湖縣', '金門縣', '連江縣')
#tscs212$a4acity <- factor(tscs212$a4acity, levels = c(1:22), labels = county)