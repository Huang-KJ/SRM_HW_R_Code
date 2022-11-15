# Import SPSS(SAV format) file #
library(haven)
tscs152 <- read_sav("~/Desktop/NCCU/111-1/三1234社會研究方法/HW/tscs152.sav")
View(tscs152)

# Import Stata(DTA format) file #
library(haven)
tscs152 <- read_dta("~/Desktop/NCCU/111-1/三1234社會研究方法/HW/tscs152.dta", encoding='big-5')
View(tscs152)