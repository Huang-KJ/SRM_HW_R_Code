library(haven)
library(psych)
library(data.table)
options(digits = 2)

tscs212 <- read_dta("../../HW/tscs212.dta") |> setDT()

data1 <- tscs212[!(e24a > 5 | e24b > 5 | e24c > 5 | e24d >  5 | e24e > 5 | e24f > 5 | e24g > 5 | e24h > 5),
                 .(e24a, e24b, e24c, e24d, e24e, e24f, e24g, e24h)]
table(data1$e24f)
data1$e24f <- car::recode(data1$e24f, "1=5;2=4;3=3;4=2;5=1")
table(data1$e24f)

data1 <- tscs212[!(e24a > 5 | e24b > 5 | e24c > 5 | e24d >  5 | e24e > 5),
                 .(e24a, e24b, e24c, e24d, e24e)]

data1.cor <- cor(data1[, .(e24a, e24b, e24c, e24d, e24e)])

fa.parallel(data1.cor, n.obs = nrow(data1), fa = "both",
            main = "Scree plots with parallel analysis")

factanal(data1, factors = 2, rotation = "varimax")
data1.fa <- fa(data1, nfactors = 2, rotate = 'none', fm = 'pa', max.iter = 1)
data1.fa$uniquenesses

fa(data1.cor, nfactors = 2, rotate = "none", fm = "pa")

fa(data1.cor, nfactors = 2, rotate = "varimax", fm = "pa")

data1.cor.promax <- fa(data1.cor, nfactors = 2, rotate = "promax", fm = "pa")

fsm <- function(oblique) {
    if (class(oblique)[2] == "fa" & is.null(oblique$Phi)) {
        warning("Object doesn't look like oblique EFA")
    } else {
        P <- unclass(oblique$loading)
        F <- P %*% oblique$Phi
        colnames(F) <- c("PA1", "PA2")
        return(F)
    }
}

fsm(data1.cor.promax)

factor.plot(data1.cor.promax, labels=rownames(data1.cor.promax$loadings))

fa.diagram(data1.cor.promax, simple=FALSE)

data1.cor.promax$weights
