## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  highlight = TRUE,
  tidy = TRUE,
  comment = "#>"
)

## ---- echo=FALSE--------------------------------------------------------------
library(dplyr)
library(kableExtra)

## ----setup--------------------------------------------------------------------
library(FuzzyPovertyR)

## -----------------------------------------------------------------------------
data(eusilc)

## -----------------------------------------------------------------------------
hcr = HCR(predicate = eusilc$red_eq, weight = eusilc$DB090, p = 0.5, q = 0.6)$HCR # add poverty threshold

## -----------------------------------------------------------------------------
# eq_income

## -----------------------------------------------------------------------------
verma = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, ID = NULL,
                     HCR = 0.12, interval = c(1,10), alpha = NULL, fm = "verma")

## -----------------------------------------------------------------------------
verma = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, ID = NULL, interval = c(1,10), alpha = 2)

## -----------------------------------------------------------------------------
head(verma$results)

## -----------------------------------------------------------------------------
verma.break = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, ID = NULL, HCR = 0.12, interval = c(1,10), alpha = NULL, breakdown = eusilc$db040)

## -----------------------------------------------------------------------------
verma.break$estimate

## -----------------------------------------------------------------------------
alpha = verma$alpha
alpha

## -----------------------------------------------------------------------------
# hh.size = sample(1:4, 1000, replace = T, prob = c(4,3,2,1))
zbm = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "ZBM", hh.size = NULL)

## -----------------------------------------------------------------------------
zbm.break = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "ZBM", hh.size = NULL, breakdown = eusilc$db040)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(zbm.break$estimate, digits = 3, align = 'c', format = "html")

## -----------------------------------------------------------------------------
z1 = 500; z2 = 1700; b = 2
belhadj = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", z1 = z1, z2 = z2, b = b) # use id to return ordered as verma?

## -----------------------------------------------------------------------------
belhadj.break = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", z1 = z1, z2 = z2, b = b, breakdown = eusilc$db040) # use id to return ordered as verma?

## -----------------------------------------------------------------------------
z = 1500
chakravarty = fm_construct(predicate = eusilc$red_eq, eusilc$DB090, fm = "chakravarty", z = z)

## -----------------------------------------------------------------------------
chakravarty.break = fm_construct(predicate = eusilc$red_eq, eusilc$DB090, fm = "chakravarty", z = z, breakdown = eusilc$db040)

## -----------------------------------------------------------------------------
knitr::kable(data.frame(verma.break$estimate, belhadj.break$estimate, chakravarty.break$estimate), col.names = c("Verma", "Belhadj", "Chakravarty"), digits = 4)

## -----------------------------------------------------------------------------
verma2 = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, ID = NULL, HCR = 0.12, interval = c(1,20), alpha = NULL, fm = "verma2")

## -----------------------------------------------------------------------------
# eusilc = na.omit(eusilc)
step1 = eusilc[,4:23]

## -----------------------------------------------------------------------------
step2 = fs_transform(step1, weight = eusilc$DB090, ID = eusilc$ID)
# step2.1 = fs_transform(step1, weight = eusilc$DB090, ID = eusilc$ID, depr.score = "d")

## -----------------------------------------------------------------------------
knitr::kable(head(step2), digits = 3, align = "c", caption = "Transformed items")

## -----------------------------------------------------------------------------
dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)

## -----------------------------------------------------------------------------
steps4_5 = fs_weight(dimensions, step2 = step2, rho = NULL)

## -----------------------------------------------------------------------------
knitr::kable(head(steps4_5), digits = 4, caption = "Results from Steps 4 and 5.")

## -----------------------------------------------------------------------------
alpha = fs_equate(steps4_5 = steps4_5, weight = eusilc$DB090, HCR = .16, interval = c(1,10))

## -----------------------------------------------------------------------------
# FS = fs_construct(steps4_5 = steps4_5, weight = eusilc$DB090, alpha = alpha, breakdown = NULL) # no breakdown
FS = fs_construct(steps4_5 = steps4_5, weight = eusilc$DB090, alpha = alpha, breakdown = eusilc$db040)
FS$estimate

## ----eval=FALSE---------------------------------------------------------------
#  FS$membership$FS1

## -----------------------------------------------------------------------------
FS$estimate

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(fs_construct(steps4_5 = steps4_5, weight = eusilc$DB090, alpha = alpha, breakdown = eusilc$db040)$estimate, digits = 4 )

## -----------------------------------------------------------------------------
alpha = fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090, ID = NULL, HCR = 0.12, interval = c(1,10), alpha = NULL)$alpha

## -----------------------------------------------------------------------------
fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "verma",  type = "bootstrap", HCR = .14, alpha = alpha, verbose = F)
fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "verma", type = "jackknife", HCR = .14, alpha = 9, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(cbind(
  Bootstrap = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "bootstrap", HCR = .14, alpha = alpha, verbose = F),
  Jackknife = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "jackknife", HCR = .14, alpha = alpha, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F)$estimate), digits = 4, caption = 'Variance estimates for sub-domains')

## -----------------------------------------------------------------------------
z1 = 1000; z2 = 2000; b = 2
fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", breakdown = NULL, type = "bootstrap", z1 = z1, z2 = z2, b = b)

## ----message = FALSE----------------------------------------------------------
Bootstrap.zbm = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "ZBM", breakdown = eusilc$db040, type = "bootstrap", hh.size = NULL, verbose = F, R = 5)
Jackknife.zbm = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "ZBM", breakdown = eusilc$db040, type = "jackknife", hh.size = NULL, stratum = eusilc$stratum, psu = eusilc$psu)$estimate

## ----echo=FALSE---------------------------------------------------------------
kbl(data.frame(Bootstrap.zbm, Jackknife.zbm), digits = 4, col.names = rep(1:3,2)) %>% kableExtra::add_header_above(c(" ", "Bootstrap" = 3, "Jackknife" = 3))

## -----------------------------------------------------------------------------
fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", type = "bootstrap", z1 = z1, z2 = z2, b = b)
fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", type = "jackknife", z1 = z1, z2 = z2, b = b, stratum = eusilc$stratum, psu = eusilc$psu)$estimate

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(cbind(
  Bootstrap = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", breakdown = eusilc$db040, type = "bootstrap", z1 = z1, z2 = z2, b = b), 
  Jackknife = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", breakdown = eusilc$db040, type = "jackknife", z1 = z1, z2 = z2, b = b, stratum = eusilc$stratum, psu = eusilc$psu)$estimate))

## -----------------------------------------------------------------------------
fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "chakravarty", breakdown = NULL, type = "bootstrap", z = 2000)

## ----echo=FALSE---------------------------------------------------------------
cbind(
  Bootstrap = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "chakravarty", breakdown = eusilc$db040, type = "bootstrap", z = 2000),
  Jackknife = fm_var(predicate = eusilc$red_eq, weight = eusilc$DB090, fm = "chakravarty", breakdown = eusilc$db040, type = "jackknife", z = 2000, stratum = eusilc$stratum, psu = eusilc$psu)$estimate)

## ----eval=FALSE---------------------------------------------------------------
#  fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, dimensions = dimensions, breakdown = NULL, HCR = .16, alpha = alpha, rho = NULL, type = 'bootstrap', M = NULL, R = 2, verbose = F)$variance %>% t() %>% knitr::kable(digits = 4)

## -----------------------------------------------------------------------------
fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, dimensions = dimensions, breakdown = eusilc$db040, HCR = .16, alpha = alpha, rho = NULL, type = 'bootstrap', M = NULL, R = 50, verbose = F)$variance %>% knitr::kable(digits = 4)

## ----eval=FALSE---------------------------------------------------------------
#  fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, dimensions = dimensions,
#         breakdown = eusilc$db040, HCR = .16, alpha = alpha, rho = NULL, type = 'jackknife',
#         stratum = eusilc$stratum, psu = eusilc$psu, verbose = F, f = .01)$variance %>% knitr::kable(digits = 4)

