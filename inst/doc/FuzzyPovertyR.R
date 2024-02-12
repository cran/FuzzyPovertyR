## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(FuzzyPovertyR)
library(kableExtra)

## -----------------------------------------------------------------------------
data(eusilc)

## -----------------------------------------------------------------------------
hcr = HCR(predicate = eusilc$eq_income, weight = eusilc$DB090, p = 0.5, q = 0.6)$HCR # add poverty threshold

## -----------------------------------------------------------------------------
verma = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, ID = NULL,
                     HCR = hcr, interval = c(1,20), alpha = NULL, fm = "verma", verbose = FALSE)
verma$fm
class(verma)
summary(verma)
plot(verma)

## -----------------------------------------------------------------------------
verma = fm_construct(predicate = eusilc$eq_income, fm = "verma1999", weight = eusilc$DB090, ID = NULL, interval = c(1,10), alpha = 2)

## -----------------------------------------------------------------------------
head(verma$results)

## -----------------------------------------------------------------------------
verma.break = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, ID = NULL, HCR = hcr, interval = c(1,10), alpha = NULL, breakdown = eusilc$db040)

## -----------------------------------------------------------------------------
summary(verma.break)
plot(verma.break)
verma.break$estimate

## -----------------------------------------------------------------------------
alpha = verma$alpha
alpha

## -----------------------------------------------------------------------------
z1 = 20000; z2 = 70000; b = 2
belhadj = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "belhadj2015", z1 = z1, z2 = z2, b = b) 

## -----------------------------------------------------------------------------
summary(belhadj)
plot(belhadj)

## -----------------------------------------------------------------------------
z = 60000
chakravarty = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "chakravarty", z = z)

## -----------------------------------------------------------------------------
summary(chakravarty)
plot(chakravarty)

## -----------------------------------------------------------------------------
chakravarty.break = fm_construct(predicate = eusilc$eq_income, eusilc$DB090, fm = "chakravarty", z = z, breakdown = eusilc$db040)

## -----------------------------------------------------------------------------
knitr::kable(data.frame(verma.break$estimate, chakravarty.break$estimate), col.names = c("Verma", "Chakravarty"), digits = 4)

## -----------------------------------------------------------------------------
# eusilc = na.omit(eusilc)
step1 = eusilc[,4:23]

## -----------------------------------------------------------------------------
step2 = fs_transform(step1, weight = eusilc$DB090, ID = eusilc$ID); class(step2)
summary(step2$step2)
# step2.1 = fs_transform(step1, weight = eusilc$DB090, ID = eusilc$ID, depr.score = "d")

## -----------------------------------------------------------------------------
knitr::kable(head(step2$step2), digits = 3, align = "c", caption = "Transformed items")

## -----------------------------------------------------------------------------
dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)

## -----------------------------------------------------------------------------
steps4_5 = fs_weight(dimensions, step2 = step2, rho = NULL); class(steps4_5)
summary(steps4_5)
plot(steps4_5)

## -----------------------------------------------------------------------------
knitr::kable(head(steps4_5$steps4_5), digits = 4, caption = "Results from Steps 4 and 5.")

## -----------------------------------------------------------------------------
alpha = fs_equate(steps4_5 = steps4_5, weight = eusilc$DB090, HCR = hcr, interval = c(1,10))

## -----------------------------------------------------------------------------
FS = fs_construct(steps4_5 = steps4_5, weight = eusilc$DB090, alpha = alpha, breakdown = NULL) # no breakdown
summary(FS)

FS = fs_construct(steps4_5 = steps4_5, weight = eusilc$DB090, alpha = alpha, breakdown = eusilc$db040)
FS$estimate
plot(FS)

## -----------------------------------------------------------------------------
FS$estimate

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(fs_construct(steps4_5 = steps4_5, weight = eusilc$DB090, alpha = alpha, breakdown = eusilc$db040)$estimate, digits = 4 )

## -----------------------------------------------------------------------------
alpha = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, ID = NULL, HCR = 0.12, interval = c(1,10), alpha = NULL)$alpha

## -----------------------------------------------------------------------------
boot.var = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma",  type = "bootstrap", HCR = .12, alpha = alpha, verbose = F, R = 10)

# plot(boot.var)

fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", type = "jackknife", HCR = .12, alpha = 9, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(cbind(
  Bootstrap = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "bootstrap", HCR = hcr, alpha = alpha, verbose = FALSE)$variance,
  Jackknife = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "jackknife", HCR = hcr, alpha = alpha, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F))$variance, caption = 'Variance estimates for sub-domains')

## -----------------------------------------------------------------------------
Bootstrap = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "bootstrap", HCR = hcr, alpha = alpha, verbose = FALSE)

plot(Bootstrap)

Bootstrap = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "jackknife", HCR = hcr, alpha = alpha, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F)

## ----eval=FALSE---------------------------------------------------------------
#  variance = fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, dimensions = dimensions, breakdown = NULL, HCR = 0.12, alpha = 2, rho = NULL, type = 'bootstrap', M = NULL, R = 2, verbose = F)
#  summary(variance)
#  plot(variance)

## -----------------------------------------------------------------------------
Bootstrap = fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, 
                  dimensions = dimensions, breakdown = eusilc$db040, HCR = .12,
                   alpha = 2, rho = NULL, type = 'bootstrap', M = NULL, R = 100, verbose = F) 
plot(Bootstrap)

## ----eval=FALSE---------------------------------------------------------------
#  fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, dimensions = dimensions,
#         stratum = eusilc$stratum, psu = eusilc$psu, verbose = F, f = .01) %>% summary()
#         breakdown = eusilc$db040, HCR = .12, alpha = 2, rho = NULL, type = 'jackknife',

