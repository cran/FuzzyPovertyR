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

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
verma = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, ID = NULL,
                     HCR = hcr, interval = c(1,20), alpha = NULL, fm = "verma", verbose = FALSE)
verma$fm
class(verma)
summary(verma)
plot(verma) 

## -----------------------------------------------------------------------------
verma = fm_construct(predicate = eusilc$eq_income, fm = "verma", weight = eusilc$DB090, ID = NULL, interval = c(1,10), alpha = 2)

## -----------------------------------------------------------------------------
head(verma$results)

## -----------------------------------------------------------------------------
verma.break = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, ID = NULL, HCR = hcr, interval = c(1,10), alpha = NULL, breakdown = eusilc$db040, 
fm="verma")

## -----------------------------------------------------------------------------
summary(verma.break)
verma.break$estimate

## -----------------------------------------------------------------------------
alpha = verma$parameters$alpha
alpha

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
verma1999 = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, ID =                         NULL, HCR = hcr, interval = c(1,20), alpha = NULL, fm =                             "verma1999", verbose = FALSE)
verma1999$fm
class(verma1999)
summary(verma1999)
plot(verma1999)

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
TFR = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, ID = NULL,
                     HCR = hcr, interval = c(1,20), alpha = NULL, fm = "TFR", verbose = FALSE)
TFR$fm
class(TFR)
summary(TFR)
plot(TFR)

## -----------------------------------------------------------------------------
z1 = 20000; z2 = 70000; b = 2
belhadj = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "belhadj2015", z1 = z1, z2 = z2, b = b) 

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
summary(belhadj)
plot(belhadj)

## -----------------------------------------------------------------------------
z1 = 10000; z2 = 70000
cerioli = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "cerioli", z1 = z1, z2 = z2) 

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
summary(cerioli)
plot(cerioli)

## -----------------------------------------------------------------------------
z = 60000
chakravarty = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "chakravarty", z = z)

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
summary(chakravarty)
plot(chakravarty)

## -----------------------------------------------------------------------------
chakravarty.break = fm_construct(predicate = eusilc$eq_income, eusilc$DB090, fm = "chakravarty", z = z, breakdown = eusilc$db040)

## -----------------------------------------------------------------------------
knitr::kable(data.frame(verma.break$estimate, chakravarty.break$estimate), col.names = c("Verma", "Chakravarty"), digits = 4)

## -----------------------------------------------------------------------------
zmin = 5000; zmax = 60000

belhadj2011 = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, fm =                           "belhadj2011", z_min = zmin, z_max = zmax)

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
summary(belhadj2011)
plot(belhadj2011)

## -----------------------------------------------------------------------------

ZBM = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090, fm =                           "ZBM", hh.size = eusilc$ncomp)

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
plot(ZBM)

## -----------------------------------------------------------------------------
# eusilc = na.omit(eusilc)
step1 = eusilc[,4:23]

## -----------------------------------------------------------------------------
#Create a dataframe in which the variable X is not ordered in the right way:
data=data.frame("X"=rep(c(1,2,3,4),20), "Y"=rep(c(7,8,9,1),20))

#Crete vec_order a vector of length n with TRUE or FALSE. True if the order of the variable is to be inverted, False otherwise

vec_order=c(TRUE,FALSE)

head(fs_order(data=data, vec_order))

## -----------------------------------------------------------------------------
step2 = fs_transform(step1, weight = eusilc$DB090, ID = eusilc$ID); class(step2)
summary(step2$step2)
# step2.1 = fs_transform(step1, weight = eusilc$DB090, ID = eusilc$ID, depr.score = "d")

## -----------------------------------------------------------------------------
knitr::kable(head(step2$step2), digits = 3, align = "c", caption = "Transformed items")

## -----------------------------------------------------------------------------
dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
steps4_5 = fs_weight(dimensions, step2 = step2, rho = NULL); class(steps4_5)
summary(steps4_5)
plot(steps4_5)

## -----------------------------------------------------------------------------
knitr::kable(head(steps4_5$steps4_5), digits = 4, caption = "Results from Steps 4 and 5.")

## -----------------------------------------------------------------------------
alpha = fs_equate(steps4_5 = steps4_5, weight = eusilc$DB090, HCR = hcr, interval = c(1,10))

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
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
boot.var = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma",  type = "bootstrap_naive", HCR = .12, alpha = alpha, verbose = F, R = 10)

# plot(boot.var)

fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", type = "jackknife", HCR = .12, alpha = 9, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F)

## ----echo=FALSE---------------------------------------------------------------

Bootstrap = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "bootstrap_naive", HCR = hcr, alpha = alpha, verbose = FALSE) %>% summary()

Jackknife = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "jackknife", HCR = hcr, alpha = alpha, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F)%>% summary()

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
Bootstrap = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "bootstrap_naive", HCR = hcr, alpha = alpha, verbose = FALSE)

plot(Bootstrap)

Bootstrap = fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "jackknife", HCR = hcr, alpha = alpha, stratum = eusilc$stratum, psu = eusilc$psu, verbose = F)

## ----eval=FALSE---------------------------------------------------------------
#  variance = fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL,
#                    dimensions = dimensions, breakdown = NULL, HCR = 0.12,
#                    alpha = 2, rho = NULL, type = 'bootstrap_naive',
#                    M = NULL, R = 50, verbose = F)
#  summary(variance)

## ----echo=FALSE, fig.height=6, fig.width=6------------------------------------
Bootstrap = fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, 
                  dimensions = dimensions, breakdown = eusilc$db040, HCR = .12,
                   alpha = 2, rho = NULL, 
                  type = 'bootstrap_naive', M = NULL, R = 10, verbose = F) 
plot(Bootstrap)

## ----eval=FALSE---------------------------------------------------------------
#  fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL, dimensions = dimensions,
#         stratum = eusilc$stratum, psu = eusilc$psu, verbose = F, f = .01,
#         breakdown = NULL, alpha = 3, rho = NULL, type = "jackknife", fixed = T)%>%summary()

