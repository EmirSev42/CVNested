library(prodlim)
library(ggplot2)
library(riskRegression)

# rm(list = ls())
# gc()

# ------------------------------------------------------------------------------
# GEN DATA
# ------------------------------------------------------------------------------
# lets generate some dummy data for our purpose
set.seed(42)
sim <- sampleData(10000, outcome = "survival")
sim <- sim %>% select(time, event, X1, X2, X3, X4, X5, X6, X7, X8)
sim %>% head()

# ------------------------------------------------------------------------------
# plotRisk ERROR
# ------------------------------------------------------------------------------

# two dummy models, what they are doesnt matter
cox_fit1 <- coxph(Surv(time, event) ~ X4 + X5 + X6 + X7,
                  data = sim, x = TRUE)

cox_fit2 <- coxph(Surv(time, event) ~ X1 + X2 + X3,
                  data = sim, x = TRUE)

x <- Score(object = list('coxph1' = cox_fit1,
                         'coxph2' = cox_fit2),
           formula = Hist(time, event)~1,
           data = sim,
           plots = 'Calibration',
           summary = 'risk',
           times = c(3,5))

summary(x, what = 'score')

# issue: when we speciy the names
try(plotRisk(x, models = c('coxph1', 'coxph2'), times = 5))

# Error in plotRisk(x, models = c("coxph1", "coxph2"), times = 5) : 
#   Fitted object does not contain models named: coxph1, coxph2
# Available models are named:

# this seems to stem out of a naming issue
# the code assigns 'pframe' as:

pframe <- x$risks$score[model != x$null.model]
pframe[, `:=`(model, factor(model))]
pframe %>% head()

# but then checks the model names as:
unique(pframe$models)
# which ends up being null, since the 
# models, with an as, even though it should be model, with no s as named earlier:
unique(pframe$model)

# I tried doing this to fix it:
x$risks$score <- x$risks$score %>% rename('models' = model)
unique(x$risks$score$models)

# but then the requested time is not detected
try(plotRisk(x, models = c('coxph1', 'coxph2'), times = 3))
# even though it is:
x$times


# ------------------------------------------------------------------------------
# plotCalibration ERROR
# ------------------------------------------------------------------------------

# have almost no events
sim$event <- 0
sim$event[c(1)] <- 1
table(sim$event)

# get score again
cox_fit1 <- coxph(Surv(time, event) ~ X4 + X5 + X6 + X7,
                  data = sim, x = TRUE)

cox_fit2 <- coxph(Surv(time, event) ~ X1 + X2 + X3,
                  data = sim, x = TRUE)

x <- Score(object = list('coxph1' = cox_fit1,
                         'coxph2' = cox_fit2),
           formula = Hist(time, event)~1,
           data = sim,
           plots = 'Calibration',
           summary = 'risk',
           times = c(3,5))

summary(x, what = 'score')

try(plotCalibration(x, cens.method = 'local', times = 5))
# Calibration Error (this only happens when there are very few events):
# Error in KernSmooth::dpik(cumtabx/N, kernel = "box") : 
#   scale estimate is zero for input data

# it seems to have to do with the decimal place rounding
# however, when events are rare, bars make more sense because they
# better represent predictions of low risk
# this should be elaborated in the paper




