### nested CV for discrete SL
### three steps
### Step 1, find learner with lowest Brier 
# if available from temporal training/testing skip Step 1
### Step 2, run CV10 (once) to test winner against benchmark
# or re-trained SL vs available SL
### Step 3, train dSL on all data
### This example assumes a single prediction time horizon (Brier)
# For multiple times rewrite considering for the lowest IBS at max(times)
library(dplyr)
library(data.table)
library(knitr)
library(riskRegression)
library(survival)
library(probably)
library(ggplot2)

set.seed(7)
sim <- sampleData(100,outcome="competing.risks")
sim <- sim %>% select(time, event, X1, X2, X3, X4, X5, X6, X7, X8)
sim %>% head()

sim %>% kable(., digits = 3, caption = "Simulated data set")
# kable(head(sim), digits=3, caption = "Simulated data set")

# benchmark or reference model (assume it was trained using some existing data)
ref <- CSC(Hist(time,event) ~ X1 + X2 + X7 + X8, data = sim)

### design many experimental models; here 2 to keep it simple
int <- CSC(Hist(time,event) ~ X4 + X5 + X6 * X7, data=sim)
lin <- CSC(Hist(time,event) ~ X4 + X5 + X6 + X7, data=sim)
bl_lib <- list('int' = int, 'lin' = lin)

### new study data
df <- sampleData(1000, outcome = "competing.risks")

df <- df %>% select(time, event, X1, X2, X4, X5, X6, X7, X8) %>% mutate(id = 1:nrow(df))

k <- 10 # 10 fold cv
cv_index <- sample(rep(1:k, each = nrow(df)/k))
table(cv_index)
head(cv_index)

### this process will be repeated across folds
length(which(cv_index == 1)) ### elements of cv_index ==1
cv_train_1 <- df[-which(cv_index == 10),] 
cv_valid_1 <- df[which(cv_index == 10),] 

X1 <- Score(
  object = bl_lib, formula = Hist(time,event) ~ 1,
  data = cv_train_1, cause = 1, times = 5,se.fit=FALSE,
  metrics = "brier", summary = "risks", seed=9,
  split.method = "cv5", B = 10, null.model = FALSE
)
X1[["Brier"]][["score"]][,-"times"]
rm(X1, cv_train_1, cv_valid_1,lin,int,sim)

# place holder for list of data tables
cv_folds <- as.list(1:k)
names(cv_folds) <- paste0("fold",1:k)

for (i in 1:length(cv_folds)) {
  print(cv_folds[[i]])
}

### Step 1: find the model with lowest Brier among alternative (new) models
# function to apply the call
mf <- function(m){
  m$call$formula <- eval(m$call$formula)
  m}

# function to apply the SL to each fold in-bag set 
get_bv <- function(fold=1) {
  cv_train <- df[-which(cv_index == fold),]  # df of all data except fold k
  
  X <- Score(
    object = bl_lib, formula = Hist(time,event) ~ 1,
    data = cv_train, cause = 1, times= 5,se.fit=FALSE,
    metrics = "brier", summary="risks", seed=9,
    split.method = "loob", B=100, M=round(.632*NROW(cv_train)), null.model=FALSE
  )
  
  b <- X[["Brier"]][["score"]][,-"times"]
  return(b)
  
}


# apply the SL meta-algorithm to the 10 in-bag sets (cv-train for each fold)
# and save the brier score table each time
B <- lapply(cv_folds,get_bv) # this can be done in PAR

# obtain the mean Brier across
br <- rbindlist(B)
# which does the following
# br <- rbind(B[["fold1"]],B[["fold2"]],B[["fold3"]],B[["fold4"]],B[["fold5"]],
#             B[["fold6"]],B[["fold7"]],B[["fold8"]],B[["fold9"]],B[["fold10"]])

# the winner has the lowest Brier; use lowest IBS at max(times) if for length(times)>1
mu <- br %>% group_by(model) %>%
  summarise_at(vars(Brier), list(Brier = mean))
mu

### Step 2: for each fold 
# - train the winner (or re-train the existing SL) using the in-bag data
# - obtain predictions from the winner and from the benchark (or available SL) 
# - save the OOB data with the predictions

get_pr <- function(fold) {  ### arguments will include bl_lib, mu, data
  cv_train <- df[-which(cv_index == fold),]  # df of all data except fold k
  cv_valid <- df[which(cv_index == fold),]  # df with only data from fold k
  
  # find the winner among the experimental alternatives
  win <- bl_lib[[mu$model[[which.min(mu$Brier)]]]]
  win <- mf(win[["call"]])
  new <- CSC(eval(win$formula), data = cv_train)
  
  # or update an existing model (if no Step 1), here done together
  # assuming there is both an existing SL to re-train and a new one
  upd <- CSC(Hist(time,event) ~ X1 + X2 + X7 + X8, data = cv_train)
  
  # obtain predictions from new, updated and reference model
  
  # ref model, no update
  pre1 <- as.data.table(predictRisk(ref,cv_valid,times=5,cause=1))
  # the ref model, but fit on the current train set
  pre2 <- as.data.table(predictRisk(upd,cv_valid,times=5,cause=1))
  # the new model (winner from SL) fits on the current train set
  pre3 <- as.data.table(predictRisk(new,cv_valid,times=5,cause=1))
  
  pred <- cbind.data.frame(pre1,pre2,pre3)
  colnames(pred) <- c("ref","upd","new")
  
  cv_pred <- cbind.data.frame(pred,cv_valid) %>% mutate(cv_fold = fold)
  
  out <- cv_pred
  return(out)
  
}


# n = n, 1 row for each prediction
system.time(
  X <- purrr::map_dfr(cv_folds, ~ get_pr(fold = .x)) # .x specifies 1:k
)

# create score object, of benchmark, updated and new (experimental)
Z <- Score(
  list("Benchmark"=X$ref,
       "Updated"=X$upd,
       "Experimental"=X$new),
  formula = Hist(time,event) ~ 1,
  data = X,
  cause = 1, 
  times= 5,
  se.fit=TRUE,
  metrics = c("auc","brier"),
  summary="risks",
  seed=9,
  plots="Calibration")

plotCalibration(Z,cens.method="jackknife")

# alternative calibration plot
# data frame for outcome, risk and model to stratify by
df_risks <- data.frame('Outcome' = Z$risks$score$event,
                 '.risk' = Z$risks$score$risk,
                 'Model' = Z$risks$score$model)

# call main fct
df_risks %>%
  cal_plot_breaks(Outcome, .risk, .by = Model,
                  include_ribbon = FALSE,
                  conf.level = 0.95,
                  include_points = TRUE,
                  num_breaks = 10) +
  facet_wrap(~.) +
  ggtitle("Calibration Plot")+ 
  theme( strip.text.x = element_blank()) + 
  ylab('Observed') + xlab('Predicted')

### if benchmark is an existing (old) SL and the updated is the same SL re-trained with more recent data
### given similar Brier we can check if predictions differ more than 10%
plotRisk(Z, times = 5)

### Step 3: train the winner using all data
dSl <- CSC(eval(bl_lib[["lin"]][["call"]][["formula"]]), data = df)

# Is this approach "correct" for verification of a train model?
# Or for centers that don't have sufficient data for temporal testing?



