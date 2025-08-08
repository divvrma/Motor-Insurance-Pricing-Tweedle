# 01_fit_models.R
# GLM Tweedie vs GBM on CAS freMTPL2 data
# - Creates: data/mtpl_model_data.rds, models/glm_tweedie.rds, models/gbm_tweedie.rds, outputs/metrics.csv, outputs/calibration.csv
# - Also writes scored dataset for app: outputs/scored.csv

packages <- c("CASdatasets","dplyr","data.table","ggplot2","statmod","tweedie","gbm","Matrix","yardstick","pROC")
inst <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(inst)) install.packages(inst, repos="https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(CASdatasets)
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(statmod)
  library(tweedie)
  library(gbm)
  library(Matrix)
  library(yardstick)
  library(pROC)
})

dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("models", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Load and join the French MTPL data
# Source: CASdatasets freMTPL2freq and freMTPL2sev
data(freMTPL2freq)
data(freMTPL2sev)

freq <- as.data.table(freMTPL2freq)
sev  <- as.data.table(freMTPL2sev)

# Aggregate claim amounts by policy
sev_agg <- sev[, .(ClaimAmount = sum(ClaimAmount, na.rm=TRUE)), by=.(IDpol)]

dt <- merge(freq, sev_agg, by="IDpol", all.x=TRUE)
dt[is.na(ClaimAmount), ClaimAmount := 0]

# Basic cleaning and feature engineering
dt <- dt[Exposure > 0]
dt[, PurePrem := ClaimAmount / Exposure]

# Coerce some variables to factors
fac_cols <- c("VehPower","VehBrand","VehGas","Area","Region")
for (col in fac_cols) dt[, (col) := as.factor(get(col))]

# Train-test split
set.seed(42)
dt[, fold := sample.int(5, .N, replace=TRUE)]
train <- dt[fold %in% 1:4]
test  <- dt[fold == 5]

# Estimate Tweedie power parameter via profile likelihood on a subset for speed
message("Estimating Tweedie power p...")
sub_ix <- sample(seq_len(nrow(train)), min(200000, nrow(train)))
pgrid <- seq(1.1, 1.9, by=0.1)
pl <- sapply(pgrid, function(pp) {
  fit <- glm(PurePrem ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Area + Density + Region + log(Exposure),
             data=train[sub_ix], family = tweedie(var.power = pp, link.power = 0), weights = Exposure)
  logLik(fit)
})
p_hat <- pgrid[which.max(pl)]
message(sprintf("Chosen Tweedie p = %.2f", p_hat))

# Fit GLM Tweedie for pure premium with log link
glm_formula <- PurePrem ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Area + Density + Region
glm_fit <- glm(glm_formula,
               data=train,
               family = tweedie(var.power = p_hat, link.power = 0),
               weights = Exposure)

saveRDS(glm_fit, file = "models/glm_tweedie.rds")

# Fit GBM with Tweedie
# gbm uses distribution=list(name="tweedie", variance.power=p, link.power=0)
set.seed(42)
xcols <- c("VehPower","VehAge","DrivAge","BonusMalus","VehBrand","VehGas","Area","Density","Region")
gbm_train <- train[, c(xcols, "PurePrem","Exposure"), with=FALSE]
gbm_fit <- gbm::gbm(
  formula = PurePrem ~ .,
  data = gbm_train,
  distribution = list(name="tweedie", variance.power=p_hat, link.power=0),
  weights = gbm_train$Exposure,
  n.trees = 4000,
  interaction.depth = 4,
  shrinkage = 0.01,
  bag.fraction = 0.7,
  n.minobsinnode = 50,
  train.fraction = 0.9,
  cv.folds = 5,
  keep.data = FALSE,
  verbose = FALSE
)
best_iter <- gbm.perf(gbm_fit, method="cv", plot.it = FALSE)
attr(gbm_fit, "best_iter") <- best_iter
saveRDS(gbm_fit, file = "models/gbm_tweedie.rds")

# Scoring
predict_glm <- function(df) pmax(1e-8, predict(glm_fit, newdata=df, type="response"))
predict_gbm <- function(df) pmax(1e-8, predict(gbm_fit, newdata=df, n.trees=attr(gbm_fit,"best_iter"), type="response"))

train[, `:=`(pred_glm = predict_glm(.SD), pred_gbm = predict_gbm(.SD))]
test[,  `:=`(pred_glm = predict_glm(.SD), pred_gbm = predict_gbm(.SD))]

# Metrics helpers
weighted_mean <- function(x, w) sum(x*w)/sum(w)

# Gini (normalized) using weighted Lorenz
wgini <- function(y, yhat, w) {
  ord <- order(yhat, decreasing = TRUE)
  y <- y[ord]; w <- w[ord]
  cw <- cumsum(w)/sum(w)
  cy <- cumsum(y*w)/sum(y*w)
  g <- sum((cy[-1] + cy[-length(cy)]) * diff(cw)) - 1
  return(g)
}

lift_table <- function(y, yhat, w, groups=10) {
  q <- cut(rank(yhat, ties.method="first")/length(yhat), breaks = seq(0,1,length.out=groups+1), include.lowest = TRUE, labels = FALSE)
  dt <- data.table(y=y, yhat=yhat, w=w, q=q)
  tab <- dt[, .(
    exposure = sum(w),
    actual_pp = sum(y*w)/sum(w),
    pred_pp   = sum(yhat*w)/sum(w),
    count = .N
  ), by=q][order(-q)]
  tab[, cum_exp := cumsum(exposure)/sum(exposure)]
  tab[, cum_loss := cumsum(actual_pp*exposure)/sum(actual_pp*exposure)]
  tab[, lift := cum_loss / cum_exp]
  tab
}

calibration_table <- function(y, yhat, w, groups=10) {
  q <- cut(rank(yhat, ties.method="first")/length(yhat), breaks = seq(0,1,length.out=groups+1), include.lowest = TRUE, labels = FALSE)
  dt <- data.table(y=y, yhat=yhat, w=w, q=q)
  dt[, .(
    exposure = sum(w),
    obs = sum(y*w)/sum(w),
    pred = sum(yhat*w)/sum(w)
  ), by=q][order(q)]
}

# Compute metrics on test
for (dfname in c("train","test")) {
  df <- get(dfname)
  for (m in c("glm","gbm")) {
    y <- df$PurePrem; yhat <- df[[paste0("pred_",m)]]; w <- df$Exposure
    g <- wgini(y,yhat,w)
    lt <- lift_table(y,yhat,w,groups=10)
    cb <- calibration_table(y,yhat,w,groups=10)
    lt$model <- m; lt$sample <- dfname
    cb$model <- m; cb$sample <- dfname
    if (!exists("all_lift")) { all_lift <- lt } else { all_lift <- rbind(all_lift, lt) }
    if (!exists("all_cal"))  { all_cal  <- cb } else { all_cal  <- rbind(all_cal, cb) }
    metrics_row <- data.frame(model=m, sample=dfname,
                              wmae = mean(abs(y - yhat)),
                              dev = mean((y^(2-p_hat) - yhat^(2-p_hat))/(2-p_hat)/(1-p_hat)), # pseudo
                              gini=g, stringsAsFactors = FALSE)
    if (!exists("all_metrics")) { all_metrics <- metrics_row } else { all_metrics <- rbind(all_metrics, metrics_row) }
  }
}

fwrite(all_metrics, "outputs/metrics.csv")
fwrite(all_lift, "outputs/lift.csv")
fwrite(all_cal,  "outputs/calibration.csv")

# Save scored data for app
scored <- test[, .(IDpol, Exposure, ClaimAmount, PurePrem, pred_glm, pred_gbm, Area, VehAge, DrivAge, BonusMalus, VehBrand, VehGas, Density, Region, VehPower)]
fwrite(scored, "outputs/scored.csv")

saveRDS(list(train=train, test=test, p_hat=p_hat), file="data/mtpl_model_data.rds")

message("Done.")