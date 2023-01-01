####mediation####
library('lavaan')
####left hippocampus####
df%<>%outlier_rm(1)
med_model_cov <- function(df) {
  ##model
  model_cov_med <-
    '
  X25886.2.0 ~ a*group
  X20016.2.0 ~ b*X25886.2.0
  X20016.2.0 ~ c*group
  X20016.2.0 ~ sex + age + X189.0.0 + X21001.2.0 + X12144.2.0+ X25010.2.0+
        X20116.2.0 + X20117.2.0 + X21000.0.0 +aspirin+Ibuprofen+Paracetamol+
        X22009.0.1 + X22009.0.2 + X22009.0.3 + X22009.0.4 + X22009.0.5 +
        X22009.0.6 + X22009.0.7 + X22009.0.8 + X22009.0.9 + X22009.0.10
  X25886.2.0 ~ sex + age + X25010.2.0 + X189.0.0 + X21001.2.0 + X12144.2.0 +
        X20116.2.0 + X20117.2.0 + X21000.0.0 +aspirin+Ibuprofen+Paracetamol+
        X22009.0.1 + X22009.0.2 + X22009.0.3 + X22009.0.4 + X22009.0.5 +
        X22009.0.6 + X22009.0.7 + X22009.0.8 + X22009.0.9 + X22009.0.10
  ab := a*b
  total := c + (a*b)'
  mediation_result <- sem(model_cov_med, data = df, estimator = "MLR")
  return(summary(mediation_result, rsquare = TRUE))
}
med_model_cov(filter(df,group!=1))#MCP-PF
med_model_cov(filter(df,group!=0))#MCP-SCP
med_model_cov(filter(df,group!=2))#SCP-PF

####right hippocampus####
med_model_cov <- function(df) {
  ##model
  model_cov_med <-
    '
  X25887.2.0 ~ a*group
  X20016.2.0 ~ b*X25887.2.0
  X20016.2.0 ~ c*group
  X20016.2.0 ~ sex + age + X189.0.0 + X21001.2.0 + X12144.2.0+ X25010.2.0+
        X20116.2.0 + X20117.2.0 + X21000.0.0 +aspirin+Ibuprofen+Paracetamol+
        X22009.0.1 + X22009.0.2 + X22009.0.3 + X22009.0.4 + X22009.0.5 +
        X22009.0.6 + X22009.0.7 + X22009.0.8 + X22009.0.9 + X22009.0.10
  X25887.2.0 ~ sex + age + X25010.2.0 + X189.0.0 + X21001.2.0 + X12144.2.0 +
        X20116.2.0 + X20117.2.0 + X21000.0.0 +aspirin+Ibuprofen+Paracetamol+
        X22009.0.1 + X22009.0.2 + X22009.0.3 + X22009.0.4 + X22009.0.5 +
        X22009.0.6 + X22009.0.7 + X22009.0.8 + X22009.0.9 + X22009.0.10
  ab := a*b
  total := c + (a*b)'
  mediation_result <- sem(model_cov_med, data = df, estimator = "MLR")
  return(summary(mediation_result, rsquare = TRUE))
}
med_model_cov(filter(df,group!=1))#MCP-PF
med_model_cov(filter(df,group!=0))#MCP-SCP
med_model_cov(filter(df,group!=2))#SCP-PF


####function####
outlier_rm <- function(dat, i) {
  upper <- mean(dat[, i], na.rm = TRUE) + 4 * sd(dat[, i], na.rm = TRUE)
  lower <- mean(dat[, i], na.rm = TRUE) - 4 * sd(dat[, i], na.rm = TRUE)
  dat <- dat[!dat[, i] >= upper & !dat[, i] <= lower, ] %>%
    return()
}
