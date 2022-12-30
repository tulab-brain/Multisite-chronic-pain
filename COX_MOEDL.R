####COX moedl####
library("survival")
dat <-
  mutate(
    dat,
    group = factor(group, levels = c('painfree', 'SCP', 'MCP')),
    X21003.0.0 = numeric(X21003.0.0),
    X189.0.0 = numeric(X189.0.0),
    X31.0.0 = factor(X31.0.0),
    X21000.0.0 = factor(X21000.0.0),
    X2443.0.0 = factor(X2443.0.0),
    X2453.0.0 = factor(X2453.0.0),
    X2966.0.0 = factor(X2966.0.0),
    X3627.0.0 = factor(X3627.0.0),
    X3894.0.0 = factor(X3894.0.0),
    X4056.0.0 = factor(X4056.0.0),
    X6138.0.0 = factor(X6138.0.0),
    X2090.0.0 = factor(X2090.0.0),
    X20116.0.0 = factor(X20116.0.0),
    X20117.0.0 = factor(X20117.0.0),
    aspirin = factor(aspirin),
    Ibuprofen = factor(Ibuprofen),
    Paracetamol = factor(Paracetamol)
  )

basic_model <-
  coxph(
    Surv(diffyear, dementia) ~ group + X31.0.0 + X21000.0.0 + X21003.0.0 + aspirin +
      Ibuprofen + Paracetamol,
    data = dat
  )
sm1 <- summary(basic_model)
sm1

full_model <-
  coxph(
    Surv(diffyear, dementia) ~ group + X31.0.0 + X21003.0.0 + X189.0.0 + X21001.0.0 +
      X21000.0.0 + X2443.0.0 + X2453.0.0 + X2966.0.0 + X3627.0.0 + X3894.0.0
    + X4056.0.0 + X6138.0.0 + X2090.0.0 + X20116.0.0 + X20117.0.0 +
      aspirin + Ibuprofen + Paracetamol,
    data = dat
  )
sm2 <- summary(full_model)
sm2



