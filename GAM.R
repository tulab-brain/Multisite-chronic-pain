####GAM####
library('mgcv')
library('dplyr')
library('ggplot')
####cognitive functions####
dat <-
  select(
    df,
    cog_var,
    group,
    X31.0.0,
    X21001.0.0,
    X189.0.0,
    X21001.2.0,
    X12144.2.0,
    X22009.0.1,
    X22009.0.2,
    X22009.0.3,
    X22009.0.4,
    X22009.0.5,
    X22009.0.6,
    X22009.0.7,
    X22009.0.8,
    X22009.0.9,
    X22009.0.10,
    X21000.0.0,
    X20116.2.0,
    X20117.2.0,
    aspirin,
    Ibuprofen,
    Paracetamol
  ) %>% drop_na()
for (i in 1:ncol(df)) {
  df <- df[!df[, i] == "",]
}
df <-
  mutate(df, ogroup = ordered(group, levels = c('MCP', 'PF', 'SCP'))) %>%
  mutate(group = factor(group, levels = c('MCP', 'PF', 'SCP')))
m <-
  gam(
    cog_var ~ group + s(age, by = Ogroup) + s(age) + sex + X189.0.0 + aspirin +
      Ibuprofen + Paracetamol +
      X21001.2.0 + X12144.2.0 + X21000.0.0 + X20116.2.0 + X20117.2.0 +
      X22009.0.1 + X22009.0.2 + X22009.0.3 + X22009.0.4 + X22009.0.5 +
      X22009.0.6 + X22009.0.7 + X22009.0.8 + X22009.0.9 + X22009.0.10,
    data = df
  )

p <- plot_mspgam_cog(df, "Fluid intelligence")
p

anova(m)
summary(m)

plot_mspgam_cog <- function(df, title) {
  grid <- expand.grid(
    age = seq(
      from = min(df$age),
      to = max(df$age),
      by = 0.1
    ),
    group = factor(c("PF", "SCP", "MCP")),
    sex = factor(0),
    X189.0.0 = mean(df$X189.0.0),
    X21001.2.0 = mean(df$X21001.2.0),
    X12144.2.0 = mean(df$X12144.2.0),
    X22009.0.1 = mean(df$X22009.0.1),
    X22009.0.2 = mean(df$X22009.0.2),
    X22009.0.3 = mean(df$X22009.0.3),
    X22009.0.4 = mean(df$X22009.0.4),
    X22009.0.5 = mean(df$X22009.0.5),
    X22009.0.6 = mean(df$X22009.0.6),
    X22009.0.7 = mean(df$X22009.0.7),
    X22009.0.8 = mean(df$X22009.0.8),
    X22009.0.9 = mean(df$X22009.0.9),
    X22009.0.10 = mean(df$X22009.0.10),
    X21000.0.0 = factor(1),
    X20116.2.0 = factor(0),
    X20117.2.0 = factor(2),
    aspirin = factor(0),
    Ibuprofen = factor(0),
    Paracetamol = factor(0)
  )
  
  fit <- predict.gam(m, grid, se.fit = TRUE)
  fit <- predict(m, newdata = grid, se.fit = TRUE)
  crit <- qt(0.05 / 2, df.residual(m), lower.tail = FALSE)
  grid$estimate <- fit$fit
  grid$lower_ci <- grid$estimate - crit * fit$se.fit
  grid$upper_ci <- grid$estimate + crit * fit$se.fit
  ggplot(grid, aes(x = age)) +
    geom_line(aes(y = estimate, color = group), size = 0.7) +
    geom_ribbon(aes(
      ymin = lower_ci,
      ymax = upper_ci,
      group = group
    ), alpha = 0.09) +
    scale_colour_manual(values = c(
      PF = "#009933",
      NOPCs = "#333333",
      COPCs =
        "#FF6600"
    )) +
    theme_classic() +
    theme(
      plot.title = element_text(
        size = 22,
        family = "sans",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5
      ),
      legend.title = element_blank(),
      legend.text = element_text(size = 15, family = "sans"),
      legend.position = c(0.85, 0.9),
      legend.key.size = unit(0.3, 'cm'),
      axis.line = element_line(size = 1.2, color = "black"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = ,
      axis.text.x = element_text(
        size = 22,
        family = "sans",
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.text.y = element_text(
        size = 22,
        family = "sans",
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.x = element_text(
        size = 22,
        family = "sans",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.y = element_text(
        size = 22,
        family = "sans",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5
      ),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    ) +
    labs(title = paste0(tit),
         x = "Age (years)") %>%
    return()
}


####hippocampal volume####
df <-
  select(
    df,
    IDP,
    group,
    sex,
    age,
    X25010.2.0,
    X189.0.0,
    X21001.2.0,
    X12144.2.0,
    X22009.0.1,
    X22009.0.2,
    X22009.0.3,
    X22009.0.4,
    X22009.0.5,
    X22009.0.6,
    X22009.0.7,
    X22009.0.8,
    X22009.0.9,
    X22009.0.10,
    X21000.0.0,
    X20116.2.0,
    X20117.2.0,
    aspirin,
    Ibuprofen,
    Paracetamol
  ) %>% drop_na()
for (i in 1:ncol(df)) {
  df <- df[!df[, i] == "",]
}
df <-
  mutate(df, ogroup = ordered(group, levels = c('MCP', 'PF', 'SCP'))) %>%
  mutate(group = factor(group, levels = c('MCP', 'PF', 'SCP'))) %>%
  outlier_rm(1)
m <-
  gam(
    IDP ~ group + s(age, by = ogroup) + s(age) + sex + X25010.2.0 + X189.0.0 + aspirin +
      Ibuprofen + Paracetamol + X21001.2.0 + X12144.2.0 + X21000.0.0 + X20116.2.0 + X20117.2.0 +
      X22009.0.1 + X22009.0.2 + X22009.0.3 + X22009.0.4 + X22009.0.5 +
      X22009.0.6 + X22009.0.7 + X22009.0.8 + X22009.0.9 + X22009.0.10,
    data = df
  )

p <- plot_mspgam(df, "IDP")
p
anova(m)
summary(m)


plot_mspgam <- function(df, tit) {
  grid <- expand.grid(
    age = seq(
      from = min(df$age),
      to = max(df$age),
      by = 0.1
    ),
    group = factor(c("PF", "SCP", "MCP")),
    sex = factor(0),
    X25010.2.0 = mean(df$X25010.2.0),
    X189.0.0 = mean(df$X189.0.0),
    X21001.2.0 = mean(df$X21001.2.0),
    X12144.2.0 = mean(df$X12144.2.0),
    X22009.0.1 = mean(df$X22009.0.1),
    X22009.0.2 = mean(df$X22009.0.2),
    X22009.0.3 = mean(df$X22009.0.3),
    X22009.0.4 = mean(df$X22009.0.4),
    X22009.0.5 = mean(df$X22009.0.5),
    X22009.0.6 = mean(df$X22009.0.6),
    X22009.0.7 = mean(df$X22009.0.7),
    X22009.0.8 = mean(df$X22009.0.8),
    X22009.0.9 = mean(df$X22009.0.9),
    X22009.0.10 = mean(df$X22009.0.10),
    X21000.0.0 = factor(1),
    X20116.2.0 = factor(0),
    X20117.2.0 = factor(2),
    aspirin = factor(1),
    Ibuprofen = factor(0),
    Paracetamol = factor(0)
  )
  
  fit <- predict.gam(m, grid, se.fit = TRUE)
  fit <- predict(m, newdata = grid, se.fit = TRUE)
  crit <- qt(0.05 / 2, df.residual(m), lower.tail = FALSE)
  grid$estimate <- fit$fit
  grid$lower_ci <- grid$estimate - crit * fit$se.fit
  grid$upper_ci <- grid$estimate + crit * fit$se.fit
  ggplot(grid, aes(x = age)) +
    geom_line(aes(y = estimate, color = group), size = 0.7) +
    geom_ribbon(
      aes(
        ymin = lower_ci,
        ymax = upper_ci,
        group = group
      ),
      alpha = 0.09,
      fill = "#333333"
    ) +
    scale_colour_manual(values = c(
      PF = "#009933",
      NOPCs = "#333333",
      COPCs = "#FF6600"
    )) +
    theme_classic() +
    theme(
      plot.title = element_text(
        size = 22,
        family = "sans",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5
      ),
      legend.title = element_blank(),
      legend.text = element_text(size = 15, family = "sans"),
      legend.position = c(0.85, 0.9),
      legend.key.size = unit(0.3, 'cm'),
      axis.line = element_line(size = 1.2, color = "black"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = ,
      axis.text.x = element_text(
        size = 22,
        family = "sans",
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.text.y = element_text(
        size = 22,
        family = "sans",
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.x = element_text(
        size = 22,
        family = "sans",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.y = element_text(
        size = 22,
        family = "sans",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5
      ),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    ) +
    labs(title = paste0(tit),
         y = "volume (mm³)",
         x = "Age (years)") %>%
    return()
}

####function####
outlier_rm <- function(dat, i) {
  upper <- mean(dat[, i], na.rm = TRUE) + 4 * sd(dat[, i], na.rm = TRUE)
  lower <- mean(dat[, i], na.rm = TRUE) - 4 * sd(dat[, i], na.rm = TRUE)
  dat <- dat[!dat[, i] >= upper & !dat[, i] <= lower, ] %>%
    return()
}