library(dplyr)
library(survival)
library(rms)

source("coxsp.ggplot2.R")
source("coxsp.predict.R")

data(cancer, package="survival")
head(lung)
lung$sex <- ifelse(lung$sex == 1, "Male", "Female")
# inst: Institution code
# time: Survival time in days
# status: censoring status 1=censored, 2=dead
# age: Age in years
# sex: Male=1 Female=2
# ph.ecog: ECOG performance score (0=good 5=dead)
# ph.karno: Karnofsky performance score as rated by physician
# pat.karno: Karnofsky performance score as rated by patient
# meal.cal: Calories consumed at meals
# wt.loss: Weight loss in last six months

# Fit un-adjusted and adjusted non-linear model
model_uni <- coxph(Surv(time, status) ~ rcs(age, parms = 3), data = lung)
model_adj <- coxph(Surv(time, status) ~ rcs(age, parms = 3) + sex, data = lung)

# Get the prediction for hazard ratio and CI for nonlinear model
HRres_uni <- predict.coxsp(
  object = model_uni, varraw = lung$age, vartran = lung$age, ref = "mean"
)
HRres_adj <- predict.coxsp(
  object = model_adj, varraw = lung$age, vartran = lung$age, ref = "mean"
)

HRres_adj$label <- "adjusted"
HRres_uni$label <- "unadjusted"
HRres <- rbind(HRres_adj, HRres_uni)

# Generate a ggplot2 plot for nonlinear hazard ratio
plt <- ggcoxsp.plt(
  spHR = HRres,
  xlim = c(40, 72),
  xtik = seq(40, 70, 5),
  xlab = "Age", 
  ylab = "Hazard ratio for lung cancer",
  main = "Hazard ratio compared to the mean age"
)

# Add additional text on plot depend on your needs
# In this case, interaction p-value and a vertical line are added
model_lin <- coxph(Surv(time, status) ~ 1, data = lung)
model_cov <- coxph(Surv(time, status) ~ sex, data = lung)

adjusted_p <- anova(model_adj, model_cov)$`Pr(>|Chi|)`[2]
unadjusted_p <- anova(model_uni, model_lin)$`Pr(>|Chi|)`[2]

plt <- plt +
  annotate(
    geom = "text",
    label = paste("adjusted association p-value", round(adjusted_p, 3)),
    x = 55, y = 0.3,
    colour = "darkslateblue", size = 6
  ) +
  annotate(
    geom = "text",
    label = paste("unadjusted association p-value", round(unadjusted_p, 3)), 
    x = 55, y = 0.1,
    colour = "peru", size = 6
  ) +
  annotate(
    "segment",
    x = mean(lung$age, na.rm = TRUE),
    xend = mean(lung$age, na.rm = TRUE),
    y = -Inf,
    yend = 1,
    colour = "darkslateblue",
    linetype = "dashed"
  )

# Pick the predictor points you want to highlight and generate a ggtable
tbl <- ggcoxsp.table2(
  spHR = HRres_adj,
  nodes = c(45, 50, 55, 60, 65),
  row.name = "Age"
)

# Aggregate the plot and table into one image
plt_res <- grid.arrange(plt, tbl, ncol = 1, heights = c(4,1))

# Save image
ggsave(
  plot = plt_res,
  filename = "Manish_walk/nonlinear/ASCVD_age_1.png",
  device = "png", width = 2000, height = 2100, units = "px"
)
