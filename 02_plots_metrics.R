# 02_plots_metrics.R
# Reads outputs and produces ggplot visuals to PNG

packages <- c("data.table","ggplot2")
inst <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(inst)) install.packages(inst, repos="https://cloud.r-project.org")
library(data.table); library(ggplot2)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

lift <- fread("outputs/lift.csv")
cal  <- fread("outputs/calibration.csv")

# Lift plot
p1 <- ggplot(lift[sample=="test"], aes(x=cum_exp, y=cum_loss, group=model)) +
  geom_line(aes(linetype=model)) +
  geom_abline(slope=1, intercept=0) +
  labs(title="Cumulative lift - test", x="Cumulative exposure", y="Cumulative loss share")
ggsave("figures/lift_test.png", p1, width=7, height=5, dpi=150)

# Calibration plot
p2 <- ggplot(cal[sample=="test"], aes(x=obs, y=pred, shape=model)) +
  geom_point() + geom_abline(slope=1, intercept=0) +
  labs(title="Calibration by decile - test", x="Observed pure premium", y="Predicted pure premium")
ggsave("figures/calibration_test.png", p2, width=7, height=5, dpi=150)

message("Saved plots in figures/.")