
library(ggpubr)
library(ggplot2)
library(reshape2)
library(patchwork)

summary(model1)

model1


# get predictions for each exposure-lag
pred1.wf_pm25 <- crosspred(cb1.intx, model1, at = 0:10*20,#0:120, 
                           bylag = 1, cumul = TRUE, cen = 0)
plot(pred1.wf_pm25, col = "red",
     zlab = "Risk of PTB", 
     ylab = "Gestational week", xlab = "Smoke-specific PM2.5")


pred1.pm25 <- crosspred(cb1.pm25, model1, at = 0:10*20, 
                        bylag = 1, cumul = TRUE, cen = 0)
plot(pred1.pm25, col = "darkorange",
     zlab = "Risk of PTB", 
     ylab = "Gestational week", xlab = "PM2.5")

# wil get crosspred for WF manually

combined_rr <- merge(melt(pred1.wf_pm25$mathigh), 
                     melt(pred1.pm25$mathigh), by = c("Var1", "Var2")) |>
  rename(RR_wf_pm = value.x,
         RR_pm = value.y)

combined_rr$total_fx <- exp(
  combined_rr$RR_wf_pm + combined_rr$RR_pm)
combined_rr$lag = as.numeric(gsub("lag", "", combined_rr$Var2))

ggplot(combined_rr, aes(x = lag, y = Var1, fill = total_fx)) +
  geom_tile(colour = "gray70", linewidth = 0.2) +
  scale_fill_gradient2(low = "royalblue1", mid = "white", 
                       high = "orangered", midpoint = 1) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Risk of Preterm Birth", 
       x = "Gestational Week",
       y = "PM2.5 Concentration")

# turn this into separate functions - one for extracting RR and 95% CI, one for the heatmap

heatplot_rr <- function(crosspred_res,
                        exposure_name) {
  
  rr_mat <- crosspred_res$matRRfit |>
    reshape2::melt() |>
    rename(exposure = Var1,
           lag = Var2,
           RR = value) |>
    mutate(lag = as.numeric(gsub("lag", "", lag)))
  
  ggplot(rr_mat, aes(x = lag, y = exposure, fill = RR)) +
    geom_tile(colour = "gray70", linewidth = 0.2) +
    scale_fill_gradient2(low = "royalblue1", mid = "white", 
                         high = "orangered", midpoint = 1) +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Relative Risk of Preterm Birth", 
         x = "Gestational Week",
         y = exposure_name)
  
}

cplist$matfit |> #cplist$`very early`$matfit |>
  reshape2::melt() |>
  rename(exposure = Var1,
         lag = Var2,
         RR = value) |>
  mutate(RR = exp(RR)) |>
  mutate(lag = as.numeric(gsub("lag", "", lag))) |>
  ggplot(aes(x = lag, y = exposure, fill = RR)) +
  geom_tile(colour = "gray70", linewidth = 0.2) +
  scale_fill_gradient2(low = "royalblue1", mid = "white", 
                       high = "orangered", midpoint = 1) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Relative Risk of Preterm Birth", 
       x = "Gestational Week",
       y = "exposure_name")



heatplot_rr(cplist$`very early`$matfit, "WF-specific PM2.5") # guess it's matfit now instead of matRR
heatplot_rr(pred1.pm25, "PM2.5 concentration (any source)")


heatplot_rr_ci

rr_mat <- pred1.wf_pm25$matRRfit |> #cplist$`late`$matfit |> #pred1.wf_days$matRRfit |>
  reshape2::melt() |>
  rename(exposure = Var1,
         lag = Var2,
         RR = value) |>
  # mutate(RR = exp(RR)) |>
  mutate(lag = as.numeric(gsub("lag", "", lag))) |>
  # rename(exposure = Var1,
  #        lag = Var2,
  #        RR = value) |>
  mutate(lag = as.numeric(gsub("lag", "", lag)))

ci_lo_mat <- pred1.wf_pm25$matRRlow |> #cplist$`late`$matlow |>#pred1.wf_days$matRRlow |>
  reshape2::melt() |>
  rename(exposure = Var1,
         lag = Var2,
         CI_lo = value) |>
  # mutate(CI_lo = exp(CI_lo)) |>
  mutate(lag = as.numeric(gsub("lag", "", lag))) |>
  # rename(exposure = Var1,
  #        lag = Var2,
  #        RR = value) |>
  mutate(lag = as.numeric(gsub("lag", "", lag)))

ci_hi_mat <- pred1.wf_pm25$matRRhigh |> #cplist$`late`$mathigh |>#pred1.wf_days$matRRhigh |>
  reshape2::melt() |>
  rename(exposure = Var1,
         lag = Var2,
         CI_hi = value) |>
  # mutate(CI_hi = exp(CI_hi)) |>
  mutate(lag = as.numeric(gsub("lag", "", lag))) |>
  # rename(exposure = Var1,
  #        lag = Var2,
  #        RR = value) |>
  mutate(lag = as.numeric(gsub("lag", "", lag)))

rr <- merge(rr_mat, ci_lo_mat, by = c("exposure", "lag")) |>
  merge(ci_hi_mat, by = c("exposure", "lag"))

head(rr)

# statistical significance, for outline
rr$significant <- ifelse(rr$CI_lo > 1 & rr$CI_hi > 1, "significant", "not")

# rr_significant <- rr |>
#   filter(significant == "significant")

rr_plot <- ggplot(rr, aes(x = lag, y = exposure, fill = RR)) +
  geom_tile(colour = "gray70", linewidth = 0.0) +
  scale_fill_gradient2(low = "royalblue1", mid = "white", 
                       high = "orangered", midpoint = 1,
                       limits = c(0.07, 3.10)) +
  geom_tile(data = rr[which(rr$significant == "significant"),], 
            fill = NA, color = "orangered", size = 0.25) +
  # geom_tile(data = subset(rr_mat, rr_mat$significant == "significant"), fill = "black") +
                       #limits = c(0, 22.1)) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Relative Risk of Preterm Birth", 
       x = "Gestational Week",
       y = "WF smoke-specific PM2.5 exposure")

ci_lo_plot <- ggplot(ci_lo_mat, aes(x = lag, y = exposure, fill = CI_lo)) +
  geom_tile(colour = "gray70", linewidth = 0.0) +
  scale_fill_gradient2(low = "royalblue1", mid = "white", 
                       high = "orangered", midpoint = 1,
                       limits = c(0.07, 3.10)) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Lower 95% CI", 
       x = "",
       y = "", fill = "")

ci_hi_plot <- ggplot(ci_hi_mat, aes(x = lag, y = exposure, fill = CI_hi)) +
  geom_tile(colour = "gray70", linewidth = 0.0) +
  scale_fill_gradient2(low = "royalblue1", mid = "white", 
                       high = "orangered", midpoint = 1,
                       limits = c(0.07, 3.10)) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Upper 95% CI", 
       x = "",
       y = "", fill = "")

rr_plot + (ci_hi_plot / ci_lo_plot)+# / plot_spacer()) +
  plot_layout(widths = c(2.5, 1), guides = "collect")


sig <- rr |> filter(significant == "significant")
summary(rr_mat$RR)

str(pred1.wf_pm25$matRRfit)

wf_pm_mat <- pred1.pm25$matRRfit

# Melt the matrix into a long format
melted_data <- melt(wf_pm_mat)

names(melted_data) <- c("pm25", "lag", "RR")

melted_data$lag <- as.numeric(gsub("lag", "", melted_data$lag))

# lim1 <- melted_data[which.max(abs(melted_data$RR - 1)), "RR"]
# lim2 <- 1 + (1 - melted_data[which.max(abs(melted_data$RR - 1)), "RR"])
# 
# lo_limit <- ifelse(lim1 < lim2, lim1, lim2)
# hi_limit <- ifelse(lim1 >= lim2, lim1, lim2)


# Plot the heatmap with ggplot2
ggplot(melted_data, aes(x = lag, y = pm25, fill = RR)) +
  geom_tile(colour = "gray70", linewidth = 0.2) +
  # scale_fill_viridis_c(option = "D") +
  scale_fill_gradient2(low = "royalblue1", mid = "white", high = "orangered",
    # low = "royalblue1", mid = "white", high = "violet",
                       # transform = "log10", 
                       midpoint = 1) +
# limits = c(lo_limit, hi_limit)) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Risk of Preterm Birth", x = "Gestational Week", y = "PM2.5 Concentration")


contrast_ratio("royalblue3", "mediumvioletred") # closest in contrast
contrast_ratio("royalblue1", "orangered")
contrast_ratio("royalblue1", "violet")









