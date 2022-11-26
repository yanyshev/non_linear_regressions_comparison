library(dplyr)
library(cowplot)
library(ggplot2)
library(broom)
source_data <- tibble(
  response = c(2406, 2464, 2336, 2281, 2641, 2385, 2297, 2416, 2460, 2549, 2311, 2278, 2240, 2183, 2408, 2379, 2378, 2554, 2232, 2356),
  explanatory = c(2508, 2572, 2408, 2522, 2700, 2531, 2390, 2595, 2524, 2685, 2435, 2354, 2404, 2381, 2581, 2529, 2562, 2624, 2407, 2448)
)
data <- source_data %>%
  rename(
    x = explanatory,
    y = response
  )
data <- data %>%
  mutate(log10_y = log10(y), log10_x = log10(x), inverse_x = 1/x)
means <- colMeans(data)
sds <- apply(data, 2, FUN = sd)
correls <- data %>%
  summarize(
    correl_XY = cor(x, y),
    correl_logYlogX = cor(log10_y, log10_x),
    correl_Y_inverseX = cor(y, inverse_x),
    correl_Y_logX = cor(y, log10_x)
  )
elasticity <- function(b_fitted, x_mean, y_mean) {
  b_fitted * x_mean / y_mean
}
approx_error <- function(sample, vector, b_fitted) {
  sample %>%
    mutate(diff = vector - as.numeric(b_fitted)) %>%
    mutate(diff_2 = abs(diff / vector)) %>%
    summarize(error = sum(diff_2) / length(vector)) %>% pull(error)
}
regression_parameters <- function(mdl, vector, x_mean, y_mean) {
  r_squared <- round(glance(mdl) %>% pull(r.squared), 4)
  coeffs <- coefficients(mdl)
  confidence <- tidy(mdl) %>% select(term, statistic, p.value)
  conf_interval <- tibble(
    intercept = confint(mdl, names(mdl$coefficients)[1], .95),
    x = confint(mdl, names(mdl$coefficients)[2], .95)
  )
  parms <- list(
    elast = as.numeric(elasticity(mdl$coefficients[2], x_mean, y_mean)),
    approx = approx_error(data, vector, mdl$fitted.values)
  )
  return(
    list(
      r_squared = r_squared,
      coeffs = coeffs,
      condidence = confidence,
      conf_interval = conf_interval,
      parms = parms
    )
  )
}
linear <- lm(
  y ~ x, data = data
)
mdl_linear <- regression_parameters(linear, data$y, means[2], means[1])
power <- lm(
  log10_y ~ log10_x, data = data
)
mdl_power <- regression_parameters(power, data$log10_y, means[4], means[3])
inverse <- lm(
  y ~ inverse_x, data = data
)
mdl_inverse <- regression_parameters(inverse, data$y, means[5], means[1])
log <- lm(
  y ~ log10_x, data = data
)
mdl_log <- regression_parameters(log, data$y, means[4], means[1])
plot_linear <- ggplot(
  source_data, aes(x = explanatory, response)
) + geom_point() + geom_smooth(method = "lm", se = F) +
  labs(title = "Linear", subtitle = mdl_linear$r_squared)
plot_power <- ggplot(
  source_data, aes(x = log10(explanatory), log10(response))
) + geom_point() + geom_smooth(formula = "y ~ x", se = F) +
  labs(title = "Power", subtitle = mdl_power$r_squared)
plot_inverse <- ggplot(
  source_data, aes(x = 1/explanatory, response)
) + geom_point() + geom_smooth(formula = "y ~ x", se = F) +
  labs(title = "Hyperbolic", subtitle = mdl_inverse$r_squared)
plot_exponential <- ggplot(
  source_data, aes(x = log10(explanatory), response)
) + geom_point() + geom_smooth(formula = "y ~ x", se = F) +
  labs(title = "Exponential", subtitle = mdl_log$r_squared)
plot_grid(
  plot_linear, plot_power, plot_inverse, plot_exponential,
  ncol = 2, nrow = 2
)