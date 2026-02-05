# ==============================================================================
# Script: 03_regression_analysis.R
# Purpose: Regression modeling and statistical analysis
# Author: [Your Name]
# Date: February 2026
# ==============================================================================

cat("Starting regression analysis...\n")

# Load data ----
cat("Loading data...\n")
df <- read_csv("IntegratedData.csv")

mobility_cases <- readRDS(here("output", "tables", "mobility_cases.rds"))

# 7. Simple Linear Regression: Mobility vs Cases ----
cat("Running simple linear regression...\n")

mobility_model <- lm(avg_cases ~ avg_retail, data = mobility_cases)

# Extract model statistics
r_squared <- summary(mobility_model)$r.squared
p_value <- summary(mobility_model)$coefficients[2, 4]
intercept <- coef(mobility_model)[1]
slope <- coef(mobility_model)[2]

# Create equation label
eq_label <- paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))
r2_label <- paste0("R² = ", round(r_squared, 3))
p_label <- ifelse(p_value < 0.001, "p < 0.001", paste0("p = ", round(p_value, 3)))

p7 <- ggplot(mobility_cases, aes(x = avg_retail, y = avg_cases)) +
  geom_point(alpha = 0.5, color = "purple", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkviolet", fill = "lavender") +
  annotate("text", 
           x = min(mobility_cases$avg_retail) + 5,
           y = max(mobility_cases$avg_cases) * 0.95,
           label = paste(eq_label, "\n", r2_label, "\n", p_label),
           hjust = 0, vjust = 1,
           size = 4.5, 
           fontface = "bold",
           color = "darkblue") +
  scale_y_continuous(labels = comma) +
  labs(title = "Impact of Retail Mobility on Daily COVID-19 Cases",
       subtitle = "Linear regression showing relationship between mobility and case counts",
       x = "Retail & Recreation Mobility Change (%)",
       y = "Average Daily Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10))

ggsave(here("output", "figures", "plot7_mobility_regression.png"), 
       p7, width = 10, height = 6, dpi = 300)

# 8. Multiple Regression: All mobility factors ----
cat("Running multiple regression...\n")

mobility_full <- df %>%
  filter(!is.na(retail_recreation), !is.na(daily_cases),
         !is.na(grocery_pharmacy), !is.na(parks),
         !is.na(transit), !is.na(workplaces)) %>%
  group_by(date) %>%
  summarize(
    avg_cases = mean(daily_cases, na.rm = TRUE),
    retail = mean(retail_recreation, na.rm = TRUE),
    grocery = mean(grocery_pharmacy, na.rm = TRUE),
    parks = mean(parks, na.rm = TRUE),
    transit = mean(transit, na.rm = TRUE),
    workplaces = mean(workplaces, na.rm = TRUE),
    residential = mean(residential, na.rm = TRUE)
  )

full_model <- lm(avg_cases ~ retail + grocery + parks + transit + workplaces + residential, 
                 data = mobility_full)

# Extract coefficients
coef_summary <- summary(full_model)$coefficients
coef_data <- data.frame(
  term = c("Retail/Recreation", "Grocery/Pharmacy", "Parks", 
           "Transit", "Workplaces", "Residential"),
  estimate = coef_summary[-1, 1],
  std.error = coef_summary[-1, 2],
  p.value = coef_summary[-1, 4]
) %>%
  mutate(significant = p.value < 0.05)

p8 <- ggplot(coef_data, aes(x = reorder(term, estimate), y = estimate, fill = significant)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                width = 0.2, size = 1) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70"),
                    labels = c("Not Significant", "Significant (p < 0.05)")) +
  labs(title = "Multiple Regression: Mobility Factors Predicting COVID-19 Cases",
       subtitle = "Coefficient estimates with standard errors",
       x = "Mobility Category",
       y = "Coefficient Estimate",
       fill = "Statistical Significance",
       caption = paste0("Adjusted R² = ", round(summary(full_model)$adj.r.squared, 3))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(here("output", "figures", "plot8_multiple_regression.png"), 
       p8, width = 10, height = 6, dpi = 300)

# 9. Time-lagged regression ----
cat("Running time-lagged regression...\n")

mobility_lagged <- mobility_cases %>%
  arrange(date) %>%
  mutate(
    retail_lag7 = lag(avg_retail, 7),
    retail_lag14 = lag(avg_retail, 14)
  ) %>%
  filter(!is.na(retail_lag7), !is.na(retail_lag14))

model_lag7 <- lm(avg_cases ~ retail_lag7, data = mobility_lagged)
model_lag14 <- lm(avg_cases ~ retail_lag14, data = mobility_lagged)

lag_comparison <- data.frame(
  Lag = factor(c("No Lag", "7-Day Lag", "14-Day Lag"), 
               levels = c("No Lag", "7-Day Lag", "14-Day Lag")),
  R_squared = c(
    summary(mobility_model)$r.squared,
    summary(model_lag7)$r.squared,
    summary(model_lag14)$r.squared
  )
)

p9 <- ggplot(lag_comparison, aes(x = Lag, y = R_squared, fill = Lag)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = sprintf("R² = %.3f", R_squared)), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB")) +
  ylim(0, max(lag_comparison$R_squared) * 1.15) +
  labs(title = "Predictive Power of Mobility on Future COVID-19 Cases",
       subtitle = "Comparing different time lags",
       x = "Time Lag",
       y = "R-squared Value") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot9_lagged_regression.png"), 
       p9, width = 8, height = 6, dpi = 300)

# 10. Weekend effect regression ----
cat("Running weekend effect regression...\n")

weekend_model_data <- df %>%
  mutate(is_weekend_factor = factor(is_weekend, labels = c("Weekday", "Weekend"))) %>%
  filter(!is.na(daily_cases))

weekend_model <- lm(daily_cases ~ is_weekend_factor, data = weekend_model_data)

weekend_pred <- weekend_model_data %>%
  group_by(is_weekend_factor) %>%
  summarize(
    mean_cases = mean(daily_cases, na.rm = TRUE),
    se = sd(daily_cases, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean_cases - 1.96 * se,
    ci_upper = mean_cases + 1.96 * se
  )

p10 <- ggplot(weekend_pred, aes(x = is_weekend_factor, y = mean_cases, fill = is_weekend_factor)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +
  geom_text(aes(label = sprintf("%.1f", mean_cases)), 
            vjust = -1.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Weekday" = "coral", "Weekend" = "lightblue")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Weekend Effect on COVID-19 Cases",
       subtitle = "Mean daily cases with 95% confidence intervals",
       x = "",
       y = "Average Daily Cases",
       caption = paste0("Difference p-value: ", 
                        format.pval(summary(weekend_model)$coefficients[2,4], digits = 3))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot10_weekend_regression.png"), 
       p10, width = 8, height = 6, dpi = 300)

# Save regression summaries ----
cat("Saving regression summaries...\n")

sink(here("output", "tables", "regression_summaries.txt"))
cat("=============================================================\n")
cat("REGRESSION ANALYSIS SUMMARIES - COVID-19 MOBILITY STUDY\n")
cat("=============================================================\n\n")

cat("1. Simple Linear Regression: Mobility vs Cases\n")
cat("-----------------------------------------------\n")
print(summary(mobility_model))

cat("\n\n2. Multiple Regression: All Mobility Factors\n")
cat("--------------------------------------------\n")
print(summary(full_model))

cat("\n\n3. Time-Lagged Models\n")
cat("---------------------\n")
cat("7-day lag R²:", summary(model_lag7)$r.squared, "\n")
cat("14-day lag R²:", summary(model_lag14)$r.squared, "\n")
print(summary(model_lag7))

cat("\n\n4. Weekend Effect Model\n")
cat("-----------------------\n")
print(summary(weekend_model))

sink()

# Save model comparison table ----
model_comparison <- data.frame(
  Model = c("Simple Regression", "Multiple Regression", "7-Day Lag", 
            "14-Day Lag", "Weekend Effect"),
  R_squared = c(
    round(summary(mobility_model)$r.squared, 4),
    round(summary(full_model)$r.squared, 4),
    round(summary(model_lag7)$r.squared, 4),
    round(summary(model_lag14)$r.squared, 4),
    round(summary(weekend_model)$r.squared, 4)
  ),
  Adj_R_squared = c(
    round(summary(mobility_model)$adj.r.squared, 4),
    round(summary(full_model)$adj.r.squared, 4),
    round(summary(model_lag7)$adj.r.squared, 4),
    round(summary(model_lag14)$adj.r.squared, 4),
    round(summary(weekend_model)$adj.r.squared, 4)
  )
)

write_csv(model_comparison, here("output", "tables", "model_comparison.csv"))

cat("Regression analysis complete!\n")
cat("Results saved to output/tables/\n")
cat("Plots saved to output/figures/\n\n")