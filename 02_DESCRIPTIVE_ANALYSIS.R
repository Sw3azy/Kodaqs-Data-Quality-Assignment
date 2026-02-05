# ==============================================================================
# Script: 02_descriptive_analysis.R
# Purpose: Descriptive statistics and visualizations
# Author: [Your Name]
# Date: February 2026
# ==============================================================================

cat("Starting descriptive analysis...\n")

# Load data ----
cat("Loading data...\n")
df <- read_csv("IntegratedData.csv")

cat(paste0("Dataset loaded: ", nrow(df), " rows, ", ncol(df), " columns\n\n"))

# 1. Total cases by state (top 10) ----
cat("Creating plot 1: Top 10 states by total cases...\n")

total_cases_by_state <- df %>%
  group_by(state) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  slice_head(n = 10)

p1 <- ggplot(total_cases_by_state, aes(x = reorder(state, total_cases), y = total_cases)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 States by Total COVID-19 Cases",
       subtitle = "Cumulative cases in 2021",
       x = "State",
       y = "Total Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot1_total_cases.png"), 
       p1, width = 10, height = 6, dpi = 300)

# 2. Average daily cases over time ----
cat("Creating plot 2: Average daily cases over time...\n")

daily_avg <- df %>%
  group_by(date) %>%
  summarize(avg_daily_cases = mean(daily_cases, na.rm = TRUE))

p2 <- ggplot(daily_avg, aes(x = date, y = avg_daily_cases)) +
  geom_line(color = "darkred", size = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Daily Cases Over Time",
       subtitle = "National average across all counties",
       x = "Date",
       y = "Average Daily Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot2_daily_avg.png"), 
       p2, width = 10, height = 6, dpi = 300)

# 3. 7-day moving average (California example) ----
cat("Creating plot 3: 7-day moving average (California)...\n")

df_ma <- df %>%
  filter(state == "california") %>%
  arrange(county, date) %>%
  group_by(county) %>%
  mutate(cases_7day_avg = rollmean(daily_cases, k = 7, fill = NA, align = "right")) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(daily_cases = sum(daily_cases, na.rm = TRUE),
            cases_7day_avg = sum(cases_7day_avg, na.rm = TRUE))

p3 <- ggplot(df_ma, aes(x = date)) +
  geom_line(aes(y = daily_cases), color = "gray70", alpha = 0.5, size = 0.5) +
  geom_line(aes(y = cases_7day_avg), color = "darkblue", size = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily Cases vs 7-Day Moving Average",
       subtitle = "California, 2021",
       x = "Date",
       y = "Cases",
       caption = "Gray: Daily cases | Blue: 7-day average") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot3_moving_avg.png"), 
       p3, width = 10, height = 6, dpi = 300)

# 4. Case fatality rate over time ----
cat("Creating plot 4: Case fatality rate over time...\n")

cfr_data <- df %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE),
            total_deaths = sum(deaths, na.rm = TRUE)) %>%
  mutate(cfr = (total_deaths / total_cases) * 100) %>%
  filter(!is.infinite(cfr), !is.na(cfr))

p4 <- ggplot(cfr_data, aes(x = date, y = cfr)) +
  geom_line(color = "darkgreen", size = 0.8) +
  labs(title = "Case Fatality Rate Over Time",
       subtitle = "National aggregate",
       x = "Date",
       y = "Case Fatality Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot4_cfr.png"), 
       p4, width = 10, height = 6, dpi = 300)

# 5. Weekend vs Weekday comparison ----
cat("Creating plot 5: Weekend vs weekday comparison...\n")

weekend_comparison <- df %>%
  mutate(day_type = ifelse(is_weekend == 1, "Weekend", "Weekday")) %>%
  group_by(day_type) %>%
  summarize(avg_daily_cases = mean(daily_cases, na.rm = TRUE),
            avg_daily_deaths = mean(daily_deaths, na.rm = TRUE))

p5 <- ggplot(weekend_comparison, aes(x = day_type, y = avg_daily_cases, fill = day_type)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Weekday" = "coral", "Weekend" = "lightblue")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Daily Cases: Weekday vs Weekend",
       subtitle = "Potential reporting effect",
       x = "",
       y = "Average Daily Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot5_weekend.png"), 
       p5, width = 10, height = 6, dpi = 300)

# 6. Mobility vs Cases correlation ----
cat("Creating plot 6: Mobility vs cases correlation...\n")

mobility_cases <- df %>%
  filter(!is.na(retail_recreation), !is.na(daily_cases)) %>%
  group_by(date) %>%
  summarize(avg_retail = mean(retail_recreation, na.rm = TRUE),
            avg_cases = mean(daily_cases, na.rm = TRUE))

# Save mobility data for regression analysis
saveRDS(mobility_cases, here("output", "tables", "mobility_cases.rds"))

p6 <- ggplot(mobility_cases, aes(x = avg_retail, y = avg_cases)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "darkviolet") +
  scale_y_continuous(labels = comma) +
  labs(title = "Retail/Recreation Mobility vs Daily Cases",
       subtitle = "Correlation between mobility and COVID-19 cases",
       x = "Retail & Recreation Mobility Change (%)",
       y = "Average Daily Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "plot6_mobility.png"), 
       p6, width = 10, height = 6, dpi = 300)

# Create combined plot ----
cat("Creating combined plot...\n")

combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6)

ggsave(here("output", "figures", "combined_descriptive.png"), 
       combined_plot, width = 14, height = 16, dpi = 300)

cat("Descriptive analysis complete!\n")
cat("Plots saved to output/figures/\n\n")