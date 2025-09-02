############################################################
# BME710 — Descriptive Statistics, Graphing, Correlation
# R (tidyverse-first). Runs in RStudio or Colab (with IRkernel).
# If a package is missing, run: install.packages("<name>")
############################################################

# ---- 0) Libraries ----
suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, ggplot2, readr, etc.
  library(readr)       # explicit CSV I/O
  library(janitor)     # clean_names, tabyl
  library(skimr)       # quick descriptive skim
  library(psych)       # describe() for descriptive stats
  library(GGally)      # ggpairs for pairplots
  library(corrplot)    # correlation heatmaps
})

# ---- 1) Data: load a simple biomedical-ish dataset ----
# Option A: use built-in 'mtcars' to keep it plug-and-play
# (Interpretable as: engine attributes ~ "physiology", mpg ~ "response")
df <- as_tibble(mtcars, rownames = "sample_id") |>
  clean_names()

# If you have a CSV, uncomment and point to it:
# df <- read_csv("path/to/your_data.csv") |> clean_names()

# ---- 2) Quick data overview ----
glimpse(df)
skimr::skim(df)         # variable types, missingness, basic stats

# Frequency tables for a categorical-like variable (create one for demo)
df <- df |>
  mutate(cyl_f = as.factor(cyl))

janitor::tabyl(df$cyl_f) |> janitor::adorn_totals("row")

# ---- 3) Descriptive stats (numeric) ----
# A. Overall describe:
psych::describe(df |> select(where(is.numeric)))

# B. Grouped describe (by cylinders)
df |> 
  group_by(cyl_f) |>
  summarise(
    n = n(),
    mpg_mean = mean(mpg, na.rm = TRUE),
    mpg_sd   = sd(mpg, na.rm = TRUE),
    wt_mean  = mean(wt, na.rm = TRUE),
    wt_sd    = sd(wt, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 4) Basic visualizations ----
# Histogram + density (mpg)
ggplot(df, aes(x = mpg)) +
  geom_histogram(bins = 15, alpha = 0.6) +
  geom_density(linewidth = 1) +
  labs(title = "MPG Distribution", x = "MPG", y = "Count")

# Boxplot by group (mpg by cylinders)
ggplot(df, aes(x = cyl_f, y = mpg)) +
  geom_boxplot() +
  labs(title = "MPG by Cylinder Count", x = "Cylinders", y = "MPG")

# Violin + box for another look (weight by cylinders)
ggplot(df, aes(x = cyl_f, y = wt)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(title = "Weight by Cylinders", x = "Cylinders", y = "Weight (1000 lbs)")

# Scatter with smoothing (mpg vs wt)
ggplot(df, aes(x = wt, y = mpg)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "MPG vs Weight", x = "Weight (1000 lbs)", y = "MPG")

# ---- 5) Correlation analysis ----
# Select numeric columns only
num_df <- df |> select(where(is.numeric))

# A. Correlation matrix (Pearson by default)
cor_mat <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")
cor_mat

# B. Visualize correlation matrix
corrplot(cor_mat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

# C. Pairwise scatterplot matrix (first 6 numeric vars for readability)
GGally::ggpairs(num_df |> select(1:6))

# D. Targeted correlation tests with p-values
#    Example: mpg vs wt (Pearson), then Spearman as robustness check
pearson_test <- cor.test(df$mpg, df$wt, method = "pearson")
spearman_test <- cor.test(df$mpg, df$wt, method = "spearman")

pearson_test
spearman_test

# E. Correlation with multiple variables against a target
target <- "mpg"
others <- setdiff(names(num_df), target)

tibble(
  variable = others,
  r_pearson = map_dbl(others, ~ cor(num_df[[target]], num_df[[.x]],
                                    use = "pairwise.complete.obs",
                                    method = "pearson")),
  r_spearman = map_dbl(others, ~ cor(num_df[[target]], num_df[[.x]],
                                     use = "pairwise.complete.obs",
                                     method = "spearman"))
) |>
  arrange(desc(abs(r_pearson)))

# ---- 6) Handling missing data (demo pattern) ----
# Create a tiny missingness to show approach
set.seed(42)
df_miss <- df
nas <- sample(seq_len(nrow(df_miss)), size = 3)
df_miss$mpg[nas] <- NA

# Check missingness quickly
colSums(is.na(df_miss))

# Simple strategy: drop rows with NA for analysis in question
df_complete <- df_miss |> drop_na(mpg, wt)
cor.test(df_complete$mpg, df_complete$wt)

# Alternative: impute (mean/median) — quick demo
df_impute <- df_miss |>
  mutate(
    mpg_imp = if_else(is.na(mpg), median(mpg, na.rm = TRUE), mpg)
  )
cor.test(df_impute$mpg_imp, df_impute$wt)

# ---- 7) Save outputs (figures/tables) ----
# Example: save a correlation heatmap as PNG
png("correlation_heatmap.png", width = 1200, height = 900, res = 150)
corrplot(cor_mat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
dev.off()

# Example: write a summary table to CSV
summary_tbl <- df |>
  group_by(cyl_f) |>
  summarise(
    n = n(),
    mpg_mean = mean(mpg, na.rm = TRUE),
    mpg_sd   = sd(mpg, na.rm = TRUE),
    wt_mean  = mean(wt, na.rm = TRUE),
    wt_sd    = sd(wt, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_tbl, "summary_by_cyl.csv")

# ---- 8) Nice-to-have: reusable helper functions ----
desc_by <- function(data, group, vars) {
  data |>
    group_by({{ group }}) |>
    summarise(
      across({{ vars }},
             list(mean = ~ mean(.x, na.rm = TRUE),
                  sd   = ~ sd(.x, na.rm = TRUE),
                  min  = ~ min(.x, na.rm = TRUE),
                  q25  = ~ quantile(.x, 0.25, na.rm = TRUE),
                  med  = ~ median(.x, na.rm = TRUE),
                  q75  = ~ quantile(.x, 0.75, na.rm = TRUE),
                  max  = ~ max(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
}

desc_by(df, cyl_f, c(mpg, wt))

# ---- 9) Session info for reproducibility ----
sessionInfo()
