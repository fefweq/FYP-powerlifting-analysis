# Final Year Project R Code
# Title: Age and Bodyweight Effects on Absolute and Relative Powerlifting Performance:
#        A Segmented Regression Analysis of OpenPowerlifting Data
# Student: Ruizhe Zhou
# Student ID: 2252653
# Data source: OpenPowerlifting
# Download date: November 22, 2025
#
# Public supplementary analysis script.
#
# Instructions:
# 1. Create a folder named "data" in the same directory as this script.
# 2. Place the raw OpenPowerlifting CSV used for the final analysis in "data".
# 3. The "data" folder should contain only one raw OpenPowerlifting CSV file.
# 4. Run this script from the directory containing this file.
#
# This script reproduces the main data cleaning, descriptive analysis,
# regression modelling, segmented regression, bootstrap breakpoint analysis,
# and figure/table generation used in the FYP report.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(segmented)
library(writexl)
library(grid)

set.seed(20260519)

raw_dir <- "data"
cleaned_dir <- file.path("outputs", "data_cleaned")
tables_dir <- file.path("outputs", "tables")
figures_dir <- file.path("outputs", "figures")

dir.create(cleaned_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

cleaned_file <- file.path(cleaned_dir, "opl_ipf_raw_sbd_cleaned.csv")


# 1. Data Import and Sample Selection -------------------------------------

# The data folder should contain only the OpenPowerlifting CSV used for the
# final analysis.
raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(raw_files) == 0) {
  stop("No raw CSV file found.")
}

if (length(raw_files) > 1) {
  stop("More than one raw CSV file found: ", paste(basename(raw_files), collapse = ", "))
}

opl_raw <- read_csv(raw_files[1], show_col_types = FALSE)

cleaning_flow <- tibble(
  step = "Raw data",
  rows_remaining = nrow(opl_raw)
)

add_flow_step <- function(flow, step_name, data) {
  bind_rows(flow, tibble(step = step_name, rows_remaining = nrow(data)))
}

opl_clean <- opl_raw %>%
  dplyr::filter(Federation == "IPF")
cleaning_flow <- add_flow_step(cleaning_flow, "Keep Federation == IPF", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::filter(Equipment == "Raw")
cleaning_flow <- add_flow_step(cleaning_flow, "Keep Equipment == Raw", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::filter(Event == "SBD")
cleaning_flow <- add_flow_step(cleaning_flow, "Keep Event == SBD", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::filter(Sex %in% c("M", "F"))
cleaning_flow <- add_flow_step(cleaning_flow, "Keep Sex values M and F", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::select(
    Sex,
    Age,
    BodyweightKg,
    Best3SquatKg,
    Best3BenchKg,
    Best3DeadliftKg,
    TotalKg,
    Dots
  )
cleaning_flow <- add_flow_step(cleaning_flow, "Keep selected variables", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::filter(
    !is.na(Age),
    !is.na(BodyweightKg),
    !is.na(TotalKg),
    !is.na(Dots)
  )
cleaning_flow <- add_flow_step(
  cleaning_flow,
  "Remove missing Age, BodyweightKg, TotalKg, or Dots",
  opl_clean
)

opl_clean <- opl_clean %>%
  dplyr::filter(TotalKg > 0)
cleaning_flow <- add_flow_step(cleaning_flow, "Keep TotalKg > 0", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::filter(Age >= 16, Age <= 70)
cleaning_flow <- add_flow_step(cleaning_flow, "Restrict Age to 16-70 years", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::filter(BodyweightKg >= 40, BodyweightKg <= 200)
cleaning_flow <- add_flow_step(cleaning_flow, "Restrict BodyweightKg to 40-200 kg", opl_clean)

opl_clean <- opl_clean %>%
  dplyr::filter(
    !is.na(Best3SquatKg),
    !is.na(Best3BenchKg),
    !is.na(Best3DeadliftKg),
    Best3SquatKg > 0,
    Best3BenchKg > 0,
    Best3DeadliftKg > 0
  )
cleaning_flow <- add_flow_step(
  cleaning_flow,
  "Remove missing or non-positive best lift values",
  opl_clean
)

write_csv(opl_clean, cleaned_file)
write_csv(cleaning_flow, file.path(tables_dir, "data_cleaning_flow.csv"))


# 2. Descriptive Statistics and Table Generation --------------------------

continuous_vars <- c(
  "Age",
  "BodyweightKg",
  "Best3SquatKg",
  "Best3BenchKg",
  "Best3DeadliftKg",
  "TotalKg",
  "Dots"
)

summarise_sex <- function(data, group_label) {
  data %>%
    count(Sex, name = "n") %>%
    dplyr::mutate(
      group = group_label,
      variable = "Sex",
      category = Sex,
      percent = 100 * n / sum(n),
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      median = NA_real_,
      max = NA_real_
    ) %>%
    dplyr::select(group, variable, category, n, percent, mean, sd, min, median, max)
}

summarise_continuous <- function(data, group_label) {
  data %>%
    dplyr::summarise(
      across(
        all_of(continuous_vars),
        list(
          n = ~sum(!is.na(.x)),
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", ".value"),
      names_sep = "__"
    ) %>%
    dplyr::mutate(
      group = group_label,
      category = NA_character_,
      percent = NA_real_
    ) %>%
    dplyr::select(group, variable, category, n, percent, mean, sd, min, median, max)
}

make_group_summary <- function(data, group_label) {
  bind_rows(
    summarise_sex(data, group_label),
    summarise_continuous(data, group_label)
  )
}

table1_stats <- bind_rows(
  make_group_summary(opl_clean, "Overall"),
  make_group_summary(dplyr::filter(opl_clean, Sex == "M"), "M"),
  make_group_summary(dplyr::filter(opl_clean, Sex == "F"), "F")
)

format_number <- function(x) {
  sprintf("%.2f", x)
}

format_n_percent <- function(n, denominator) {
  paste0(n, " (", format_number(100 * n / denominator), "%)")
}

format_mean_sd <- function(summary_row) {
  paste0(format_number(summary_row$mean), " (", format_number(summary_row$sd), ")")
}

format_median_range <- function(summary_row) {
  paste0(
    format_number(summary_row$median),
    " [",
    format_number(summary_row$min),
    ", ",
    format_number(summary_row$max),
    "]"
  )
}

get_continuous_summary <- function(group_label, variable_name) {
  table1_stats %>%
    dplyr::filter(group == group_label, variable == variable_name) %>%
    slice(1)
}

get_sample_size <- function(group_label) {
  get_continuous_summary(group_label, "Age")$n
}

get_sex_summary <- function(group_label, sex_value) {
  denominator <- get_sample_size(group_label)
  sex_row <- table1_stats %>%
    dplyr::filter(group == group_label, variable == "Sex", category == sex_value) %>%
    slice(1)
  n <- if (nrow(sex_row) == 0) 0 else sex_row$n
  format_n_percent(n, denominator)
}

make_clean_value <- function(group_label, variable_name, statistic) {
  summary_row <- get_continuous_summary(group_label, variable_name)
  if (statistic == "mean_sd") {
    format_mean_sd(summary_row)
  } else {
    format_median_range(summary_row)
  }
}

table1_rows <- tibble(
  Variable = c(
    "Sample size, n",
    "Female, n (%)",
    "Male, n (%)",
    "Age, mean (SD)",
    "Age, median [min, max]",
    "BodyweightKg, mean (SD)",
    "BodyweightKg, median [min, max]",
    "Best3SquatKg, mean (SD)",
    "Best3SquatKg, median [min, max]",
    "Best3BenchKg, mean (SD)",
    "Best3BenchKg, median [min, max]",
    "Best3DeadliftKg, mean (SD)",
    "Best3DeadliftKg, median [min, max]",
    "TotalKg, mean (SD)",
    "TotalKg, median [min, max]",
    "Dots, mean (SD)",
    "Dots, median [min, max]"
  ),
  variable_name = c(NA, NA, NA, rep(continuous_vars, each = 2)),
  statistic = c(
    "sample_size",
    "female",
    "male",
    rep(c("mean_sd", "median_range"), times = length(continuous_vars))
  )
)

table1 <- table1_rows %>%
  rowwise() %>%
  dplyr::mutate(
    Overall = case_when(
      statistic == "sample_size" ~ as.character(get_sample_size("Overall")),
      statistic == "female" ~ get_sex_summary("Overall", "F"),
      statistic == "male" ~ get_sex_summary("Overall", "M"),
      statistic == "mean_sd" ~ make_clean_value("Overall", variable_name, statistic),
      TRUE ~ make_clean_value("Overall", variable_name, statistic)
    ),
    Male = case_when(
      statistic == "sample_size" ~ as.character(get_sample_size("M")),
      statistic == "female" ~ get_sex_summary("M", "F"),
      statistic == "male" ~ get_sex_summary("M", "M"),
      statistic == "mean_sd" ~ make_clean_value("M", variable_name, statistic),
      TRUE ~ make_clean_value("M", variable_name, statistic)
    ),
    Female = case_when(
      statistic == "sample_size" ~ as.character(get_sample_size("F")),
      statistic == "female" ~ get_sex_summary("F", "F"),
      statistic == "male" ~ get_sex_summary("F", "M"),
      statistic == "mean_sd" ~ make_clean_value("F", variable_name, statistic),
      TRUE ~ make_clean_value("F", variable_name, statistic)
    )
  ) %>%
  ungroup() %>%
  dplyr::select(Variable, Overall, Male, Female)

write_csv(table1, file.path(tables_dir, "table1_sample_characteristics_clean.csv"))
write_xlsx(table1, file.path(tables_dir, "table1_sample_characteristics_clean.xlsx"))


# 3. Exploratory Figures ---------------------------------------------------

opl_plot <- opl_clean %>%
  dplyr::mutate(Sex = factor(Sex, levels = c("F", "M"), labels = c("Female", "Male")))

sex_colors <- c("Female" = "#B04A5A", "Male" = "#2F5D8C")
neutral_fill <- "#BCC6D6"
neutral_line <- "#2B2B2B"

theme_pub <- theme_classic(base_size = 11, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10.5, face = "bold", hjust = 0),
    axis.title = element_text(size = 10.5, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 9.5),
    strip.background = element_rect(fill = "#F2F2F2", color = "#6F6F6F", linewidth = 0.45),
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(8, 10, 8, 8)
  )

save_multipanel <- function(plots, filename, width = 8.0, height = 6.2) {
  png(
    filename = file.path(figures_dir, filename),
    width = width,
    height = height,
    units = "in",
    res = 320,
    bg = "white"
  )
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2)))
  print(plots[[1]], vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(plots[[2]], vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(plots[[3]], vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(plots[[4]], vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  dev.off()
}

save_two_panel <- function(plots, filename, width = 8.4, height = 3.4) {
  png(
    filename = file.path(figures_dir, filename),
    width = width,
    height = height,
    units = "in",
    res = 320,
    bg = "white"
  )
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(plots[[1]], vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(plots[[2]], vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
}

make_histogram <- function(data, variable, x_label, binwidth, panel_label) {
  ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(
      binwidth = binwidth,
      fill = neutral_fill,
      color = neutral_line,
      linewidth = 0.28,
      boundary = 0
    ) +
    labs(title = panel_label, x = x_label, y = "Frequency") +
    theme_pub +
    theme(legend.position = "none")
}

figure1_panels <- list(
  make_histogram(opl_clean, "Age", "Age (years)", 2, "Panel A. Age"),
  make_histogram(opl_clean, "BodyweightKg", "BodyweightKg (kg)", 5, "Panel B. BodyweightKg"),
  make_histogram(opl_clean, "TotalKg", "TotalKg (kg)", 25, "Panel C. TotalKg"),
  make_histogram(opl_clean, "Dots", "Dots score", 20, "Panel D. Dots")
)

save_multipanel(figure1_panels, "figure1_distributions_multipanel.png")

make_loess_plot <- function(data, x_var, y_var, x_label, y_label, panel_label) {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = Sex)) +
    geom_point(alpha = 0.28, size = 0.60, stroke = 0) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 1.15, alpha = 0.16) +
    scale_color_manual(values = sex_colors) +
    labs(title = panel_label, x = x_label, y = y_label) +
    theme_pub
}

figure2_panels <- list(
  make_loess_plot(opl_plot, "Age", "TotalKg", "Age (years)", "TotalKg (kg)", "Panel A. TotalKg and Age"),
  make_loess_plot(opl_plot, "BodyweightKg", "TotalKg", "BodyweightKg (kg)", "TotalKg (kg)", "Panel B. TotalKg and BodyweightKg"),
  make_loess_plot(opl_plot, "Age", "Dots", "Age (years)", "Dots score", "Panel C. Dots and Age"),
  make_loess_plot(opl_plot, "BodyweightKg", "Dots", "BodyweightKg (kg)", "Dots score", "Panel D. Dots and BodyweightKg")
)

save_multipanel(figure2_panels, "figure2_loess_multipanel.png", width = 8.4, height = 6.3)


# 4. Baseline Linear and Quadratic Regression -----------------------------

model_specs <- tibble(
  Outcome = c(
    "TotalKg", "TotalKg", "TotalKg", "TotalKg",
    "Dots", "Dots", "Dots", "Dots"
  ),
  Predictor = c(
    "Age", "Age", "BodyweightKg", "BodyweightKg",
    "Age", "Age", "BodyweightKg", "BodyweightKg"
  ),
  Model_type = c(
    "Linear", "Quadratic", "Linear", "Quadratic",
    "Linear", "Quadratic", "Linear", "Quadratic"
  ),
  Formula = c(
    "TotalKg ~ Age + Sex",
    "TotalKg ~ Age + I(Age^2) + Sex",
    "TotalKg ~ BodyweightKg + Sex",
    "TotalKg ~ BodyweightKg + I(BodyweightKg^2) + Sex",
    "Dots ~ Age + Sex",
    "Dots ~ Age + I(Age^2) + Sex",
    "Dots ~ BodyweightKg + Sex",
    "Dots ~ BodyweightKg + I(BodyweightKg^2) + Sex"
  )
)

calculate_rmse <- function(model) {
  sqrt(mean(residuals(model)^2))
}

fit_model_summary <- function(outcome, predictor, model_type, formula_text, data) {
  model <- lm(as.formula(formula_text), data = data)
  model_summary <- summary(model)
  tibble(
    Outcome = outcome,
    Predictor = predictor,
    Model_type = model_type,
    Formula = formula_text,
    n = nobs(model),
    R_squared = model_summary$r.squared,
    Adjusted_R_squared = model_summary$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model),
    RMSE = calculate_rmse(model)
  )
}

baseline_results <- bind_rows(
  lapply(
    seq_len(nrow(model_specs)),
    function(i) {
      fit_model_summary(
        outcome = model_specs$Outcome[i],
        predictor = model_specs$Predictor[i],
        model_type = model_specs$Model_type[i],
        formula_text = model_specs$Formula[i],
        data = dplyr::mutate(opl_clean, Sex = factor(Sex))
      )
    }
  )
) %>%
  dplyr::mutate(
    R_squared = round(R_squared, 4),
    Adjusted_R_squared = round(Adjusted_R_squared, 4),
    AIC = round(AIC, 2),
    BIC = round(BIC, 2),
    RMSE = round(RMSE, 2)
  )

write_csv(baseline_results, file.path(tables_dir, "table2_baseline_model_comparison.csv"))
write_xlsx(baseline_results, file.path(tables_dir, "table2_baseline_model_comparison.xlsx"))


# 5. Sex-Specific Segmented Regression ------------------------------------

segmented_specs <- tibble(
  Outcome = c(
    "TotalKg", "TotalKg", "TotalKg", "TotalKg",
    "Dots", "Dots", "Dots", "Dots"
  ),
  Sex = c(
    "Male", "Female", "Male", "Female",
    "Male", "Female", "Male", "Female"
  ),
  Predictor = c(
    "Age", "Age", "BodyweightKg", "BodyweightKg",
    "Age", "Age", "BodyweightKg", "BodyweightKg"
  )
)

fit_segmented_model <- function(data, outcome, sex_label, predictor) {
  model_data <- data %>%
    dplyr::filter(Sex == sex_label) %>%
    dplyr::select(all_of(c(outcome, predictor))) %>%
    dplyr::filter(complete.cases(.))

  base_model <- lm(as.formula(paste(outcome, "~", predictor)), data = model_data)
  start_psi <- median(model_data[[predictor]], na.rm = TRUE)
  seg_model <- segmented(
    base_model,
    seg.Z = as.formula(paste("~", predictor)),
    psi = start_psi,
    control = seg.control(display = FALSE)
  )

  model_summary <- summary(seg_model)
  breakpoint <- seg_model$psi[1, "Est."]
  slopes <- slope(seg_model)[[predictor]][, "Est."]

  tibble(
    Outcome = outcome,
    Sex = sex_label,
    Predictor = predictor,
    n = nobs(seg_model),
    Breakpoint_tau = breakpoint,
    Slope_before_breakpoint = slopes[1],
    Slope_after_breakpoint = slopes[2],
    R_squared = model_summary$r.squared,
    Adjusted_R_squared = model_summary$adj.r.squared,
    AIC = AIC(seg_model),
    BIC = BIC(seg_model),
    RMSE = calculate_rmse(seg_model)
  )
}

segmented_results <- bind_rows(
  lapply(
    seq_len(nrow(segmented_specs)),
    function(i) {
      fit_segmented_model(
        data = opl_plot,
        outcome = segmented_specs$Outcome[i],
        sex_label = segmented_specs$Sex[i],
        predictor = segmented_specs$Predictor[i]
      )
    }
  )
) %>%
  dplyr::mutate(
    Breakpoint_tau = round(Breakpoint_tau, 2),
    Slope_before_breakpoint = round(Slope_before_breakpoint, 4),
    Slope_after_breakpoint = round(Slope_after_breakpoint, 4),
    R_squared = round(R_squared, 4),
    Adjusted_R_squared = round(Adjusted_R_squared, 4),
    AIC = round(AIC, 2),
    BIC = round(BIC, 2),
    RMSE = round(RMSE, 2)
  )

write_csv(segmented_results, file.path(tables_dir, "table3_segmented_regression_results.csv"))
write_xlsx(segmented_results, file.path(tables_dir, "table3_segmented_regression_results.xlsx"))

fit_segmented_for_prediction <- function(data, outcome, sex_label, predictor, breakpoint) {
  model_data <- data %>%
    dplyr::filter(Sex == sex_label) %>%
    dplyr::select(all_of(c(outcome, predictor))) %>%
    dplyr::filter(complete.cases(.))

  base_model <- lm(as.formula(paste(outcome, "~", predictor)), data = model_data)
  segmented(
    base_model,
    seg.Z = as.formula(paste("~", predictor)),
    psi = breakpoint,
    control = seg.control(display = FALSE)
  )
}

make_prediction_data <- function(data, results, outcome, predictor) {
  bind_rows(
    lapply(
      c("Female", "Male"),
      function(sex_label) {
        model_data <- data %>%
          dplyr::filter(Sex == sex_label) %>%
          dplyr::select(all_of(c(outcome, predictor))) %>%
          dplyr::filter(complete.cases(.))

        breakpoint <- results %>%
          dplyr::filter(Outcome == outcome, Sex == sex_label, Predictor == predictor) %>%
          pull(Breakpoint_tau)

        seg_model <- fit_segmented_for_prediction(
          data,
          outcome,
          sex_label,
          predictor,
          breakpoint
        )

        prediction_data <- tibble(
          !!predictor := seq(
            min(model_data[[predictor]], na.rm = TRUE),
            max(model_data[[predictor]], na.rm = TRUE),
            length.out = 250
          )
        )
        prediction_data$Predicted <- predict(seg_model, newdata = prediction_data)
        prediction_data$Sex <- sex_label
        prediction_data
      }
    )
  )
}

make_segmented_plot <- function(data, results, outcome, predictor, x_label, y_label, panel_label) {
  prediction_data <- make_prediction_data(data, results, outcome, predictor)
  breakpoint_data <- results %>%
    dplyr::filter(Outcome == outcome, Predictor == predictor)

  ggplot(data, aes(x = .data[[predictor]], y = .data[[outcome]])) +
    geom_point(alpha = 0.24, size = 0.52, color = "#555555", stroke = 0) +
    geom_line(
      data = prediction_data,
      aes(x = .data[[predictor]], y = Predicted, color = Sex),
      linewidth = 1.20
    ) +
    geom_vline(
      data = breakpoint_data,
      aes(xintercept = Breakpoint_tau),
      linetype = "dashed",
      color = "#202020",
      linewidth = 0.80
    ) +
    facet_wrap(~Sex, nrow = 1) +
    scale_color_manual(values = sex_colors, guide = "none") +
    labs(title = panel_label, x = x_label, y = y_label) +
    theme_pub
}

figure3_panels <- list(
  make_segmented_plot(opl_plot, segmented_results, "TotalKg", "Age", "Age (years)", "TotalKg (kg)", "Panel A. Age"),
  make_segmented_plot(opl_plot, segmented_results, "TotalKg", "BodyweightKg", "BodyweightKg (kg)", "TotalKg (kg)", "Panel B. BodyweightKg")
)

figure4_panels <- list(
  make_segmented_plot(opl_plot, segmented_results, "Dots", "Age", "Age (years)", "Dots score", "Panel A. Age"),
  make_segmented_plot(opl_plot, segmented_results, "Dots", "BodyweightKg", "BodyweightKg (kg)", "Dots score", "Panel B. BodyweightKg")
)

save_two_panel(
  figure3_panels,
  "figure3_segmented_totalkg_multipanel.png",
  width = 8.4,
  height = 3.4
)

save_two_panel(
  figure4_panels,
  "figure4_segmented_dots_multipanel.png",
  width = 8.4,
  height = 3.4
)


# 6. Bootstrap Confidence Intervals ---------------------------------------

bootstrap_resamples <- 1000

bootstrap_specs <- tibble(
  Outcome = "Dots",
  Sex = c("Male", "Female", "Male", "Female"),
  Predictor = c("Age", "Age", "BodyweightKg", "BodyweightKg")
)

fit_tau <- function(data, outcome, predictor, start_psi = NULL) {
  model_data <- data %>%
    dplyr::select(all_of(c(outcome, predictor))) %>%
    dplyr::filter(complete.cases(.))

  if (is.null(start_psi)) {
    start_psi <- median(model_data[[predictor]], na.rm = TRUE)
  }

  base_model <- lm(as.formula(paste(outcome, "~", predictor)), data = model_data)
  seg_model <- segmented(
    base_model,
    seg.Z = as.formula(paste("~", predictor)),
    psi = start_psi,
    control = seg.control(display = FALSE)
  )

  as.numeric(seg_model$psi[1, "Est."])
}

bootstrap_tau <- function(data, outcome, sex_label, predictor, n_resamples) {
  model_data <- data %>%
    dplyr::filter(Sex == sex_label) %>%
    dplyr::select(all_of(c(outcome, predictor))) %>%
    dplyr::filter(complete.cases(.))

  original_tau <- fit_tau(model_data, outcome, predictor)
  bootstrap_taus <- rep(NA_real_, n_resamples)

  for (i in seq_len(n_resamples)) {
    sample_rows <- sample.int(nrow(model_data), size = nrow(model_data), replace = TRUE)
    boot_data <- model_data[sample_rows, , drop = FALSE]
    bootstrap_taus[i] <- tryCatch(
      fit_tau(boot_data, outcome, predictor, start_psi = original_tau),
      error = function(e) NA_real_,
      warning = function(w) NA_real_
    )
  }

  successful_taus <- bootstrap_taus[!is.na(bootstrap_taus)]

  tibble(
    Outcome = outcome,
    Sex = sex_label,
    Predictor = predictor,
    Original_tau = original_tau,
    Bootstrap_mean_tau = mean(successful_taus),
    Bootstrap_median_tau = median(successful_taus),
    CI_2_5_percent = quantile(successful_taus, probs = 0.025, names = FALSE),
    CI_97_5_percent = quantile(successful_taus, probs = 0.975, names = FALSE),
    Successful_bootstrap_fits = length(successful_taus),
    Failed_bootstrap_fits = sum(is.na(bootstrap_taus))
  )
}

bootstrap_results <- bind_rows(
  lapply(
    seq_len(nrow(bootstrap_specs)),
    function(i) {
      bootstrap_tau(
        data = opl_plot,
        outcome = bootstrap_specs$Outcome[i],
        sex_label = bootstrap_specs$Sex[i],
        predictor = bootstrap_specs$Predictor[i],
        n_resamples = bootstrap_resamples
      )
    }
  )
) %>%
  dplyr::mutate(
    Original_tau = round(Original_tau, 2),
    Bootstrap_mean_tau = round(Bootstrap_mean_tau, 2),
    Bootstrap_median_tau = round(Bootstrap_median_tau, 2),
    CI_2_5_percent = round(CI_2_5_percent, 2),
    CI_97_5_percent = round(CI_97_5_percent, 2)
  )

write_csv(bootstrap_results, file.path(tables_dir, "table4_bootstrap_breakpoint_ci.csv"))
write_xlsx(bootstrap_results, file.path(tables_dir, "table4_bootstrap_breakpoint_ci.xlsx"))

figure5_data <- bootstrap_results %>%
  dplyr::mutate(
    Model = case_when(
      Sex == "Male" & Predictor == "Age" ~ "Male Age",
      Sex == "Female" & Predictor == "Age" ~ "Female Age",
      Sex == "Male" & Predictor == "BodyweightKg" ~ "Male Bodyweight",
      Sex == "Female" & Predictor == "BodyweightKg" ~ "Female Bodyweight",
      TRUE ~ paste(Sex, Predictor)
    ),
    Model = factor(
      Model,
      levels = rev(c("Male Age", "Female Age", "Male Bodyweight", "Female Bodyweight"))
    )
  )

figure5 <- ggplot(figure5_data, aes(x = Original_tau, y = Model)) +
  geom_errorbarh(
    aes(xmin = CI_2_5_percent, xmax = CI_97_5_percent),
    height = 0.17,
    color = "#333333",
    linewidth = 0.70
  ) +
  geom_point(color = "#2F5D8C", size = 2.25) +
  labs(x = "Breakpoint estimate", y = "Model") +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9.5, color = "black"),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(12, 24, 12, 12)
  )

ggsave(
  filename = file.path(figures_dir, "figure5_bootstrap_ci.png"),
  plot = figure5,
  width = 7.4,
  height = 4.9,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)


# 7. Software Session Information -----------------------------------------

sessionInfo()
