if (!exists("scripts_dir")) source(file.path("v2", "scripts2", "00_setup.R"))

if (!file.exists(source_file_meta65)) {
  stop("Missing required meta-analysis file: ", source_file_meta65)
}

meta_raw <- utils::read.csv(
  source_file_meta65,
  colClasses = "character",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NR"),
  fileEncoding = "UTF-8-BOM"
) %>%
  janitor::clean_names()

num <- function(x) suppressWarnings(as.numeric(x))

lower_is_better_pattern <- paste(
  c(
    "time", "reaction", "movement", "rpe", "rating of perceived exertion",
    "fatigue", "anxiety", "dyspnea", "heart rate", "\\bhr\\b", "bpm",
    "lactate", "decrement", "rsasdec", "cp displacement", "time at spo2"
  ),
  collapse = "|"
)

meta_data <- meta_raw %>%
  mutate(
    row_id = row_number(),
    paper_id = str_remove(paper_id, "\\.pdf$"),
    year_num = num(year),
    ig_n_num = num(ig_n),
    ig_mean_num = num(ig_mean),
    ig_sd_num = num(ig_sd),
    cg_n_num = num(cg_n),
    cg_mean_num = num(cg_mean),
    cg_sd_num = num(cg_sd),
    n_allocated_ig_num = num(n_allocated_ig),
    n_allocated_cg_num = num(n_allocated_cg),
    dropout_n_ig_num = num(dropout_n_ig),
    dropout_n_cg_num = num(dropout_n_cg),
    any_ae_n_ig_num = num(any_ae_n_ig),
    any_ae_total_ig_num = num(any_ae_total_ig),
    any_ae_n_cg_num = num(any_ae_n_cg),
    any_ae_total_cg_num = num(any_ae_total_cg),
    overall_ae_n_num = num(overall_ae_n),
    overall_ae_total_num = num(overall_ae_total),
    outcome_lower = str_to_lower(outcome_metric),
    lower_by_metric = str_detect(outcome_lower, lower_is_better_pattern),
    lower_is_better_flag = case_when(
      str_to_upper(lower_is_better) %in% c("TRUE", "T", "YES", "1") ~ TRUE,
      lower_by_metric ~ TRUE,
      TRUE ~ FALSE
    ),
    direction_standardization_note = case_when(
      lower_is_better_flag ~ "Hedges' g multiplied by -1 so positive values favor the breathing intervention",
      TRUE ~ "Positive Hedges' g favors the breathing intervention"
    ),
    direction_multiplier = if_else(lower_is_better_flag, -1, 1),
    complete_for_smd = !is.na(ig_n_num) & !is.na(cg_n_num) &
      !is.na(ig_mean_num) & !is.na(cg_mean_num) &
      !is.na(ig_sd_num) & !is.na(cg_sd_num) &
      ig_n_num > 1 & cg_n_num > 1 & ig_sd_num > 0 & cg_sd_num > 0
  )

effect_input <- meta_data %>% filter(complete_for_smd)
effect_exclusions <- meta_data %>%
  filter(!complete_for_smd) %>%
  transmute(paper_id, outcome_domain, outcome_metric, reason = "missing_or_invalid_n_mean_or_sd")

smd <- if (nrow(effect_input) > 0) {
  metafor::escalc(
    measure = "SMD",
    m1i = ig_mean_num,
    sd1i = ig_sd_num,
    n1i = ig_n_num,
    m2i = cg_mean_num,
    sd2i = cg_sd_num,
    n2i = cg_n_num,
    data = effect_input,
    vtype = "UB"
  ) %>%
    as_tibble() %>%
    mutate(
      hedges_g_raw = yi,
      variance_raw = vi,
      hedges_g_normalized = yi * direction_multiplier,
      variance_normalized = vi,
      analysis_group = case_when(
        str_to_lower(outcome_domain) == "performance" ~ "efficacy_performance",
        str_to_lower(outcome_domain) == "physiological" ~ "efficacy_physiological",
        str_to_lower(outcome_domain) == "psychological" ~ "efficacy_psychological",
        TRUE ~ "efficacy_other"
      )
    )
} else {
  tibble()
}

fit_summary <- function(data, group_name, yi = "hedges_g_normalized", vi = "variance_normalized") {
  if (nrow(data) == 0 || !all(c("analysis_group", yi, vi) %in% names(data))) {
    return(tibble(group = group_name, k = 0, estimate = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, prediction_lower = NA_real_, prediction_upper = NA_real_, p_value = NA_real_, tau2 = NA_real_, q_statistic = NA_real_, q_p_value = NA_real_, i2 = NA_real_, status = "fewer_than_two_effects"))
  }
  group_data <- data %>% filter(analysis_group == group_name)
  if (nrow(group_data) < 2) {
    return(tibble(group = group_name, k = nrow(group_data), estimate = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, prediction_lower = NA_real_, prediction_upper = NA_real_, p_value = NA_real_, tau2 = NA_real_, q_statistic = NA_real_, q_p_value = NA_real_, i2 = NA_real_, status = "fewer_than_two_effects"))
  }
  model <- metafor::rma(yi = group_data[[yi]], vi = group_data[[vi]], method = "REML")
  pred <- predict(model)
  tibble(group = group_name, k = model$k, estimate = as.numeric(model$b), ci_lower = model$ci.lb, ci_upper = model$ci.ub, prediction_lower = pred$pi.lb, prediction_upper = pred$pi.ub, p_value = model$pval, tau2 = model$tau2, q_statistic = model$QE, q_p_value = model$QEp, i2 = model$I2, status = "model_fit")
}

plot_forest <- function(data, group_name, filename, xlab = "Hedges' g") {
  group_data <- data %>% filter(analysis_group == group_name)
  if (nrow(group_data) < 2) return(invisible(NULL))
  initial_model <- metafor::rma(yi = hedges_g_normalized, vi = variance_normalized, data = group_data, method = "REML")
  group_data <- group_data %>%
    mutate(weights_pct = 100 * as.numeric(weights(initial_model)) / sum(weights(initial_model))) %>%
    arrange(desc(weights_pct))
  model <- metafor::rma(yi = hedges_g_normalized, vi = variance_normalized, data = group_data, method = "REML")
  labels <- paste(group_data$paper_id, group_data$outcome_metric, sep = ": ")
  weights_pct <- 100 * as.numeric(weights(model)) / sum(weights(model))
  grDevices::png(file.path(plots_dir, filename), width = 4400, height = max(2200, 380 * nrow(group_data) + 1100), res = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(9, 10, 4, 8))
  weight_x <- -5.2
  metafor::forest(model, slab = labels, ilab = sprintf("%.1f%%", weights_pct), ilab.lab = "Weight", ilab.xpos = weight_x, header = "Study / outcome", xlab = xlab, mlab = expression(bold("Random-effects model")), refline = 0, shade = TRUE, addpred = TRUE, cex = 0.95, xlim = c(-16, 8), alim = c(-2, 2), textpos = c(-16, 8), slab.just = "left")
  text(weight_x, -1, "100%", cex = 1.0, font = 2)
  mtext(sprintf("Heterogeneity: Q = %.2f, df = %d, p = %.3f; I^2 = %.1f%%; tau^2 = %.3f", model$QE, model$k - model$p, model$QEp, model$I2, model$tau2), side = 1, line = 5.8, adj = 0, cex = 1.0)
}

plot_rr_forest <- function(data, filename, xlab = "Risk ratio", max_display_rr_upper = 100) {
  if (nrow(data) < 2) return(invisible(NULL))
  display_check <- data %>%
    mutate(
      rr_ci_lower = exp(yi - 1.96 * sqrt(vi)),
      rr_ci_upper = exp(yi + 1.96 * sqrt(vi)),
      display_exclusion_reason = case_when(
        !is.na(rr_ci_upper) & rr_ci_upper > max_display_rr_upper ~ paste0("not_displayed_extreme_upper_ci_gt_", max_display_rr_upper),
        TRUE ~ NA_character_
      )
    )
  excluded_display <- display_check %>%
    filter(!is.na(display_exclusion_reason)) %>%
    select(paper_id, yi, vi, rr_ci_lower, rr_ci_upper, display_exclusion_reason)
  if (nrow(excluded_display) > 0) {
    readr::write_csv(
      excluded_display,
      file.path(tables_dir, paste0(tools::file_path_sans_ext(filename), "__display_excluded.csv")),
      na = ""
    )
  }
  data <- display_check %>% filter(is.na(display_exclusion_reason))
  if (nrow(data) < 2) return(invisible(NULL))

  initial_model <- metafor::rma(yi = data$yi, vi = data$vi, method = "REML")
  data <- data %>%
    mutate(weights_pct = 100 * as.numeric(weights(initial_model)) / sum(weights(initial_model))) %>%
    arrange(desc(weights_pct))
  model <- metafor::rma(yi = data$yi, vi = data$vi, method = "REML")
  labels <- data$paper_id
  weights_pct <- 100 * as.numeric(weights(model)) / sum(weights(model))
  grDevices::png(file.path(plots_dir, filename), width = 4200, height = max(2200, 360 * nrow(data) + 1050), res = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(9, 10, 4, 8))
  weight_x <- -5.2
  metafor::forest(model, slab = labels, atransf = exp, ilab = sprintf("%.1f%%", weights_pct), ilab.lab = "Weight", ilab.xpos = weight_x, header = "Study", xlab = xlab, mlab = expression(bold("Random-effects model")), refline = 0, shade = TRUE, addpred = TRUE, cex = 0.95, xlim = c(-16, 8), textpos = c(-16, 8), slab.just = "left")
  text(weight_x, -1, "100%", cex = 1.0, font = 2)
  mtext(sprintf("Heterogeneity: Q = %.2f, df = %d, p = %.3f; I^2 = %.1f%%; tau^2 = %.3f", model$QE, model$k - model$p, model$QEp, model$I2, model$tau2), side = 1, line = 5.8, adj = 0, cex = 1.0)
}

efficacy_groups <- c("efficacy_performance", "efficacy_physiological", "efficacy_psychological")
efficacy_summary <- purrr::map_dfr(efficacy_groups, ~ fit_summary(smd, .x))

ae_comparative <- meta_data %>%
  filter(!is.na(any_ae_n_ig_num), !is.na(any_ae_n_cg_num), !is.na(any_ae_total_ig_num), !is.na(any_ae_total_cg_num), any_ae_total_ig_num > 0, any_ae_total_cg_num > 0) %>%
  transmute(paper_id, bt_classification, ai = any_ae_n_ig_num, bi = any_ae_total_ig_num - any_ae_n_ig_num, ci = any_ae_n_cg_num, di = any_ae_total_cg_num - any_ae_n_cg_num)

ae_rr <- if (nrow(ae_comparative) > 0) {
  metafor::escalc(measure = "RR", ai = ai, bi = bi, ci = ci, di = di, add = 0.5, to = "all", data = ae_comparative) %>%
    as_tibble() %>%
    mutate(analysis_group = "safety_adverse_events_rr")
} else {
  tibble()
}

dropout_comparative <- meta_data %>%
  filter(!is.na(dropout_n_ig_num), !is.na(dropout_n_cg_num), !is.na(n_allocated_ig_num), !is.na(n_allocated_cg_num), n_allocated_ig_num > 0, n_allocated_cg_num > 0) %>%
  transmute(paper_id, ai = dropout_n_ig_num, bi = n_allocated_ig_num - dropout_n_ig_num, ci = dropout_n_cg_num, di = n_allocated_cg_num - dropout_n_cg_num)

dropout_rr <- if (nrow(dropout_comparative) > 0) {
  metafor::escalc(measure = "RR", ai = ai, bi = bi, ci = ci, di = di, add = 0.5, to = "all", data = dropout_comparative) %>%
    as_tibble() %>%
    mutate(analysis_group = "feasibility_dropout_rr")
} else {
  tibble()
}

rr_summary <- bind_rows(
  fit_summary(if (nrow(ae_rr) > 0) ae_rr %>% rename(estimate_input = yi, variance_input = vi) else ae_rr, "safety_adverse_events_rr", yi = "estimate_input", vi = "variance_input"),
  fit_summary(if (nrow(dropout_rr) > 0) dropout_rr %>% rename(estimate_input = yi, variance_input = vi) else dropout_rr, "feasibility_dropout_rr", yi = "estimate_input", vi = "variance_input")
) %>%
  mutate(measure = "log risk ratio", rr = exp(estimate), rr_ci_lower = exp(ci_lower), rr_ci_upper = exp(ci_upper))

write_table_outputs(
  list(
    extracted_meta_data = meta_data,
    efficacy_effect_sizes = smd,
    efficacy_meta_summary = efficacy_summary,
    efficacy_excluded = effect_exclusions,
    safety_adverse_event_rr = ae_rr,
    feasibility_dropout_rr = dropout_rr,
    safety_feasibility_summary = rr_summary
  ),
  "meta65_analysis"
)

plot_forest(smd, "efficacy_performance", "meta65_forest_efficacy_performance.png")
plot_forest(smd, "efficacy_physiological", "meta65_forest_efficacy_physiological.png")
plot_forest(smd, "efficacy_psychological", "meta65_forest_efficacy_psychological.png")
plot_rr_forest(ae_rr, "meta65_forest_safety_adverse_events_rr.png")
plot_rr_forest(dropout_rr, "meta65_forest_feasibility_dropout_rr.png")
