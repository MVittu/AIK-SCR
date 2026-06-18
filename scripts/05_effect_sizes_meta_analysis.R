if (!exists("tab4_long") || !exists("tab5_long") || !exists("tab3_clean")) {
  source(file.path("scripts", "01_clean_data.R"))
}

lower_is_better_pattern <- paste(
  c(
    "time", "reaction", "movement", "rpe", "rating of perceived exertion",
    "fatigue", "anxiety", "dyspnea", "blood lactate", "lactate",
    "heart rate", "return time", "coefficient of variation"
  ),
  collapse = "|"
)

power_pattern <- paste(c("power", "work", "volume", "strength", "1 repetition maximum", "torque"), collapse = "|")
reaction_pattern <- paste(c("reaction time", "movement time", "art", "vrt"), collapse = "|")
rsa_pattern <- paste(c("repeated-sprint", "rsa", "sprint"), collapse = "|")

effect_data <- tab4_long %>%
  select(
    paper_id, document_name, outcome_index, outcome_metric,
    ig_n_num, ig_mean_num, ig_sd_num
  ) %>%
  full_join(
    tab5_long %>%
      select(paper_id, outcome_index, cg_n_num, cg_mean_num, cg_sd_num, effect_estimate),
    by = c("paper_id", "outcome_index")
  ) %>%
  left_join(
    tab3_clean %>%
      group_by(paper_id) %>%
      summarise(
        bt_classification = paste(unique(na.omit(bt_classification)), collapse = "; "),
        bt_category = first(na.omit(bt_category), default = NA_character_),
        .groups = "drop"
      ),
    by = "paper_id"
  ) %>%
  mutate(
    outcome_metric = str_squish(outcome_metric),
    outcome_lower = str_to_lower(outcome_metric),
    direction_multiplier = if_else(
      replace_na(str_detect(outcome_lower, lower_is_better_pattern), FALSE),
      -1,
      1
    ),
    analysis_cluster = case_when(
      bt_category == "hypoventilation" & str_detect(outcome_lower, rsa_pattern) ~ "hypoventilation_rsa",
      bt_category == "hyperventilation" & str_detect(outcome_lower, power_pattern) ~ "hyperventilation_power",
      bt_category == "paced_breathing" & str_detect(outcome_lower, reaction_pattern) ~ "paced_breathing_reaction",
      TRUE ~ NA_character_
    ),
    complete_for_smd = !is.na(ig_n_num) & !is.na(cg_n_num) &
      !is.na(ig_mean_num) & !is.na(cg_mean_num) &
      !is.na(ig_sd_num) & !is.na(cg_sd_num) &
      ig_n_num > 1 & cg_n_num > 1 & ig_sd_num > 0 & cg_sd_num > 0
  )

effect_exclusions <- effect_data %>%
  filter(!complete_for_smd) %>%
  transmute(
    paper_id, outcome_index, outcome_metric,
    reason = "missing_or_invalid_group_n_mean_or_sd"
  )

effect_input <- effect_data %>% filter(complete_for_smd)

if (nrow(effect_input) > 0) {
  smd <- metafor::escalc(
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
      dispersion_assumption = "IG_Dispersion and CG_Dispersion treated as SD"
    )
} else {
  smd <- effect_input %>%
    mutate(
      yi = numeric(),
      vi = numeric(),
      hedges_g_raw = numeric(),
      variance_raw = numeric(),
      hedges_g_normalized = numeric(),
      variance_normalized = numeric(),
      dispersion_assumption = character()
    )
}

run_meta <- function(data, cluster_name) {
  cluster_data <- data %>% filter(analysis_cluster == cluster_name)
  if (nrow(cluster_data) < 2) {
    return(tibble(
      cluster = cluster_name,
      k = nrow(cluster_data),
      pooled_g = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = NA_real_,
      tau2 = NA_real_,
      q_statistic = NA_real_,
      q_p_value = NA_real_,
      i2 = NA_real_,
      status = "fewer_than_two_effects"
    ))
  }
  model <- metafor::rma(
    yi = hedges_g_normalized,
    vi = variance_normalized,
    data = cluster_data,
    method = "REML"
  )
  pred <- predict(model)
  tibble(
    cluster = cluster_name,
    k = model$k,
    pooled_g = as.numeric(model$b),
    ci_lower = model$ci.lb,
    ci_upper = model$ci.ub,
    prediction_lower = pred$pi.lb,
    prediction_upper = pred$pi.ub,
    p_value = model$pval,
    tau2 = model$tau2,
    q_statistic = model$QE,
    q_p_value = model$QEp,
    i2 = model$I2,
    status = "model_fit"
  )
}

clusters <- c("hypoventilation_rsa", "hyperventilation_power", "paced_breathing_reaction")
meta_summary <- purrr::map_dfr(clusters, ~ run_meta(smd, .x))

write_table_outputs(
  list(
    effect_sizes = smd,
    subgroup_meta_analysis = meta_summary,
    excluded_from_smd = effect_exclusions
  ),
  "table4_5_effect_sizes"
)

write_table_outputs(
  list(subgroup_meta_analysis = meta_summary),
  "meta_analysis_subgroups"
)

make_forest_plot <- function(data, cluster_name, filename) {
  cluster_data <- data %>% filter(analysis_cluster == cluster_name)
  if (nrow(cluster_data) < 2) return(invisible(NULL))

  model <- metafor::rma(
    yi = hedges_g_normalized,
    vi = variance_normalized,
    data = cluster_data,
    method = "REML"
  )
  labels <- paste(cluster_data$paper_id, cluster_data$outcome_metric, sep = ": ")
  weight_pct <- 100 * weights(model) / sum(weights(model))

  grDevices::png(
    filename = file.path(plots_dir, filename),
    width = 2400,
    height = max(1600, 260 * nrow(cluster_data) + 900),
    res = 300
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  par(mar = c(9, 12, 4, 6))
  weight_x <- -2.3
  total_row <- -1

  metafor::forest(
    model,
    slab = labels,
    ilab = sprintf("%.1f%%", weight_pct),
    ilab.lab = "Weight",
    ilab.xpos = weight_x,
    header = "Study / outcome",
    xlab = "Hedges' g",
    mlab = expression(bold("Random-effects model")),
    refline = 0,
    shade = TRUE,
    addpred = TRUE,
    cex = 0.9,
    xlim = c(-6.5, 4),
    alim = c(-1.5, 1.5),
    textpos = c(-6.5, 4),
    slab.just = "left"
  )
  text(weight_x, total_row, "100%", cex = 0.9, font = 2)
  mtext(
    sprintf(
      "Heterogeneity: Q = %.2f, df = %d, p = %.3f; I^2 = %.1f%%; tau^2 = %.3f",
      model$QE, model$k - model$p, model$QEp, model$I2, model$tau2
    ),
    side = 1,
    line = 5.8,
    adj = 0,
    cex = 0.9
  )
}

make_forest_plot(smd, "hypoventilation_rsa", "forest_hypoventilation_rsa.png")
make_forest_plot(smd, "hyperventilation_power", "forest_hyperventilation_power.png")
make_forest_plot(smd, "paced_breathing_reaction", "forest_paced_breathing_reaction.png")
