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

fit_meta_model <- function(cluster_data, cluster_name) {
  meta::metagen(
    TE = hedges_g_normalized,
    seTE = sqrt(variance_normalized),
    studlab = paste(paper_id, outcome_metric, sep = ": "),
    data = cluster_data,
    sm = "SMD",
    common = FALSE,
    random = TRUE,
    method.tau = "REML",
    method.random.ci = "classic",
    prediction = TRUE,
    title = cluster_name
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
  model <- fit_meta_model(cluster_data, cluster_name)
  tibble(
    cluster = cluster_name,
    k = model$k,
    pooled_g = model$TE.random,
    ci_lower = model$lower.random,
    ci_upper = model$upper.random,
    p_value = model$pval.random,
    tau2 = model$tau2,
    q_statistic = model$Q,
    q_p_value = model$pval.Q,
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

  model <- fit_meta_model(cluster_data, cluster_name)
  grDevices::png(
    filename = file.path(plots_dir, filename),
    width = 2400,
    height = max(1700, 320 * nrow(cluster_data) + 900),
    res = 300
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  meta::forest(
    model,
    sortvar = model$TE,
    prediction = TRUE,
    print.I2 = TRUE,
    print.tau2 = TRUE,
    print.Q = TRUE,
    test.overall.random = TRUE,
    leftcols = c("studlab"),
    leftlabs = c("Study / outcome"),
    rightcols = c("effect", "ci", "w.random"),
    rightlabs = c("Hedges' g", "95% CI", "Weight"),
    smlab = "Hedges' g (positive favors intervention)",
    col.square = "#2C7FB8",
    col.diamond = "#D95F0E",
    col.diamond.lines = "#D95F0E",
    col.predict = "#31A354",
    pooled.events = FALSE
  )
}

make_forest_plot(smd, "hypoventilation_rsa", "forest_hypoventilation_rsa.png")
make_forest_plot(smd, "hyperventilation_power", "forest_hyperventilation_power.png")
make_forest_plot(smd, "paced_breathing_reaction", "forest_paced_breathing_reaction.png")
