if (!exists("tab3_clean")) source(file.path("scripts", "01_clean_data.R"))

bt_distribution <- tab3_clean %>%
  count(bt_category, sort = TRUE) %>%
  add_count_pct()

duration_summary <- tab3_clean %>%
  summarise(
    n_parseable = sum(!is.na(dose_duration_days)),
    median_days = safe_median(dose_duration_days),
    q1_days = safe_quantile(dose_duration_days, 0.25),
    q3_days = safe_quantile(dose_duration_days, 0.75),
    iqr_days = safe_iqr(dose_duration_days)
  )

frequency_summary <- tab3_clean %>%
  summarise(
    n_parseable = sum(!is.na(dose_frequency_week)),
    median_sessions_per_week = safe_median(dose_frequency_week),
    q1_sessions_per_week = safe_quantile(dose_frequency_week, 0.25),
    q3_sessions_per_week = safe_quantile(dose_frequency_week, 0.75),
    iqr_sessions_per_week = safe_iqr(dose_frequency_week)
  )

write_table_outputs(
  list(
    bt_distribution = bt_distribution,
    duration_summary = duration_summary,
    frequency_summary = frequency_summary,
    study_level_interventions = tab3_clean
  ),
  "table3_intervention_parameters"
)

bt_plot <- ggplot(bt_distribution, aes(reorder(bt_category, n), n)) +
  geom_col(fill = "#238B45") +
  geom_text(aes(label = label), hjust = -0.05, size = 3.2) +
  coord_flip() +
  expand_limits(y = max(bt_distribution$n, na.rm = TRUE) * 1.25) +
  labs(x = NULL, y = "Number of studies")
save_plot(bt_plot, "bt_category_distribution.png")

duration_plot <- tab3_clean %>%
  filter(!is.na(dose_duration_days)) %>%
  ggplot(aes(dose_duration_days)) +
  geom_histogram(binwidth = 7, fill = "#6BAED6", color = "white") +
  labs(x = "Intervention duration (days)", y = "Number of studies")
save_plot(duration_plot, "intervention_duration_distribution.png")
