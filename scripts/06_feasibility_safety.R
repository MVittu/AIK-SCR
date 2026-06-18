if (!exists("tab6_clean") || !exists("tab2_clean") || !exists("tab3_clean")) {
  source(file.path("scripts", "01_clean_data.R"))
}

feasibility <- tab6_clean %>%
  left_join(tab2_clean %>% select(paper_id, n_total_num), by = "paper_id") %>%
  left_join(
    tab3_clean %>%
      group_by(paper_id) %>%
      summarise(bt_category = first(na.omit(bt_category), default = NA_character_), .groups = "drop"),
    by = "paper_id"
  )

dropout_summary <- feasibility %>%
  filter(!is.na(dropout_prop), !is.na(n_total_num)) %>%
  summarise(
    studies_with_dropout = n(),
    participants_with_dropout = sum(n_total_num),
    pooled_dropout_rate = safe_weighted_mean(dropout_prop, n_total_num)
  )

event_dictionary <- tibble(
  event = c(
    "dizziness", "paresthesia", "tingling", "numbness", "syncope",
    "panic", "agitation", "pulmonary_edema", "heaviness",
    "lightheadedness", "deafness", "no_adverse_events"
  ),
  pattern = c(
    "dizziness", "paresthesia", "tingling", "numbness", "syncope",
    "panic", "agitation", "pulmonary edema", "heaviness",
    "lightheadedness", "deafness", "without adverse events|none of the subjects|no adverse"
  )
)

adverse_event_counts <- event_dictionary %>%
  mutate(data = map(pattern, function(pat) {
    feasibility %>%
      filter(!is.na(adverse_events), str_detect(str_to_lower(adverse_events), pat)) %>%
      count(bt_category, name = "study_mentions") %>%
      mutate(bt_category = replace_na(bt_category, "other_unclear"))
  })) %>%
  select(event, data) %>%
  unnest(data) %>%
  arrange(event, desc(study_mentions)) %>%
  group_by(event) %>%
  mutate(label = format_count_pct(study_mentions, total = sum(study_mentions))) %>%
  ungroup()

adherence_summary <- feasibility %>%
  summarise(
    studies_with_adherence = sum(!is.na(adherence_prop)),
    median_adherence = safe_median(adherence_prop),
    q1_adherence = safe_quantile(adherence_prop, 0.25),
    q3_adherence = safe_quantile(adherence_prop, 0.75)
  )

write_table_outputs(
  list(
    dropout_summary = dropout_summary,
    adherence_summary = adherence_summary,
    adverse_events_by_bt = adverse_event_counts,
    study_level_feasibility = feasibility
  ),
  "table6_feasibility_safety"
)

if (nrow(adverse_event_counts) > 0) {
  adverse_plot_data <- adverse_event_counts %>%
    group_by(event) %>%
    summarise(study_mentions = sum(study_mentions), .groups = "drop") %>%
    mutate(label = format_count_pct(study_mentions))

  adverse_plot <- adverse_plot_data %>%
    ggplot(aes(reorder(event, study_mentions), study_mentions)) +
    geom_col(fill = "#8856A7", show.legend = FALSE) +
    geom_text(aes(label = label), hjust = -0.05, size = 3.0) +
    coord_flip() +
    expand_limits(y = max(adverse_plot_data$study_mentions, na.rm = TRUE) * 1.35) +
    labs(x = NULL, y = "Study mentions")
  save_plot(adverse_plot, "adverse_events_by_bt.png")
}

dropout_plot_data <- feasibility %>%
  filter(!is.na(dropout_prop)) %>%
  mutate(dropout_percent = dropout_prop * 100)

if (nrow(dropout_plot_data) > 0) {
  dropout_plot <- ggplot(dropout_plot_data, aes(reorder(paper_id, dropout_percent), dropout_percent)) +
    geom_col(fill = "#FB6A4A") +
    geom_text(aes(label = paste0(sprintf("%.1f", dropout_percent), "%")), hjust = -0.05, size = 3.2) +
    coord_flip() +
    expand_limits(y = max(dropout_plot_data$dropout_percent, na.rm = TRUE) * 1.25) +
    labs(x = NULL, y = "Dropout rate (%)")
  save_plot(dropout_plot, "dropout_rates.png")
}
