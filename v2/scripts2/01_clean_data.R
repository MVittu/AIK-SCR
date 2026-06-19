if (!exists("source_file_complete")) source(file.path("v2", "scripts2", "00_setup.R"))

if (!file.exists(source_file_complete)) {
  stop("Missing required complete v2 data file: ", source_file_complete)
}

raw_complete <- utils::read.csv(
  source_file_complete,
  colClasses = "character",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM",
  na.strings = c("", "NA", "NR")
) %>%
  janitor::clean_names() %>%
  mutate(across(everything(), ~ str_squish(as.character(.x)))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ if_else(str_to_upper(.x) %in% c("NR", "NA"), NA_character_, .x)))

intervention_raw <- if (file.exists(source_file_intervention65)) {
  utils::read.csv(
    source_file_intervention65,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM",
    na.strings = c("", "NA", "NR")
  ) %>%
    janitor::clean_names() %>%
    mutate(across(everything(), ~ str_squish(as.character(.x)))) %>%
    mutate(across(everything(), ~ na_if(.x, ""))) %>%
    mutate(paper_id = str_remove(paper_id, "\\.pdf$"))
} else {
  tibble(paper_id = character())
}

num <- function(x) suppressWarnings(as.numeric(x))

parse_percent_v2 <- function(x) {
  value <- num(x)
  ifelse(!is.na(value) & value > 1, value / 100, value)
}

parse_duration_days <- function(x) {
  lower <- str_to_lower(as.character(x))
  value <- suppressWarnings(readr::parse_number(lower))
  case_when(
    is.na(lower) ~ NA_real_,
    str_detect(lower, "acute|single|one session|baseline|testing") ~ 1,
    str_detect(lower, "minute|min") ~ value / 1440,
    str_detect(lower, "hour|hr") ~ value / 24,
    str_detect(lower, "week|wk") ~ value * 7,
    str_detect(lower, "month") ~ value * 30,
    str_detect(lower, "year") ~ value * 365,
    str_detect(lower, "day") ~ value,
    !is.na(value) ~ value,
    TRUE ~ NA_real_
  )
}

normalize_bt <- function(category, technique = NA_character_) {
  lower <- str_to_lower(paste(category, technique))
  case_when(
    is.na(lower) ~ "other_unclear",
    str_detect(lower, "hypoventilation|low lung|rsh-vh|rs-vh") ~ "hypoventilation",
    str_detect(lower, "wim hof|hyperventilation|voluntary hyperventilation|fast-paced|fast paced|evh|hypocapnia") ~ "hyperventilation",
    str_detect(lower, "box|slow-paced|slow paced|paced|coherent|regulated breathing|slow") ~ "paced_breathing",
    str_detect(lower, "breath holding|breath-holding|apnea|apnoea|valsalva") ~ "breath_holding",
    str_detect(lower, "pranayama|yogic|yoga|bhramari|bhastrika|nadi shodhana") ~ "pranayama_yogic",
    str_detect(lower, "diaphragmatic|abdominal|thoracic") ~ "diaphragmatic",
    str_detect(lower, "hook") ~ "hook_breathing",
    TRUE ~ "other_unclear"
  )
}

normalize_study_design <- function(x) {
  lower <- str_to_lower(as.character(x))
  case_when(
    is.na(lower) ~ "unclear",
    str_detect(lower, "observational|cross-sectional|retrospective") ~ "single_group_or_observational",
    str_detect(lower, "random|rct") & str_detect(lower, "crossover|cross-over") ~ "randomized_crossover",
    str_detect(lower, "crossover|cross-over|within-subject") ~ "crossover",
    str_detect(lower, "random|rct|controlled|parallel|trial") ~ "parallel_or_controlled_trial",
    str_detect(lower, "before-after|before after") ~ "before_after",
    str_detect(lower, "experimental") ~ "parallel_or_controlled_trial",
    TRUE ~ "other_or_unclear"
  )
}

ext_clean <- raw_complete %>%
  mutate(
    source_row = row_number(),
    paper_id = str_remove(paper_id, "\\.pdf$")
  ) %>%
  left_join(
    intervention_raw %>%
      select(
        paper_id,
        intervention_bt_classification = bt_classification,
        intervention_breathing_category = breathing_category,
        intervention_comparator = comparator,
        intervention_arm_structure = arm_structure,
        intervention_timing = timing,
        intervention_total_duration = total_duration
      ),
    by = "paper_id"
  ) %>%
  mutate(
    publication_year_num = num(year),
    n_total_num = num(n_total),
    age_mean_num = num(age_mean),
    age_sd_num = num(age_sd),
    sex_ratio_prop = parse_percent_v2(sex_male_pct),
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
    adherence_ig_prop = parse_percent_v2(adherence_rate_ig),
    adherence_cg_prop = parse_percent_v2(adherence_rate_cg),
    bt_classification = coalesce(bt_classification, intervention_bt_classification),
    comparator = coalesce(comparator, intervention_comparator),
    arm_structure = coalesce(arm_structure, intervention_arm_structure),
    bt_category = normalize_bt(coalesce(intervention_breathing_category, bt_classification)),
    design_group = normalize_study_design(design),
    dose_duration_days_num = parse_duration_days(intervention_total_duration),
    male_n = n_total_num * sex_ratio_prop,
    female_n = n_total_num * (1 - sex_ratio_prop),
    outcome_index = row_number()
  )

study_level <- ext_clean %>%
  arrange(publication_year_num, paper_id, row_type) %>%
  distinct(paper_id, .keep_all = TRUE)

tab1_clean <- study_level %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    publication_year = publication_year_num,
    study_design = design,
    sport_discipline = sport_category,
    athlete_level,
    view_link = NA_character_,
    design_group,
    journal,
    country
  )

tab2_clean <- study_level %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    n_total,
    age_mean,
    age_sd,
    sex_ratio = sex_male_pct,
    view_link = NA_character_,
    n_total_num,
    age_mean_num,
    age_sd_num,
    sex_ratio_prop,
    male_n,
    female_n
  )

tab3_clean <- study_level %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    bt_classification,
    dose_frequency = NA_character_,
    dose_duration = intervention_total_duration,
    comparator_type = comparator,
    view_link = NA_character_,
    bt_category,
    dose_duration_days = dose_duration_days_num,
    dose_frequency_week = NA_real_,
    breathing_category = bt_category,
    comparator_description = comparator,
    arm_structure,
    timing = intervention_timing
  )

tab4_long <- ext_clean %>%
  filter(row_type == "efficacy_outcome") %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    outcome_metric,
    ig_n,
    ig_mean,
    ig_dispersion = ig_sd,
    view_link = NA_character_,
    outcome_index,
    ig_n_num,
    ig_mean_num,
    ig_sd_num,
    outcome_domain,
    timepoint,
    lower_is_better,
    dispersion_note,
    efficacy_source,
    complete_for_smd
  )

tab5_long <- ext_clean %>%
  filter(row_type == "efficacy_outcome") %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    cg_n,
    cg_mean,
    cg_dispersion = cg_sd,
    effect_estimate = NA_character_,
    view_link = NA_character_,
    outcome_index,
    cg_n_num,
    cg_mean_num,
    cg_sd_num,
    outcome_domain,
    outcome_metric
  )

tab6_clean <- study_level %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    adverse_events = ae_description,
    adherence_rate = adherence_rate_ig,
    dropout_n_total = coalesce(dropout_n_ig_num, 0) + coalesce(dropout_n_cg_num, 0),
    allocated_n_total = coalesce(n_allocated_ig_num, 0) + coalesce(n_allocated_cg_num, 0),
    dropout_rate = if_else(
      allocated_n_total > 0,
      as.character(dropout_n_total / allocated_n_total),
      NA_character_
    ),
    feasibility_score = completion_note,
    view_link = NA_character_,
    adherence_prop = adherence_ig_prop,
    dropout_prop = num(dropout_rate),
    dropout_n_ig_num,
    dropout_n_cg_num,
    n_allocated_ig_num,
    n_allocated_cg_num,
    any_ae_reported,
    overall_ae_n,
    overall_ae_total,
    serious_ae_n,
    safety_summary = safety_source_quote,
    feasibility_source_quote
  )

cleaned_data <- list(
  source = ext_clean,
  study_level = study_level,
  tab1 = tab1_clean,
  tab2 = tab2_clean,
  tab3 = tab3_clean,
  tab4 = tab4_long,
  tab5 = tab5_long,
  tab6 = tab6_clean
)

write_table_outputs(cleaned_data, "cleaned_data")

cleaning_quality <- bind_rows(
  tab2_clean %>%
    transmute(table = "demographics", paper_id, source_row = NA_integer_, issue = if_else(is.na(n_total_num), "missing_or_unparseable_n_total", NA_character_)),
  tab3_clean %>%
    transmute(table = "interventions", paper_id, source_row = NA_integer_, issue = if_else(bt_category == "other_unclear", "other_or_unclear_bt_category", NA_character_)),
  tab4_long %>%
    transmute(table = "intervention_effects", paper_id, source_row = outcome_index, issue = if_else(is.na(ig_mean_num) | is.na(ig_sd_num) | is.na(ig_n_num), "incomplete_intervention_group_fields_for_smd", NA_character_)),
  tab5_long %>%
    transmute(table = "control_effects", paper_id, source_row = outcome_index, issue = if_else(is.na(cg_mean_num) | is.na(cg_sd_num) | is.na(cg_n_num), "incomplete_control_group_fields_for_smd", NA_character_))
) %>%
  filter(!is.na(issue))

readr::write_csv(cleaning_quality, file.path(tables_dir, "data_quality_log.csv"), na = "")
