if (!exists("source_file_ext65")) source(file.path("v2", "scripts2", "00_setup.R"))
if (!exists("data_quality_results")) source(file.path(scripts_dir, "00_data_quality_check.R"))

raw_ext <- utils::read.csv(
  source_file_ext65,
  colClasses = "character",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
) %>%
  janitor::clean_names() %>%
  mutate(across(everything(), ~ str_squish(as.character(.x)))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ if_else(str_to_upper(.x) %in% c("NR", "NA"), NA_character_, .x)))

word_numbers <- c(
  zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5,
  six = 6, seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11,
  twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15,
  sixteen = 16, seventeen = 17, eighteen = 18, nineteen = 19,
  twenty = 20, thirty = 30, forty = 40, fifty = 50, sixty = 60,
  seventy = 70, eighty = 80, ninety = 90
)

parse_word_number <- function(x) {
  text <- str_to_lower(str_replace_all(x, "-", " "))
  text <- str_replace_all(text, "[^a-z ]", " ")
  parts <- str_split(str_squish(text), "\\s+")[[1]]
  if (length(parts) == 0 || all(is.na(parts))) return(NA_real_)
  vals <- word_numbers[parts]
  if (all(is.na(vals))) return(NA_real_)
  sum(vals, na.rm = TRUE)
}

parse_numeric <- function(x) {
  x <- as.character(x)
  out <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ".")))
  missing <- is.na(out) & !is.na(x)
  out[missing] <- vapply(x[missing], parse_word_number, numeric(1))
  out
}

parse_percent <- function(x) {
  value <- parse_numeric(x)
  ifelse(!is.na(value) & value > 1, value / 100, value)
}

parse_duration_days <- function(x) {
  lower <- str_to_lower(as.character(x))
  value <- parse_numeric(lower)
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

parse_frequency_week <- function(x) {
  lower <- str_to_lower(as.character(x))
  value <- parse_numeric(lower)
  case_when(
    is.na(lower) ~ NA_real_,
    str_detect(lower, "daily|every day") ~ 7,
    str_detect(lower, "per day|/day") ~ value * 7,
    str_detect(lower, "per week|/week|weekly|week") ~ value,
    str_detect(lower, "single|once|one session") ~ 1,
    !is.na(value) ~ value,
    TRUE ~ NA_real_
  )
}

normalize_bt <- function(category, technique = NA_character_) {
  lower <- str_to_lower(paste(category, technique))
  case_when(
    is.na(lower) ~ "other_unclear",
    str_detect(lower, "hypoventilation|low lung|rsh-vh|rs-vh") ~ "hypoventilation",
    str_detect(lower, "hook") ~ "hook_breathing",
    str_detect(lower, "wim hof|hyperventilation|voluntary hyperventilation|fast-paced|fast paced|evh|hypocapnia") ~ "hyperventilation",
    str_detect(lower, "box") ~ "paced_breathing",
    str_detect(lower, "slow-paced|slow paced|paced|coherent|regulated breathing|slow") ~ "paced_breathing",
    str_detect(lower, "breath holding|breath-holding|apnea|apnoea|valsalva") ~ "breath_holding",
    str_detect(lower, "pranayama|yogic|yoga|bhramari|bhastrika|nadi shodhana") ~ "pranayama_yogic",
    str_detect(lower, "diaphragmatic|abdominal|thoracic") ~ "diaphragmatic",
    str_detect(lower, "pattern observation") ~ "breathing_pattern_observation",
    TRUE ~ "other_unclear"
  )
}

normalize_study_design <- function(x) {
  lower <- str_to_lower(as.character(x))
  case_when(
    is.na(lower) ~ "unclear",
    str_detect(lower, "random|rct") & str_detect(lower, "crossover|cross-over") ~ "randomized_crossover",
    str_detect(lower, "crossover|cross-over|within-subject") ~ "crossover",
    str_detect(lower, "random|rct") & str_detect(lower, "control|trial|matched|experimental") ~ "parallel_or_controlled_trial",
    str_detect(lower, "non-randomized controlled|controlled trial") ~ "parallel_or_controlled_trial",
    str_detect(lower, "before-after|before after") ~ "before_after",
    str_detect(lower, "observational|cross-sectional|single group") ~ "single_group_or_observational",
    str_detect(lower, "retrospective") ~ "retrospective",
    str_detect(lower, "acute experimental") ~ "acute_experimental",
    TRUE ~ "other_or_unclear"
  )
}

ext_clean <- raw_ext %>%
  mutate(
    source_row = row_number(),
    paper_id_original = paper_id,
    publication_year_num = parse_numeric(year),
    n_total_num = parse_numeric(n_total),
    age_mean_num = parse_numeric(age_mean),
    age_sd_num = parse_numeric(age_sd),
    sex_ratio_prop = parse_percent(sex_male_pct),
    sex_male_n_num = parse_numeric(sex_male_n),
    sex_female_n_num = parse_numeric(sex_female_n),
    dose_frequency_week = parse_frequency_week(session_frequency),
    dose_duration_days_num = coalesce(parse_duration_days(total_duration), parse_duration_days(session_duration)),
    ig_n_num = parse_numeric(n_intervention),
    cg_n_num = parse_numeric(n_control),
    ig_mean_num = NA_real_,
    ig_sd_num = NA_real_,
    cg_mean_num = NA_real_,
    cg_sd_num = NA_real_,
    adherence_prop = parse_percent(adherence_value),
    dropout_prop = parse_percent(dropout_pct),
    bt_classification = coalesce(breathing_technique, breathing_category),
    bt_category = normalize_bt(breathing_category, breathing_technique),
    design_group = normalize_study_design(study_design),
    outcome_metric = primary_outcome_measure,
    effect_estimate = primary_effect_value,
    adverse_events = adverse_events_description,
    outcome_index = row_number()
  ) %>%
  mutate(
    male_n = coalesce(sex_male_n_num, n_total_num * sex_ratio_prop),
    female_n = coalesce(sex_female_n_num, n_total_num * (1 - sex_ratio_prop))
  )

tab1_clean <- ext_clean %>%
  distinct(paper_id, .keep_all = TRUE) %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    publication_year = publication_year_num,
    study_design,
    sport_discipline,
    athlete_level,
    view_link = NA_character_,
    design_group,
    journal,
    country
  )

tab2_clean <- ext_clean %>%
  distinct(paper_id, .keep_all = TRUE) %>%
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
    female_n,
    age_range,
    race_ethnicity_reported,
    baseline_anthropometrics,
    baseline_fitness
  )

tab3_clean <- ext_clean %>%
  distinct(paper_id, bt_classification, session_frequency, session_duration, total_duration, comparator_type, .keep_all = TRUE) %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    bt_classification,
    dose_frequency = session_frequency,
    dose_duration = total_duration,
    comparator_type,
    view_link = NA_character_,
    bt_category,
    dose_duration_days = dose_duration_days_num,
    dose_frequency_week,
    breathing_category,
    technique_description,
    device_free,
    biofeedback_used,
    comparator_description,
    intervention_timing,
    session_duration,
    supervision
  )

tab4_long <- ext_clean %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    outcome_metric,
    ig_n = n_intervention,
    ig_mean = NA_character_,
    ig_dispersion = NA_character_,
    view_link = NA_character_,
    outcome_index,
    ig_n_num,
    ig_mean_num,
    ig_sd_num,
    primary_outcome_domain,
    primary_result_direction,
    primary_effect_value,
    primary_uncertainty,
    secondary_outcome,
    efficacy_summary
  )

tab5_long <- ext_clean %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    cg_n = n_control,
    cg_mean = NA_character_,
    cg_dispersion = NA_character_,
    effect_estimate,
    view_link = NA_character_,
    outcome_index,
    cg_n_num,
    cg_mean_num,
    cg_sd_num,
    primary_uncertainty
  )

tab6_clean <- ext_clean %>%
  distinct(paper_id, .keep_all = TRUE) %>%
  transmute(
    document_id = NA_character_,
    document_name = title_short,
    paper_id,
    adverse_events,
    adherence_rate = adherence_value,
    dropout_rate = dropout_pct,
    feasibility_score = feasibility_summary,
    view_link = NA_character_,
    adherence_prop,
    dropout_prop,
    adherence_reported,
    dropout_reported,
    dropout_n,
    adverse_events_reported,
    safety_summary,
    main_limitations,
    risk_of_bias_concerns,
    extraction_notes
  )

cleaned_data <- list(
  source = ext_clean,
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
    transmute(table = "intervention_effects", paper_id, source_row = NA_integer_, issue = if_else(is.na(ig_mean_num) | is.na(ig_sd_num) | is.na(ig_n_num), "incomplete_intervention_group_fields_for_smd", NA_character_)),
  tab5_long %>%
    transmute(table = "control_effects", paper_id, source_row = NA_integer_, issue = if_else(is.na(cg_mean_num) | is.na(cg_sd_num) | is.na(cg_n_num), "incomplete_control_group_fields_for_smd", NA_character_))
) %>%
  filter(!is.na(issue))

readr::write_csv(cleaning_quality, file.path(tables_dir, "data_quality_log.csv"), na = "")
