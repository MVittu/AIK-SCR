source(file.path("scripts", "00_setup.R"))

source_file <- file.path(data_dir, "Data-ext_by_paper_id.csv")

if (!file.exists(source_file)) {
  stop("Missing required data file: ", source_file)
}

raw_ext <- utils::read.csv(
  source_file,
  colClasses = "character",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "--/--"),
  fileEncoding = "UTF-8-BOM"
) %>%
  janitor::clean_names() %>%
  mutate(across(everything(), ~ str_squish(as.character(.x)))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "--/--")))

required_columns <- c(
  "paper_id", "publication_year", "study_design", "sport_discipline",
  "athlete_level", "n_total", "age_mean", "age_sd", "sex_ratio_male_pct",
  "bt_classification", "dose_frequency", "dose_duration_days",
  "comparator_type", "outcome_metric", "ig_n", "ig_mean", "ig_dispersion",
  "cg_n", "cg_mean", "cg_dispersion", "effect_estimate", "adverse_events",
  "adherence_rate_pct", "dropout_rate_pct", "feasibility_score"
)

missing_columns <- setdiff(required_columns, names(raw_ext))
if (length(missing_columns) > 0) {
  stop("Data-ext_by_paper_id.csv is missing required columns: ", paste(missing_columns, collapse = ", "))
}

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

classify_bt <- function(x) {
  lower <- str_to_lower(as.character(x))
  case_when(
    is.na(lower) ~ "other_unclear",
    str_detect(lower, "hypoventilation|low lung|rsh-vh|rs-vh") ~ "hypoventilation",
    str_detect(lower, "hyperventilation|wim hof|evh|hypocapnia|fast-paced") ~ "hyperventilation",
    str_detect(lower, "paced|coherent|6 bpm|box breathing|regulated breathing|slow") ~ "paced_breathing",
    str_detect(lower, "breath-hold|breath holding|breath-holding|apnea|valsalva") ~ "breath_holding",
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
    str_detect(lower, "random|rct") & str_detect(lower, "crossover|cross-over") ~ "randomized_crossover",
    str_detect(lower, "crossover|cross-over|within-subject") ~ "crossover",
    str_detect(lower, "random|rct") & str_detect(lower, "control|trial|matched|experimental") ~ "parallel_or_controlled_trial",
    str_detect(lower, "observational|cross-sectional|single group|pre-experimental") ~ "single_group_or_observational",
    str_detect(lower, "retrospective") ~ "retrospective",
    str_detect(lower, "repeated") ~ "repeated_measures",
    TRUE ~ "other_or_unclear"
  )
}

ext_clean <- raw_ext %>%
  mutate(
    publication_year_num = parse_numeric(publication_year),
    n_total_num = parse_numeric(n_total),
    age_mean_num = parse_numeric(age_mean),
    age_sd_num = parse_numeric(age_sd),
    sex_ratio_prop = parse_percent(sex_ratio_male_pct),
    dose_frequency_week = parse_numeric(dose_frequency),
    dose_duration_days_num = parse_numeric(dose_duration_days),
    ig_n_num = parse_numeric(ig_n),
    ig_mean_num = parse_numeric(ig_mean),
    ig_sd_num = parse_numeric(ig_dispersion),
    cg_n_num = parse_numeric(cg_n),
    cg_mean_num = parse_numeric(cg_mean),
    cg_sd_num = parse_numeric(cg_dispersion),
    adherence_prop = parse_percent(adherence_rate_pct),
    dropout_prop = parse_percent(dropout_rate_pct),
    bt_category = classify_bt(bt_classification),
    design_group = normalize_study_design(study_design),
    outcome_index = row_number()
  )

tab1_clean <- ext_clean %>%
  distinct(paper_id, .keep_all = TRUE) %>%
  transmute(
    document_id = NA_character_,
    document_name = NA_character_,
    paper_id,
    publication_year = publication_year_num,
    study_design,
    sport_discipline,
    athlete_level,
    view_link = NA_character_,
    design_group
  )

tab2_clean <- ext_clean %>%
  distinct(paper_id, .keep_all = TRUE) %>%
  transmute(
    document_id = NA_character_,
    document_name = NA_character_,
    paper_id,
    n_total,
    age_mean,
    age_sd,
    sex_ratio = sex_ratio_male_pct,
    view_link = NA_character_,
    n_total_num,
    age_mean_num,
    age_sd_num,
    sex_ratio_prop,
    male_n = n_total_num * sex_ratio_prop,
    female_n = n_total_num * (1 - sex_ratio_prop)
  )

tab3_clean <- ext_clean %>%
  distinct(paper_id, bt_classification, dose_frequency, dose_duration_days, comparator_type, .keep_all = TRUE) %>%
  transmute(
    document_id = NA_character_,
    document_name = NA_character_,
    paper_id,
    bt_classification,
    dose_frequency,
    dose_duration = dose_duration_days,
    comparator_type,
    view_link = NA_character_,
    bt_category,
    dose_duration_days = dose_duration_days_num,
    dose_frequency_week
  )

tab4_long <- ext_clean %>%
  transmute(
    document_id = NA_character_,
    document_name = NA_character_,
    paper_id,
    outcome_metric,
    ig_n,
    ig_mean,
    ig_dispersion,
    view_link = NA_character_,
    outcome_index,
    ig_n_num,
    ig_mean_num,
    ig_sd_num
  )

tab5_long <- ext_clean %>%
  transmute(
    document_id = NA_character_,
    document_name = NA_character_,
    paper_id,
    cg_n,
    cg_mean,
    cg_dispersion,
    effect_estimate,
    view_link = NA_character_,
    outcome_index,
    cg_n_num,
    cg_mean_num,
    cg_sd_num
  )

tab6_clean <- ext_clean %>%
  distinct(paper_id, .keep_all = TRUE) %>%
  transmute(
    document_id = NA_character_,
    document_name = NA_character_,
    paper_id,
    adverse_events,
    adherence_rate = adherence_rate_pct,
    dropout_rate = dropout_rate_pct,
    feasibility_score,
    view_link = NA_character_,
    adherence_prop,
    dropout_prop
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

saveRDS(cleaned_data, file.path(outputs_dir, "cleaned_data.rds"))

data_quality <- bind_rows(
  ext_clean %>%
    distinct(paper_id, .keep_all = TRUE) %>%
    transmute(table = "source_study_level", paper_id, issue = if_else(is.na(publication_year_num), "missing_publication_year", NA_character_)),
  tab2_clean %>%
    transmute(table = "demographics", paper_id, issue = if_else(is.na(n_total_num), "missing_or_unparseable_n_total", NA_character_)),
  tab3_clean %>%
    transmute(table = "interventions", paper_id, issue = if_else(bt_category == "other_unclear", "other_or_unclear_bt_category", NA_character_)),
  tab4_long %>%
    transmute(table = "intervention_effects", paper_id, issue = if_else(is.na(ig_mean_num) | is.na(ig_sd_num) | is.na(ig_n_num), "incomplete_intervention_effect_fields", NA_character_)),
  tab5_long %>%
    transmute(table = "control_effects", paper_id, issue = if_else(is.na(cg_mean_num) | is.na(cg_sd_num) | is.na(cg_n_num), "incomplete_control_effect_fields", NA_character_))
) %>%
  filter(!is.na(issue))

readr::write_csv(data_quality, file.path(tables_dir, "data_quality_log.csv"))
