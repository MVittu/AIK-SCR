source(file.path("scripts", "00_setup.R"))

expected_files <- c(
  tab1 = "Tab1-ScR-Bibliography.csv",
  tab2 = "Tab2-ScR-Descriptive.csv",
  tab3 = "Tab3-ScR-Specifics.csv",
  tab4 = "Tab4-ScR-Intervention.csv",
  tab5 = "tab5-ScR-Control.csv",
  tab6 = "Tab6-ScR-Feasibility.csv"
)

missing_files <- expected_files[!file.exists(file.path(data_dir, expected_files))]
if (length(missing_files) > 0) {
  stop("Missing required data files: ", paste(missing_files, collapse = ", "))
}

read_table <- function(file_name, names_out) {
  raw <- readr::read_csv(
    file.path(data_dir, file_name),
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
  if (ncol(raw) != length(names_out)) {
    stop(file_name, " has ", ncol(raw), " columns; expected ", length(names_out), ".")
  }
  names(raw) <- names_out
  raw %>%
    mutate(across(everything(), ~ str_squish(as.character(.x)))) %>%
    mutate(across(everything(), ~ na_if(.x, ""))) %>%
    mutate(across(everything(), ~ na_if(.x, "--/--")))
}

tab1 <- read_table(expected_files[["tab1"]], c(
  "document_id", "document_name", "paper_id", "publication_year",
  "study_design", "sport_discipline", "athlete_level", "view_link"
))

tab2 <- read_table(expected_files[["tab2"]], c(
  "document_id", "document_name", "paper_id", "n_total",
  "age_mean", "age_sd", "sex_ratio", "view_link"
))

tab3 <- read_table(expected_files[["tab3"]], c(
  "document_id", "document_name", "paper_id", "bt_classification",
  "dose_frequency", "dose_duration", "comparator_type", "view_link"
))

tab4 <- read_table(expected_files[["tab4"]], c(
  "document_id", "document_name", "paper_id", "outcome_metric",
  "ig_n", "ig_mean", "ig_dispersion", "view_link"
))

tab5 <- read_table(expected_files[["tab5"]], c(
  "document_id", "document_name", "paper_id", "cg_n",
  "cg_mean", "cg_dispersion", "effect_estimate", "view_link"
))

tab6 <- read_table(expected_files[["tab6"]], c(
  "document_id", "document_name", "paper_id", "adverse_events",
  "adherence_rate", "dropout_rate", "feasibility_score", "view_link"
))

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
  text <- str_squish(as.character(x))
  is_numeric_like <- str_detect(text, "^[+-]?\\d") |
    str_detect(str_to_lower(text), "^[a-z]+(-[a-z]+)?\\s*%?$")
  value <- ifelse(is_numeric_like, parse_numeric(text), NA_real_)
  ifelse(!is.na(value) & value > 1, value / 100, value)
}

parse_duration_days <- function(x) {
  value <- parse_numeric(x)
  lower <- str_to_lower(as.character(x))
  case_when(
    is.na(value) ~ NA_real_,
    str_detect(lower, "week") ~ value * 7,
    str_detect(lower, "month") ~ value * 30.44,
    TRUE ~ value
  )
}

parse_frequency_per_week <- function(x) {
  lower <- str_to_lower(as.character(x))
  value <- parse_numeric(x)
  case_when(
    is.na(lower) ~ NA_real_,
    str_detect(lower, "daily|every day|per day") ~ 7,
    str_detect(lower, "week") & !is.na(value) ~ value,
    str_detect(lower, "month") & !is.na(value) ~ value / 4.35,
    !is.na(value) ~ value,
    TRUE ~ NA_real_
  )
}

classify_bt <- function(x) {
  lower <- str_to_lower(as.character(x))
  case_when(
    is.na(lower) ~ "other_unclear",
    str_detect(lower, "hypoventilation|low lung|rsh-vh|rs-vh") ~ "hypoventilation",
    str_detect(lower, "hyperventilation|wim hof|evh|hypocapnia|fast-paced") ~ "hyperventilation",
    str_detect(lower, "paced|coherent|6 bpm|box breathing|regulated breathing|slow") ~ "paced_breathing",
    str_detect(lower, "breath-hold|breath holding|breath-holding|apnea|valsalva") ~ "breath_holding",
    str_detect(lower, "pranayama|yogic|bhramari|bhastrika|nadi shodhana") ~ "pranayama_yogic",
    str_detect(lower, "diaphragmatic|abdominal|thoracic") ~ "diaphragmatic",
    str_detect(lower, "hook") ~ "hook_breathing",
    TRUE ~ "other_unclear"
  )
}

normalize_study_design <- function(x) {
  lower <- str_to_lower(as.character(x))
  case_when(
    is.na(lower) ~ "unclear",
    str_detect(lower, "random") & str_detect(lower, "crossover|cross-over") ~ "randomized_crossover",
    str_detect(lower, "crossover|cross-over|within-subject") ~ "crossover",
    str_detect(lower, "random") & str_detect(lower, "control|trial|matched") ~ "parallel_or_controlled_trial",
    str_detect(lower, "single group|pre-experimental|observational") ~ "single_group_or_observational",
    str_detect(lower, "retrospective") ~ "retrospective",
    str_detect(lower, "repeated") ~ "repeated_measures",
    TRUE ~ "other_or_unclear"
  )
}

split_values <- function(x) {
  if (is.na(x)) return(NA_character_)
  str_split(x, "\\s*;\\s*")[[1]]
}

split_semicolon_rows <- function(df, split_cols) {
  purrr::pmap_dfr(df, function(...) {
    row <- tibble(...)
    values <- lapply(split_cols, function(col) split_values(row[[col]][[1]]))
    names(values) <- split_cols
    max_len <- max(vapply(values, length, integer(1)), na.rm = TRUE)
    expanded <- row[rep(1, max_len), , drop = FALSE]
    for (col in split_cols) {
      vals <- values[[col]]
      if (length(vals) == 1 && max_len > 1) vals <- rep(vals, max_len)
      if (length(vals) < max_len) vals <- c(vals, rep(NA_character_, max_len - length(vals)))
      expanded[[col]] <- vals[seq_len(max_len)]
    }
    expanded$outcome_index <- seq_len(max_len)
    expanded
  })
}

tab1_clean <- tab1 %>%
  mutate(
    publication_year = parse_numeric(publication_year),
    design_group = normalize_study_design(study_design)
  )

tab2_clean <- tab2 %>%
  mutate(
    n_total_num = parse_numeric(n_total),
    age_mean_num = parse_numeric(age_mean),
    age_sd_num = parse_numeric(age_sd),
    sex_ratio_prop = parse_percent(sex_ratio),
    male_n = n_total_num * sex_ratio_prop,
    female_n = n_total_num * (1 - sex_ratio_prop)
  )

tab3_clean <- tab3 %>%
  mutate(
    bt_category = classify_bt(bt_classification),
    dose_duration_days = parse_duration_days(dose_duration),
    dose_frequency_week = parse_frequency_per_week(dose_frequency)
  )

tab4_long <- split_semicolon_rows(tab4, c("outcome_metric", "ig_n", "ig_mean", "ig_dispersion")) %>%
  mutate(
    ig_n_num = parse_numeric(ig_n),
    ig_mean_num = parse_numeric(ig_mean),
    ig_sd_num = parse_numeric(ig_dispersion)
  )

tab5_long <- split_semicolon_rows(tab5, c("cg_n", "cg_mean", "cg_dispersion", "effect_estimate")) %>%
  mutate(
    cg_n_num = parse_numeric(cg_n),
    cg_mean_num = parse_numeric(cg_mean),
    cg_sd_num = parse_numeric(cg_dispersion)
  )

tab6_clean <- tab6 %>%
  mutate(
    adherence_prop = parse_percent(adherence_rate),
    dropout_prop = parse_percent(dropout_rate)
  )

cleaned_data <- list(
  tab1 = tab1_clean,
  tab2 = tab2_clean,
  tab3 = tab3_clean,
  tab4 = tab4_long,
  tab5 = tab5_long,
  tab6 = tab6_clean
)

saveRDS(cleaned_data, file.path(outputs_dir, "cleaned_data.rds"))

data_quality <- bind_rows(
  tab2_clean %>%
    transmute(table = "tab2_descriptive", paper_id, issue = if_else(is.na(n_total_num), "unparseable_n_total", NA_character_)),
  tab3_clean %>%
    transmute(table = "tab3_specifics", paper_id, issue = if_else(is.na(bt_category), "unclassified_bt", NA_character_)),
  tab4_long %>%
    transmute(table = "tab4_intervention", paper_id, issue = if_else(is.na(ig_mean_num) | is.na(ig_sd_num) | is.na(ig_n_num), "incomplete_intervention_effect_fields", NA_character_)),
  tab5_long %>%
    transmute(table = "tab5_control", paper_id, issue = if_else(is.na(cg_mean_num) | is.na(cg_sd_num) | is.na(cg_n_num), "incomplete_control_effect_fields", NA_character_))
) %>%
  filter(!is.na(issue))

readr::write_csv(data_quality, file.path(tables_dir, "data_quality_log.csv"))
