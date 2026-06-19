if (!exists("source_file_complete")) {
  source(file.path("v2", "scripts2", "00_setup.R"))
}

if (!file.exists(source_file_complete)) {
  stop("Missing required complete v2 data file: ", source_file_complete)
}

quality_raw <- utils::read.csv(
  source_file_complete,
  colClasses = "character",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM",
  na.strings = c("", "NA", "NR")
) %>%
  janitor::clean_names()

num <- function(x) suppressWarnings(as.numeric(x))

required_columns <- c(
  "paper_id", "row_type", "year", "design", "n_total",
  "bt_classification", "outcome_domain", "outcome_metric",
  "ig_n", "ig_mean", "ig_sd", "cg_n", "cg_mean", "cg_sd"
)

column_presence <- tibble(
  field = required_columns,
  present = field %in% names(quality_raw)
)

study_level <- quality_raw %>% distinct(paper_id, .keep_all = TRUE)

study_level_issues <- bind_rows(
  study_level %>% transmute(paper_id, issue = if_else(is.na(paper_id), "missing_paper_id", NA_character_)),
  study_level %>% transmute(paper_id, issue = if_else(is.na(year), "missing_year", NA_character_)),
  study_level %>% transmute(paper_id, issue = if_else(is.na(design), "missing_design", NA_character_)),
  study_level %>% transmute(paper_id, issue = if_else(is.na(n_total) | is.na(num(n_total)), "missing_or_non_numeric_n_total", NA_character_)),
  study_level %>% transmute(paper_id, issue = if_else(is.na(bt_classification), "missing_bt_classification", NA_character_))
) %>%
  filter(!is.na(issue))

efficacy_issues <- quality_raw %>%
  filter(row_type == "efficacy_outcome") %>%
  mutate(
    complete_for_smd_check = !is.na(num(ig_n)) & !is.na(num(ig_mean)) & !is.na(num(ig_sd)) &
      !is.na(num(cg_n)) & !is.na(num(cg_mean)) & !is.na(num(cg_sd)) &
      num(ig_n) > 1 & num(cg_n) > 1 & num(ig_sd) > 0 & num(cg_sd) > 0
  ) %>%
  transmute(
    paper_id,
    outcome_metric,
    issue = if_else(!complete_for_smd_check, "incomplete_numeric_fields_for_smd", NA_character_)
  ) %>%
  filter(!is.na(issue))

data_quality_summary <- tibble(
  metric = c(
    "rows_total",
    "unique_papers",
    "efficacy_rows",
    "smd_complete_rows",
    "study_level_issue_rows",
    "efficacy_issue_rows"
  ),
  value = c(
    nrow(quality_raw),
    n_distinct(quality_raw$paper_id),
    sum(quality_raw$row_type == "efficacy_outcome", na.rm = TRUE),
    sum(tolower(quality_raw$complete_for_smd) == "true", na.rm = TRUE),
    nrow(study_level_issues),
    nrow(efficacy_issues)
  )
)

data_quality_results <- bind_rows(
  study_level_issues %>% mutate(table = "study_level", outcome_metric = NA_character_),
  efficacy_issues %>% mutate(table = "efficacy_outcomes")
) %>%
  select(table, paper_id, outcome_metric, issue)

readr::write_csv(column_presence, file.path(tables_dir, "data_quality_complete_columns.csv"), na = "")
readr::write_csv(data_quality_results, file.path(tables_dir, "data_quality_complete.csv"), na = "")
readr::write_csv(data_quality_summary, file.path(tables_dir, "data_quality_complete_summary.csv"), na = "")
