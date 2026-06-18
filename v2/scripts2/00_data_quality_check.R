if (!exists("source_file_ext65")) {
  source(file.path("v2", "scripts2", "00_setup.R"))
}

if (!file.exists(source_file_ext65)) {
  stop("Missing required data file: ", source_file_ext65)
}

expected_papers <- c(
  "Durand2000.pdf", "Spiering2004.pdf", "Lindholm2005.pdf", "Woorons2008.pdf",
  "Sakamoto2014.pdf", "Guimard2014.pdf", "Malakhov2014.pdf", "Jacob2015.pdf",
  "Sakamoto2015.pdf", "Stavrou2015.pdf", "Fujii2015.pdf", "Woorons2016.pdf",
  "Hakked2017.pdf", "Kacoglu2017.pdf", "Pelka2017.pdf", "Trincat2017.pdf",
  "Burtch2017.pdf", "Woorons2017.pdf", "FornasierSantos2018.pdf", "Gray2018.pdf",
  "Lim2018.pdf", "Sakamoto2018.pdf", "Fernandez2019.pdf", "Laborde2019.pdf",
  "Woorons2019.pdf", "Woorons2019b.pdf", "Bouten2020.pdf", "Lapointe2020.pdf",
  "Robertson2020.pdf", "Sakamoto2020.pdf", "Woorons2020.pdf", "Bahensky2021.pdf",
  "Citherlet2021.pdf", "Dobashi2021.pdf", "Grossman2021.pdf", "Taylor2021.pdf",
  "Vaidya2021.pdf", "Brocherie2022.pdf", "Conlon2022.pdf", "Marko2022.pdf",
  "You2023.pdf", "Braham2024.pdf", "Buxton2024.pdf", "Li2024.pdf",
  "Merlin2024.pdf", "Rosa2024.pdf", "Sikora2024.pdf", "Woorons2024.pdf",
  "Deniz2025.pdf", "Fernandez2025.pdf", "Fesseler2025.pdf", "Fox2025.pdf",
  "Kasap2025.pdf", "Liapaki2025.pdf", "Lorinczi2025.pdf", "Yilmaz2025.pdf",
  "Chiron2026.pdf", "Devipriya2026.pdf", "FernandezBarradas2026.pdf",
  "Iskra2026.pdf", "Jing2026.pdf", "Jones2026.pdf", "Katz2026.pdf",
  "Raidl2026.pdf", "Tomita2026.pdf"
)

required_columns_ext65 <- c(
  "Paper_ID", "First_Author", "Year", "Title_Short", "Journal", "Country",
  "Study_Design", "Sport_Discipline", "Sport_Category", "Athlete_Level",
  "N_Total", "N_Intervention", "N_Control", "Age_Mean", "Age_SD", "Age_Range",
  "Sex_Male_N", "Sex_Female_N", "Sex_Male_Pct", "Race_Ethnicity_Reported",
  "Baseline_Anthropometrics", "Baseline_Fitness", "Breathing_Technique",
  "Breathing_Category", "Technique_Description", "Device_Free", "Biofeedback_Used",
  "Comparator_Type", "Comparator_Description", "Intervention_Timing",
  "Session_Frequency", "Session_Duration", "Total_Duration", "Supervision",
  "Primary_Outcome_Domain", "Primary_Outcome_Measure", "Primary_Result_Direction",
  "Primary_Effect_Value", "Primary_Uncertainty", "Secondary_Outcome",
  "Efficacy_Summary", "Adherence_Reported", "Adherence_Value", "Dropout_Reported",
  "Dropout_N", "Dropout_Pct", "Adverse_Events_Reported",
  "Adverse_Events_Description", "Safety_Summary", "Feasibility_Summary",
  "Main_Limitations", "Risk_of_Bias_Concerns", "Extraction_Notes"
)

raw_ext65_for_quality <- utils::read.csv(
  source_file_ext65,
  colClasses = "character",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

make_issue <- function(severity, paper_id, source_row, field, issue, value = NA_character_, expected_action = NA_character_) {
  tibble(
    severity = severity,
    paper_id = paper_id,
    source_row = source_row,
    field = field,
    issue = issue,
    value = value,
    expected_action = expected_action
  )
}

parse_number_quality <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x), locale = readr::locale(decimal_mark = ".")))
}

quality_issues <- list()

missing_columns <- setdiff(required_columns_ext65, names(raw_ext65_for_quality))
extra_columns <- setdiff(names(raw_ext65_for_quality), required_columns_ext65)

if (length(missing_columns) > 0) {
  quality_issues[["missing_columns"]] <- make_issue(
    "critical", NA_character_, NA_integer_, "header",
    paste("missing_required_columns", paste(missing_columns, collapse = "; ")),
    expected_action = "Add the missing required columns before analysis."
  )
}

if (length(extra_columns) > 0) {
  quality_issues[["extra_columns"]] <- make_issue(
    "warning", NA_character_, NA_integer_, "header",
    paste("unexpected_extra_columns", paste(extra_columns, collapse = "; ")),
    expected_action = "Confirm whether these fields should be incorporated."
  )
}

if (length(missing_columns) == 0) {
  quality_data <- raw_ext65_for_quality %>%
    mutate(
      source_row = row_number() + 1L,
      Paper_ID = str_squish(Paper_ID),
      Year_num = parse_number_quality(Year),
      N_Total_num = parse_number_quality(N_Total),
      row_position_expected = match(Paper_ID, expected_papers),
      row_position_actual = row_number()
    )

  missing_papers <- setdiff(expected_papers, quality_data$Paper_ID)
  extra_papers <- setdiff(quality_data$Paper_ID, expected_papers)

  if (nrow(quality_data) != length(expected_papers)) {
    quality_issues[["row_count"]] <- make_issue(
      "error", NA_character_, NA_integer_, "Paper_ID",
      paste0("expected_", length(expected_papers), "_rows_but_found_", nrow(quality_data)),
      expected_action = "Add missing papers or remove unintended rows."
    )
  }

  if (length(missing_papers) > 0) {
    quality_issues[["missing_papers"]] <- make_issue(
      "error", missing_papers, NA_integer_, "Paper_ID", "expected_paper_missing",
      expected_action = "Extract and add this paper in chronological order."
    )
  }

  if (length(extra_papers) > 0) {
    quality_issues[["extra_papers"]] <- make_issue(
      "error", extra_papers, NA_integer_, "Paper_ID", "paper_not_in_expected_list",
      expected_action = "Confirm this paper belongs in the v2 corpus."
    )
  }

  duplicate_ids <- quality_data %>%
    count(Paper_ID, name = "n") %>%
    filter(!is.na(Paper_ID), n > 1)
  if (nrow(duplicate_ids) > 0) {
    quality_issues[["duplicates"]] <- duplicate_ids %>%
      transmute(
        severity = "critical",
        paper_id = Paper_ID,
        source_row = NA_integer_,
        field = "Paper_ID",
        issue = paste0("duplicate_paper_id_n_", n),
        value = Paper_ID,
        expected_action = "Keep one row per study/report or distinguish multi-study reports explicitly."
      )
  }

  invalid_year <- quality_data %>%
    filter(is.na(Year_num) | Year_num < 1900 | Year_num > 2030)
  if (nrow(invalid_year) > 0) {
    quality_issues[["invalid_year"]] <- invalid_year %>%
      transmute(
        severity = "error", paper_id = Paper_ID, source_row, field = "Year",
        issue = "missing_or_implausible_year", value = Year,
        expected_action = "Enter a four-digit publication year."
      )
  }

  unparseable_n <- quality_data %>%
    filter(is.na(N_Total_num) | N_Total_num <= 0)
  if (nrow(unparseable_n) > 0) {
    quality_issues[["unparseable_n"]] <- unparseable_n %>%
      transmute(
        severity = "warning", paper_id = Paper_ID, source_row, field = "N_Total",
        issue = "missing_or_unparseable_total_sample_size", value = N_Total,
        expected_action = "Add total sample size if reported; otherwise keep NR."
      )
  }

  missing_primary_outcome <- quality_data %>%
    filter(is.na(Primary_Outcome_Measure) | str_to_upper(str_squish(Primary_Outcome_Measure)) %in% c("", "NR"))
  if (nrow(missing_primary_outcome) > 0) {
    quality_issues[["missing_primary_outcome"]] <- missing_primary_outcome %>%
      transmute(
        severity = "warning", paper_id = Paper_ID, source_row, field = "Primary_Outcome_Measure",
        issue = "primary_outcome_not_reported_in_extraction", value = Primary_Outcome_Measure,
        expected_action = "Extract the main outcome measure whenever possible."
      )
  }

  category_fields <- c("Study_Design", "Sport_Category", "Athlete_Level", "Breathing_Category", "Comparator_Type")
  low_information <- quality_data %>%
    select(source_row, Paper_ID, all_of(category_fields)) %>%
    pivot_longer(all_of(category_fields), names_to = "field", values_to = "value") %>%
    filter(str_to_upper(str_squish(value)) %in% c("NR", "UNCLEAR", "UNCLASSIFIED", "OTHER", "OTHER EXPERIMENTAL", "OTHER BREATH-REGULATION"))
  if (nrow(low_information) > 0) {
    quality_issues[["low_information_categories"]] <- low_information %>%
      transmute(
        severity = "warning", paper_id = Paper_ID, source_row, field,
        issue = "low_information_category_used", value,
        expected_action = "Use a broader defensible category where possible and explain uncertainty in Extraction_Notes."
      )
  }

  chronological_breaks <- quality_data %>%
    mutate(prev_year = lag(Year_num), prev_paper = lag(Paper_ID)) %>%
    filter(!is.na(prev_year), !is.na(Year_num), Year_num < prev_year)
  if (nrow(chronological_breaks) > 0) {
    quality_issues[["chronology"]] <- chronological_breaks %>%
      transmute(
        severity = "warning", paper_id = Paper_ID, source_row, field = "Year",
        issue = paste0("row_not_chronological_after_", prev_paper),
        value = as.character(Year),
        expected_action = "Move row to chronological position."
      )
  }
}

data_quality_results <- bind_rows(quality_issues)
if (nrow(data_quality_results) == 0) {
  data_quality_results <- make_issue(
    "pass", NA_character_, NA_integer_, NA_character_,
    "no_data_quality_issues_detected"
  )
}

data_quality_summary <- data_quality_results %>%
  count(severity, name = "n_issues") %>%
  arrange(match(severity, c("critical", "error", "warning", "pass")))

readr::write_csv(data_quality_results, file.path(tables_dir, "data_quality_ext65.csv"), na = "")
readr::write_csv(data_quality_summary, file.path(tables_dir, "data_quality_ext65_summary.csv"), na = "")

if (any(data_quality_results$severity == "critical")) {
  stop("Critical data quality issue(s) detected. See: ", file.path(tables_dir, "data_quality_ext65.csv"))
}

if (any(data_quality_results$severity %in% c("error", "warning"))) {
  message("Data quality issues detected. See: ", file.path(tables_dir, "data_quality_ext65.csv"))
}
