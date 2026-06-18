`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

required_packages <- c(
  "tidyverse",
  "readr",
  "janitor",
  "stringr",
  "metafor",
  "ggplot2"
)

install_missing_packages <- function(pkgs = required_packages) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  invisible(missing)
}

install_missing_packages()

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(janitor)
  library(stringr)
  library(metafor)
  library(ggplot2)
})

project_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."), mustWork = FALSE)
if (!dir.exists(file.path(project_root, "Data"))) {
  project_root <- normalizePath(getwd(), mustWork = FALSE)
}

data_dir <- file.path(project_root, "Data")
outputs_dir <- file.path(project_root, "outputs")
tables_dir <- file.path(outputs_dir, "tables")
plots_dir <- file.path(outputs_dir, "plots")

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 12))

save_plot <- function(plot, filename, width = 8, height = 5, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(plots_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

safe_median <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  median(x)
}

safe_quantile <- function(x, prob) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(quantile(x, prob))
}

safe_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  IQR(x)
}

safe_weighted_mean <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}

format_count_pct <- function(n, total = sum(n, na.rm = TRUE), digits = 1) {
  pct <- if (total > 0) 100 * n / total else rep(NA_real_, length(n))
  paste0(n, " (", sprintf(paste0("%.", digits, "f"), pct), "%)")
}

add_count_pct <- function(data, count_col = "n", pct_col = "relative_frequency", label_col = "label") {
  total <- sum(data[[count_col]], na.rm = TRUE)
  data[[pct_col]] <- if (total > 0) data[[count_col]] / total else NA_real_
  data[[label_col]] <- format_count_pct(data[[count_col]], total = total)
  data
}

write_table_outputs <- function(sheets, workbook_name) {
  purrr::iwalk(sheets, function(data, sheet_name) {
    csv_name <- paste0(workbook_name, "__", sheet_name, ".csv")
    readr::write_csv(data, file.path(tables_dir, csv_name), na = "")
  })

  invisible(file.path(tables_dir, paste0(workbook_name, "__*.csv")))
}
