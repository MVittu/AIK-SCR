if (!exists("project_root")) source(file.path("v2", "scripts2", "00_setup.R"))

# Point rmarkdown to RStudio's bundled pandoc (needed by PRISMA_save for PNG export)
pandoc_path <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
if (dir.exists(pandoc_path)) Sys.setenv(RSTUDIO_PANDOC = pandoc_path)

# Install GitHub version (newer than CRAN 1.1.1)
if (!requireNamespace("remotes",   quietly = TRUE)) install.packages("remotes",   repos = "https://cloud.r-project.org")
if (!requireNamespace("rsvg",      quietly = TRUE)) install.packages("rsvg",      repos = "https://cloud.r-project.org")
if (!requireNamespace("PRISMA2020", quietly = TRUE) ||
    as.character(packageVersion("PRISMA2020")) == "1.1.1") {
  remotes::install_github("prisma-flowdiagram/PRISMA2020", upgrade = "never")
}
library(PRISMA2020)

# Load template and fill in review numbers
template_csv <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
d <- read.csv(template_csv, stringsAsFactors = FALSE)

set_n <- function(df, field, value) {
  df$n[!is.na(df$data) & df$data == field] <- as.character(value)
  df
}

d <- d |>
  set_n("database_results",          272) |>
  set_n("database_specific_results", "PubMed, 272") |>
  set_n("register_results",            0) |>
  set_n("register_specific_results",  "") |>
  set_n("website_results",             0) |>
  set_n("organisation_results",        0) |>
  set_n("citations_results",           3) |>  # Taylor 2021, Fernandez 2019, Fernandez 2025
  set_n("duplicates",                  0) |>
  set_n("excluded_automatic",          0) |>
  set_n("excluded_other",              0) |>
  set_n("records_screened",          272) |>
  set_n("records_excluded",          229) |>
  set_n("dbr_sought_reports",         43) |>
  set_n("dbr_notretrieved_reports",    0) |>
  set_n("other_sought_reports",        3) |>
  set_n("other_notretrieved_reports",  0) |>
  set_n("dbr_assessed",               43) |>
  set_n("dbr_excluded",      "Ineligible, 12") |>
  set_n("other_assessed",              3) |>
  set_n("other_excluded",    "None, 0") |>
  set_n("new_studies",                34) |>
  set_n("new_reports",                34) |>
  set_n("total_studies",              34) |>
  set_n("total_reports",              34)

prisma_data <- PRISMA_data(d)

prisma_plot <- PRISMA_flowdiagram(
  prisma_data,
  interactive        = FALSE,
  previous           = FALSE,
  other              = TRUE,
  detail_databases   = TRUE,
  detail_registers   = FALSE,
  side_boxes         = TRUE,
  fontsize           = 7,
  font               = "Helvetica"
)

output_png <- file.path(plots_dir, "prisma_flow.png")
PRISMA_save(prisma_plot, filename = output_png, filetype = "PNG", overwrite = TRUE, width = 4000)

message("PRISMA flow diagram saved to: ", output_png)
