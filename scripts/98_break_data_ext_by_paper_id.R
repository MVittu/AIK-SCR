input_file <- file.path("Data", "Data-ext.csv")
output_file <- file.path("Data", "Data-ext_by_paper_id.csv")

# Set to TRUE only if you want to replace Data/Data-ext.csv in place.
overwrite_input <- FALSE

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

raw_text <- paste(readLines(input_file, warn = FALSE, encoding = "UTF-8"), collapse = " ")
raw_text <- trimws(raw_text)

if (!nzchar(raw_text)) {
  warning("Input file is empty: ", input_file)
  writeLines(character(), if (overwrite_input) input_file else output_file, useBytes = TRUE)
  quit(save = "no", status = 0)
}

# Paper_ID is expected to look like NameYear, e.g. Woorons2024 or Spiering2004.
# The regex inserts a newline before each Paper_ID, except at the very beginning.
paper_id_pattern <- "\\b[A-Z][A-Za-z.'-]*20[0-9]{2}\\b"
matches <- gregexpr(paper_id_pattern, raw_text, perl = TRUE)[[1]]

if (identical(matches, -1L)) {
  warning("No Paper_ID patterns found in: ", input_file)
  writeLines(raw_text, if (overwrite_input) input_file else output_file, useBytes = TRUE)
  quit(save = "no", status = 0)
}

match_lengths <- attr(matches, "match.length")
starts <- as.integer(matches)
ends <- starts + match_lengths - 1L

pieces <- character(length(starts))
for (i in seq_along(starts)) {
  record_start <- starts[[i]]
  record_end <- if (i < length(starts)) starts[[i + 1L]] - 1L else nchar(raw_text)
  pieces[[i]] <- trimws(substr(raw_text, record_start, record_end))
}

prefix <- trimws(substr(raw_text, 1L, starts[[1L]] - 1L))
if (nzchar(prefix)) {
  pieces <- c(prefix, pieces)
}

target_file <- if (overwrite_input) input_file else output_file
writeLines(pieces, target_file, useBytes = TRUE)

message("Wrote ", length(pieces), " line(s) to ", target_file)
