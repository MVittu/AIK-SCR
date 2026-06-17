source(file.path("scripts", "01_clean_data.R"))
source(file.path("scripts", "02_bibliometric_methodology.R"))
source(file.path("scripts", "03_demographics.R"))
source(file.path("scripts", "04_intervention_parameters.R"))
source(file.path("scripts", "05_effect_sizes_meta_analysis.R"))
source(file.path("scripts", "06_feasibility_safety.R"))

message("Analysis complete. Tables written to: ", tables_dir)
message("Plots written to: ", plots_dir)
