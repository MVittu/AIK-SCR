if (!exists("tab2_clean")) source(file.path("scripts", "01_clean_data.R"))

demographic_rows <- tab2_clean %>%
  filter(!is.na(n_total_num))

age_rows <- demographic_rows %>%
  filter(!is.na(age_mean_num), !is.na(age_sd_num), n_total_num > 1)

weighted_age_mean <- safe_weighted_mean(age_rows$age_mean_num, age_rows$n_total_num)

pooled_age_sd <- {
  total_n <- sum(age_rows$n_total_num)
  if (is.na(weighted_age_mean) || total_n <= 1) {
    NA_real_
  } else {
  numerator <- sum((age_rows$n_total_num - 1) * age_rows$age_sd_num^2) +
    sum(age_rows$n_total_num * (age_rows$age_mean_num - weighted_age_mean)^2)
  sqrt(numerator / (total_n - 1))
  }
}

sex_summary <- demographic_rows %>%
  summarise(
    total_n_with_sex_data = sum(n_total_num[!is.na(sex_ratio_prop)], na.rm = TRUE),
    estimated_male_n = sum(male_n, na.rm = TRUE),
    estimated_female_n = sum(female_n, na.rm = TRUE),
    estimated_male_prop = if_else(
      estimated_male_n + estimated_female_n > 0,
      estimated_male_n / (estimated_male_n + estimated_female_n),
      NA_real_
    )
  )

age_summary <- tibble(
  studies_with_age_data = nrow(age_rows),
  participants_with_age_data = sum(age_rows$n_total_num),
  weighted_age_mean = weighted_age_mean,
  pooled_age_sd = pooled_age_sd
)

write_table_outputs(
  list(
    pooled_age = age_summary,
    sex_distribution = sex_summary,
    study_level_demographics = tab2_clean
  ),
  "table2_demographics"
)

sex_plot_data <- sex_summary %>%
  select(estimated_male_n, estimated_female_n) %>%
  pivot_longer(everything(), names_to = "sex", values_to = "estimated_n") %>%
  mutate(sex = recode(sex, estimated_male_n = "Male", estimated_female_n = "Female"))

sex_plot <- ggplot(sex_plot_data, aes(sex, estimated_n, fill = sex)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(Male = "#2C7FB8", Female = "#F768A1")) +
  labs(x = NULL, y = "Estimated participants")
save_plot(sex_plot, "sex_distribution.png", width = 5, height = 4)
