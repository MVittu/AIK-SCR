if (!exists("tab1_clean")) source(file.path("scripts", "01_clean_data.R"))

annual_counts <- tab1_clean %>%
  filter(!is.na(publication_year)) %>%
  count(publication_year, name = "n_publications") %>%
  arrange(publication_year)

linear_model <- lm(n_publications ~ publication_year, data = annual_counts)
exponential_model <- lm(log(n_publications) ~ publication_year, data = annual_counts)

linear_predictions <- tibble(
  publication_year = annual_counts$publication_year,
  model = "linear",
  predicted_publications = as.numeric(predict(linear_model, newdata = annual_counts))
)

exponential_predictions <- tibble(
  publication_year = annual_counts$publication_year,
  model = "exponential",
  predicted_publications = as.numeric(exp(predict(exponential_model, newdata = annual_counts)))
)

trend_predictions <- bind_rows(linear_predictions, exponential_predictions) %>%
  left_join(annual_counts, by = "publication_year") %>%
  mutate(residual = n_publications - predicted_publications)

model_fit_metrics <- trend_predictions %>%
  group_by(model) %>%
  summarise(
    rmse_original_count_scale = sqrt(mean(residual^2)),
    mae_original_count_scale = mean(abs(residual)),
    .groups = "drop"
  )

trend_summary <- tibble(
  model = c("linear", "exponential"),
  estimate = c(coef(linear_model)[["publication_year"]], coef(exponential_model)[["publication_year"]]),
  intercept = c(coef(linear_model)[["(Intercept)"]], coef(exponential_model)[["(Intercept)"]]),
  r_squared = c(summary(linear_model)$r.squared, summary(exponential_model)$r.squared),
  adjusted_r_squared = c(summary(linear_model)$adj.r.squared, summary(exponential_model)$adj.r.squared),
  aic = c(AIC(linear_model), AIC(exponential_model)),
  bic = c(BIC(linear_model), BIC(exponential_model)),
  p_value = c(
    coef(summary(linear_model))[["publication_year", "Pr(>|t|)"]],
    coef(summary(exponential_model))[["publication_year", "Pr(>|t|)"]]
  )
) %>%
  left_join(model_fit_metrics, by = "model") %>%
  mutate(
    best_by_rmse = rmse_original_count_scale == min(rmse_original_count_scale, na.rm = TRUE),
    best_by_aic = aic == min(aic, na.rm = TRUE),
    best_by_bic = bic == min(bic, na.rm = TRUE)
  )

design_distribution <- tab1_clean %>%
  count(design_group, sort = TRUE) %>%
  mutate(relative_frequency = n / sum(n))

sport_distribution <- tab1_clean %>%
  separate_rows(sport_discipline, sep = "\\s*;\\s*") %>%
  mutate(sport_discipline = replace_na(str_to_lower(sport_discipline), "unclear")) %>%
  count(sport_discipline, sort = TRUE) %>%
  mutate(relative_frequency = n / sum(n))

write_table_outputs(
  list(
    annual_publications = annual_counts,
    trend_models = trend_summary,
    trend_predictions = trend_predictions,
    study_designs = design_distribution,
    sports = sport_distribution
  ),
  "table1_bibliometric_methodological"
)

trend_plot <- ggplot(annual_counts, aes(publication_year, n_publications)) +
  geom_col(fill = "#2C7FB8") +
  geom_line(
    data = trend_predictions,
    aes(y = predicted_publications, color = model),
    linewidth = 0.9
  ) +
  geom_point(
    data = trend_predictions,
    aes(y = predicted_publications, color = model),
    size = 2
  ) +
  scale_color_manual(values = c(linear = "#D95F0E", exponential = "#31A354")) +
  scale_x_continuous(breaks = annual_counts$publication_year) +
  labs(x = "Publication year", y = "Number of publications", color = "Trend model")
save_plot(trend_plot, "publication_trend.png")

design_plot <- design_distribution %>%
  ggplot(aes(reorder(design_group, n), n)) +
  geom_col(fill = "#41AB5D") +
  coord_flip() +
  labs(x = NULL, y = "Number of studies")
save_plot(design_plot, "study_design_distribution.png")

sport_plot <- sport_distribution %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(reorder(sport_discipline, n), n)) +
  geom_col(fill = "#756BB1") +
  coord_flip() +
  labs(x = NULL, y = "Number of study mentions")
save_plot(sport_plot, "sport_distribution.png")
