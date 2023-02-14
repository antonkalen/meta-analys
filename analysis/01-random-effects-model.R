# This script is used to calculate a three-level random effects
# meta-analytical model. It requires a data file in .xlsx format.


# Load packages -----------------------------------------------------------

library(here)
library(readxl)
library(janitor)
library(dplyr)
library(brms)
library(purrr)
library(tidybayes)
library(tidyr)
library(readr)
library(glue)
library(ggplot2)

# Set parameters ----------------------------------------------------------
# In this part, we set all the parameters that we might be interested in changing

# Data file name (needs quotes around name, and file ending)
data_file <- "Copy of Meta-analysis RE-AIM FINAL DB221031.xlsx"

# Confidence level
ci <- .95

# Priors for the model
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(cauchy(0, .3), class = sd),
  prior(cauchy(0, .3), class = sigma)
)

# Iterations
iter <- 8000
chains <- 4
adapt_delta <- .999

# Load data ---------------------------------------------------------------

meta_data <- read_xlsx(path = here("data", data_file))


# Prepare data ------------------------------------------------------------

# Create R-friendly names (lowercase with underscore)
clean_data <- clean_names(meta_data)

# Create a nested dataframe for each outcome
nested_data <- clean_data |> nest_by(outcome)

# Add column for maximum number of effect sizes from the same study in each outcome
clean_data <- nested_data |> 
  mutate(nr = max(table(data["study_id"]))) 

# Create initial model ----------------------------------------------------

model_three_level <- brm(
  hedgess_g | se(std_err, sigma = TRUE) ~ 1 + (1|study_id),
  data = clean_data[[1,2]][[1]],
  prior = priors,
  cores = 4,
  chains = chains,
  iter = iter,
  control = list(adapt_delta = adapt_delta)
)

model_two_level <- brm(
  hedgess_g | se(std_err) ~ 1 + (1|study_id),
  data = clean_data[[1,2]][[1]],
  prior = priors[-3,], # Removes the sigma prior
  cores = 4,
  chains = chains,
  iter = iter,
  control = list(adapt_delta = adapt_delta)
)

# Run analysis ------------------------------------------------------------

models <- clean_data |> 
  mutate(
    model = list(
      if (nr > 1) {update(model_three_level, newdata = data)}
      else if (nr == 1) {update(model_two_level, newdata = data)}
    )
  )

# Check results -----------------------------------------------------------

draws_df <- models |> 
  transmute(
    draws = list(
      if (nr > 1) {gather_draws(model, b_Intercept, sd_study_id__Intercept, sigma)}
      else if (nr == 1) {gather_draws(model, b_Intercept, sd_study_id__Intercept)}
    )
  ) |> 
  unnest(draws) |> 
  mutate(
    .variable = case_when(
      .variable == "b_Intercept" ~ "es",
      .variable == "sd_study_id__Intercept" ~ "tau2_inter",
      .variable == "sigma" ~ "tau2_intra"
    ),
    .value = if_else(.variable %in% c("tau2_inter", "tau2_intra"), .value^2, .value)
  )


results <- draws_df |> 
  group_by(outcome, .variable) |> 
  summarise(median_hdci(.value, .width = ci)) |> 
  pivot_wider(
    id_cols = outcome, 
    names_from = .variable, 
    values_from = c(y, ymin, ymax),
    names_vary = "slowest"
  ) |> 
  mutate(
    across(
      where(is.numeric), 
      ~formatC(round_half_up(.x, digits = 2), format = "f", digits = 2)
    )
  )

write_excel_csv(results, file = here("output", "results.csv"))


# Visualisation -----------------------------------------------------------

draw_intercept <- draws_df |> 
  filter(.variable == "es")

axis_labels_summary <- draw_intercept |> 
  median_hdci(.value, .width = ci) |> 
  mutate(
    across(
      c(.value, .lower, .upper),
      ~formatC(round(., digits = 2), digits = 2, format = "f")
    ),
    m_ci = glue("{.value} [{.lower}, {.upper}]") 
  )

draw_intercept |> 
  ggplot(aes(x = .value, y = outcome)) +
  geom_vline(xintercept = 0, color = "gray30", size = .3) +
  stat_halfeye(
    .width = ci,
    orientation = "horizontal",
    normalize = "groups",
    fill = "gray80",
    size = .75,
    scale = .5,
    point_interval = median_hdci
  ) +
  geom_text(aes(label = m_ci, x = Inf), data = axis_labels_summary) +
  scale_x_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(.5, nrow(axis_labels_summary) + .8)) +
  theme_void(base_size = 6) +
  theme(
    axis.line.x.bottom = element_line(.3, color = "gray30"),
    axis.text.y.left = element_text(hjust = 1, vjust = 0),
    axis.text.y.right = element_text(hjust = 1, vjust = 0),
    axis.text.x = element_text(),
    axis.ticks.length.x = unit(1, "pt"),
    axis.ticks.x = element_line(color = "gray30", size = .3)
  )
