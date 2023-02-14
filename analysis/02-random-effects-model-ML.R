# This script is used to calculate a three-level random effects
# meta-analytical model. It requires a data file in .xlsx format.


# Load packages -----------------------------------------------------------

library(here)
library(readxl)
library(janitor)
library(metafor)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(glue)
library(ggplot2)
library(clubSandwich)

# Set parameters ----------------------------------------------------------
# In this part, we set all the parameters that we might be interested in changing

# Data file name (needs quotes around name, and file ending)
data_file <- "Copy of Meta-analysis RE-AIM FINAL DB221031.xlsx"

# Confidence level
ci <- .95

# Load data ---------------------------------------------------------------

meta_data <- read_xlsx(path = here("data", data_file))


# Prepare data ------------------------------------------------------------

# Create R-friendly names (lowercase with underscore)
clean_data <- clean_names(meta_data)

# Create a nested dataframe for each outcome
nested_data <- clean_data |> nest_by(outcome)

# Add column for maximum number of effect sizes from the same study in each outcome
clean_data <- nested_data |> 
  mutate(
    nr = max(table(data["study_id"]))
  )

# Run analysis ------------------------------------------------------------

models <- clean_data |> 
  mutate(
    model = list(
      if (nr > 1) {
        rma.mv(
          yi = hedgess_g,
          V = variance,
          random = ~ 1|study_id,
          data = data,
          level = ci
        )
      }
      else if (nr == 1) {
        rma(
          yi = hedgess_g,
          vi = variance,
          data = data,
          level = ci
        )
      }
    )
  )

# Check results -----------------------------------------------------------

results <- models |> 
  summarise(
    clean = broom::tidy(model, conf.int = TRUE, conf.level = ci)
  ) |> 
  unnest(clean)
  

write_excel_csv(results, file = here("output", "results_REML.csv"))


# Visualisation -----------------------------------------------------------

axis_labels_summary <- results |> 
  mutate(
    across(
      c(estimate, conf.low, conf.high),
      ~formatC(round(., digits = 2), digits = 2, format = "f")
    ),
    m_ci = glue("{estimate} [{conf.low}, {conf.high}]") 
  )

results |> 
  ggplot(aes(x = estimate, y = outcome)) +
  geom_vline(xintercept = 0, color = "gray30", size = .3) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_text(aes(label = m_ci, x = Inf), data = axis_labels_summary) +
  scale_x_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(.5, nrow(axis_labels_summary) + .8), clip = 'off') +
  theme_void(base_size = 6) +
  theme(
    axis.line.x.bottom = element_line(.3, color = "gray30"),
    axis.text.y.left = element_text(hjust = 1, vjust = 0),
    axis.text.y.right = element_text(hjust = 1, vjust = 0),
    axis.text.x = element_text(),
    axis.ticks.length.x = unit(1, "pt"),
    axis.ticks.x = element_line(color = "gray30", size = .3),
    plot.margin = margin(10, 80, 10, 10)
  )
