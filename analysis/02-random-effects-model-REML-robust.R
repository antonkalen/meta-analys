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
library(glue)
library(ggplot2)
library(clubSandwich)
library(broom.mixed)
library(writexl)
library(metaviz)

# Set parameters ----------------------------------------------------------
# In this part, we set all the parameters that we might be interested in changing

# Data file name (needs quotes around name, and file ending)
data_file <- "Copy of Meta-analysis RE-AIM FINAL DB221107.xlsx"

# Confidence level
ci <- .95

# Load data ---------------------------------------------------------------

meta_data <- read_xlsx(path = here("data", data_file))

# Prepare data ------------------------------------------------------------

# Create R-friendly names (lowercase with underscore)
# Remove all data missing vital info
clean_data <- clean_names(meta_data) |> 
  drop_na(study_id, outcome, hedgess_g, std_err, variance) |> 
  mutate(es_id = row_number())

# Create a nested dataframe for each outcome
nested_data <- clean_data |> nest_by(outcome)

# Add column for maximum number of effect sizes from the same study in each outcome
nested_data2 <- nested_data |> 
  mutate(
    nr = max(table(data["study_id"]))
  ) |> 
  filter(nrow(data["study_id"]) > 1)

# Run analysis ------------------------------------------------------------

# Descriptives
nr_studies <- clean_data |> 
  summarise(
    n_effect_sizes = n(),
    k_studies = n_distinct(study_id),
    .by = outcome
  )

# Models

models <- nested_data2 |> 
  mutate(
    model = list(
      if (nr > 1) {
        rma.mv(
          yi = hedgess_g,
          V = variance,
          random = ~ 1|study_id/es_id,
          data = data,
          test = "t",
          dfs = "contain",
          level = ci
        )
      }
      else if (nr == 1) {
        rma.mv(
          yi = hedgess_g,
          V = variance,
          random = ~ 1|es_id,
          data = data,
          test = "t",
          dfs = "contain",
          level = ci
        )
      }
    )
  )


# Egger regression --------------------------------------------------------

regtest_model <- rma.mv(
  yi = hedgess_g,
  V = std_err,
  random = ~ 1|study_id/es_id,
  mods = ~ std_err,
  test = "t",
  dfs = "contain",
  data = clean_data
)

eggers_result <- tidy(regtest_model) |> 
  filter(term == "std_err")

# Check results -----------------------------------------------------------

results <- models |> 
  summarise(
    clean = conf_int(model, cluster = data$study_id, vcov = "CR2", p_values = TRUE)
  ) |> 
  unnest(clean)

write_xlsx(
  list(coefs = results, eggers = eggers_result, nr_studies = nr_studies), 
  here("output", "results_REML_robust.xlsx")
)

# Visualisation -----------------------------------------------------------

axis_labels_summary <- results |> 
  mutate(
    across(
      c(beta, CI_L, CI_U),
      ~formatC(round(., digits = 2), digits = 2, format = "f")
    ),
    m_ci = glue("{beta} [{CI_L}, {CI_U}]") 
  )

results |> 
  ggplot(aes(x = beta, y = outcome)) +
  geom_vline(xintercept = 0, color = "gray30", linewidth = .3) +
  geom_pointrange(aes(xmin = CI_L, xmax = CI_U)) +
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



# Funnel plots ------------------------------------------------------------

funnel_mod <- rma.mv(
  yi = hedgess_g,
  V = std_err,
  random = ~ 1|study_id/es_id,
  data = clean_data
)

sunset_plot <- viz_sunset(funnel_mod)

ggsave(
  "sunset.tiff",
  plot = sunset_plot,
  path = "figs",
  width = 14,
  height = 14,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)
