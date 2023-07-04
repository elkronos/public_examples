# Check and load necessary libraries
necessary_packages <- c("tidyverse", "brms", "cmdstanr", "ggdist")
if (all(!necessary_packages %in% installed.packages())) {
  stop("Some necessary packages are not installed. Please install the packages 'tidyverse', 'brms', 'cmdstanr', 'ggdist' before proceeding.")
} else {
  library(tidyverse)
  library(brms)
  library(cmdstanr)
  library(ggdist)
}

# Function to create artificial data
create_teams_data <- function(nTeams, seed = 123) {
  set.seed(seed)
  stopifnot(length(nTeams) == 1, is.numeric(nTeams), nTeams > 0)
  teamSizes <- sample(10:100, nTeams, replace = TRUE)
  trueRates <- rbeta(nTeams, 2, 10)
  numberLeft <- rbinom(nTeams, teamSizes, trueRates)
  stopifnot(all(numberLeft <= teamSizes))
  teamId <- as.character(1:nTeams)
  data.frame(teamId, teamSizes, numberLeft)
}

# Function to fit model
fit_model <- function(data, dependent_var, independent_var, group_var) {
  stopifnot(all(c(dependent_var, independent_var, group_var) %in% names(data)))
  formula_str <- paste0(dependent_var, " | trials(", independent_var, ") ~ 1 + (1 | ", group_var, ")")
  brms::brm(
    as.formula(formula_str),
    data = data,
    family = binomial(link = "logit"),
    prior = prior(normal(0, 10), class = "Intercept"),
    chains = 4,
    iter = 5000,
    control = list(adapt_delta = 0.95),
    seed = 123,
    backend = "cmdstanr",
    refresh = 0,
    silent = 2
  )
}

# Function to extract posterior samples
extract_posterior_samples <- function(model, teamsData, group_var) {
  posteriorDraws <- as_draws_df(model, variable = "Intercept", regex = TRUE) %>%
    dplyr::select(-.draw, -.chain, -.iteration, -sd_teamId__Intercept) %>%
    tidyr::pivot_longer(cols = -b_Intercept, names_to = group_var, values_to = "paramValues") %>%
    dplyr::mutate(
      teamId = stringr::str_extract(teamId, "\\d+"),
      team = stringr::str_glue("Team {teamId}"),
      logOdds = paramValues + b_Intercept,
      estimatedTR = exp(logOdds) / (1 + exp(logOdds))
    ) %>%
    dplyr::left_join(teamsData %>% dplyr::select(group_var, teamSizes), by = group_var) %>%
    dplyr::mutate(
      team = stringr::str_glue("{team} (n={teamSizes})"),
      team = forcats::fct_reorder(factor(team), as.numeric(teamId))
    )
  return(posteriorDraws)
}

# Function to compute observed turnover rate by team
compute_observed_rate <- function(teamsData, group_var, dependent_var, independent_var) {
  observedRT <- teamsData %>% 
    dplyr::mutate(team = stringr::str_glue("Team {teamId}")) %>%
    dplyr::mutate(
      observedRT = get(dependent_var)/get(independent_var),
      team = stringr::str_glue("{team} (n={teamSizes})"),
      team = forcats::fct_reorder(factor(team), as.numeric(teamId))
    )
  return(observedRT)
}

# Function to create dataviz
create_dataviz <- function(posteriorDraws, observedRT, fixedEffect) {
  ggplot2::ggplot(data = posteriorDraws, aes(x=estimatedTR, group = team)) + 
    ggdist::stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_hdi", fill = "skyblue", normalize = "groups") +
    ggplot2::geom_point(data = observedRT, aes(x = observedRT, y = 0, group = team), color = "red", inherit.aes = F, size = 2.5, shape=3, stroke = 1.5) +
    ggplot2::geom_vline(xintercept = fixedEffect, linetype = "dashed", color = "#2C2F46") +
    ggplot2::facet_wrap(~team, ncol = 2) +
    ggplot2::scale_x_continuous(breaks = seq(0,1,0.1)) +
    ggplot2::labs(
      y = "NORMALIZED DENSITY",
      x = "ESTIMATED TURNOVER RATE",
      title = "Using Bayesian shrinkage in reporting employee turnover",
      caption = "\nThe black solid lines represent the 80% and 95% credibility intervals, respectively. The black dot represents the median of the Highest Confidence Interval.\nThe vertical dashed line represents the fixed, population-level effect estimate. The red cross represents the observed turnover rate for a given team."
    ) +
    ggplot2::theme(
      plot.title = element_text(color = '#2C2F46', face = "bold", size = 21, margin=margin(0,0,12,0)),
      plot.subtitle = element_text(color = '#2C2F46', face = "plain", size = 16, margin=margin(0,0,20,0)),
      plot.caption = element_text(color = '#2C2F46', face = "plain", size = 11, hjust = 0),
      axis.title.x.bottom = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), color = '#2C2F46', face = "plain", size = 13, hjust = 0),
      axis.title.y.left = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), color = '#2C2F46', face = "plain", size = 13, hjust = 1),
      axis.text.x = element_text(color = '#2C2F46', face = "plain", size = 12),
      axis.text.y = element_text(color = '#2C2F46', face = "plain", size = 9),
      strip.text.x = element_text(size = 11, face = "plain"),
      axis.line.x = element_line(colour = "#E0E1E6"),
      axis.line.y = element_line(colour = "#E0E1E6"),
      legend.position="",
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank()
    )
}

# Use the functions
teamsData <- create_teams_data(nTeams = 10)
model <- fit_model(teamsData, dependent_var = "numberLeft", independent_var = "teamSizes", group_var = "teamId")
posteriorDraws <- extract_posterior_samples(model, teamsData, group_var = "teamId")
observedRT <- compute_observed_rate(teamsData, group_var = "teamId", dependent_var = "numberLeft", independent_var = "teamSizes")
fixedEffect <- exp(fixef(model)$Estimate) / (1 + exp(fixef(model)$Estimate))
create_dataviz(posteriorDraws, observedRT, fixedEffect)
