# Packages
dir.create("figures", showWarnings = FALSE)
library(tidyverse)
library(jsonlite)


# -------------------------
# ELECTION DATA
# -------------------------

# Load MIT county presidential election data
election = read_csv("data/countypres_2000-2024.csv")

# Keep selected elections and major parties
# Use TOTAL vote rows when available; otherwise sum voting modes
election_major = election %>%
  filter(
    year %in% c(2012, 2016, 2020, 2024),
    party %in% c("DEMOCRAT", "REPUBLICAN"),
    !is.na(county_fips)
  ) %>%
  group_by(
    year, state, state_po, county_name,
    county_fips, party
  ) %>%
  summarise(
    candidatevotes = if (any(mode == "TOTAL", na.rm = TRUE)) {
      sum(candidatevotes[mode == "TOTAL"], na.rm = TRUE)
    } else {
      sum(candidatevotes, na.rm = TRUE)
    },
    totalvotes = first(totalvotes),
    .groups = "drop"
  )

# Calculate Democratic and Republican county vote shares
election_county = election_major %>%
  mutate(
    vote_share = candidatevotes / totalvotes
  ) %>%
  select(
    year, state, state_po, county_name,
    county_fips, party, vote_share
  ) %>%
  pivot_wider(
    names_from = party,
    values_from = vote_share
  ) %>%
  rename(
    dem_vote_share = DEMOCRAT,
    rep_vote_share = REPUBLICAN
  )


# -------------------------
# CENSUS / ACS DATA
# -------------------------

# Census API key
census_key = Sys.getenv("CENSUS_API_KEY")

# Download income and education data for one ACS year
get_acs_data = function(year) {
  
  url = paste0(
    "https://api.census.gov/data/", year,
    "/acs/acs5?",
    "get=NAME,B19013_001E,B15003_001E,B15003_022E,",
    "B15003_023E,B15003_024E,B15003_025E",
    "&for=county:*&in=state:*",
    if (nzchar(census_key)) paste0("&key=", census_key) else ""
  )
  
  raw = read_json(url, simplifyVector = TRUE)
  
  data = as_tibble(raw[-1, ])
  names(data) = raw[1, ]
  
  data = data %>%
    mutate(
      across(starts_with("B"), as.numeric),
      across(starts_with("B"), ~ ifelse(.x < 0, NA, .x)),
      county_fips = paste0(state, county),
      year = year,
      
      # Median household income
      median_income = B19013_001E,
      
      # Bachelor's degree or higher among population age 25+
      bachelors_or_higher =
        B15003_022E +
        B15003_023E +
        B15003_024E +
        B15003_025E,
      
      bachelors_pct =
        bachelors_or_higher / B15003_001E
    ) %>%
    select(
      year,
      county_fips,
      median_income,
      bachelors_pct
    )
  
  return(data)
}

# Download ACS 5-year data for each election year
acs_2012 = get_acs_data(2012)
acs_2016 = get_acs_data(2016)
acs_2020 = get_acs_data(2020)
acs_2024 = get_acs_data(2024)

# Combine ACS years
acs = bind_rows(
  acs_2012,
  acs_2016,
  acs_2020,
  acs_2024
) %>%
  mutate(
    median_income = ifelse(median_income < 0, NA, median_income),
    bachelors_pct = ifelse(
      bachelors_pct < 0 | bachelors_pct > 1,
      NA,
      bachelors_pct
    )
  )


# -------------------------
# MERGE AND CLEAN
# -------------------------

# Standardize county FIPS to five digits
election_county = election_county %>%
  mutate(
    county_fips = str_pad(
      as.character(county_fips),
      width = 5,
      pad = "0"
    )
  )

# Merge election and demographic data by county and year
project_data = election_county %>%
  left_join(
    acs,
    by = c("year", "county_fips")
  )

# Keep complete matched county-year observations
# Excludes geographic units that cannot be directly matched to ACS counties
analysis_data = project_data %>%
  filter(
    !is.na(median_income),
    !is.na(bachelors_pct),
    !is.na(dem_vote_share),
    !is.na(rep_vote_share)
  )


# -------------------------
# CORRELATION ANALYSIS
# -------------------------

# County-level Pearson correlations by election year
correlations = analysis_data %>%
  group_by(year) %>%
  summarise(
    income_dem = cor(median_income, dem_vote_share),
    education_dem = cor(bachelors_pct, dem_vote_share),
    income_rep = cor(median_income, rep_vote_share),
    education_rep = cor(bachelors_pct, rep_vote_share)
  )

correlations

# Prepare Democratic correlations for visualization
correlations_long = correlations %>%
  select(year, income_dem, education_dem) %>%
  pivot_longer(
    cols = c(income_dem, education_dem),
    names_to = "variable",
    values_to = "correlation"
  ) %>%
  mutate(
    variable = recode(
      variable,
      income_dem = "Median Household Income",
      education_dem = "Bachelor's Degree or Higher"
    )
  )

# Correlation trends across elections
ggplot(
  correlations_long,
  aes(
    x = year,
    y = correlation,
    group = variable,
    linetype = variable
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = round(correlation, 2)),
    vjust = -1,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = c(2012, 2016, 2020, 2024)
  ) +
  scale_y_continuous(
    limits = c(0, 0.6),
    breaks = seq(0, 0.6, 0.1)
  ) +
  labs(
    title = "Socioeconomic Characteristics and Democratic Vote Share",
    subtitle = "County-level Pearson correlations across presidential elections",
    x = "Election Year",
    y = "Pearson Correlation (r)",
    linetype = NULL
  ) +
  theme_minimal()

# Figure 1: correlation trends across elections
correlation_plot = ggplot(
  correlations_long,
  aes(x = year, y = correlation, group = variable, linetype = variable)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = round(correlation, 2)),
    vjust = -1,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = c(2012, 2016, 2020, 2024)
  ) +
  scale_y_continuous(
    limits = c(0, 0.6),
    breaks = seq(0, 0.6, 0.1)
  ) +
  labs(
    x = "Election Year",
    y = "Pearson Correlation (r)",
    linetype = NULL
  ) +
  theme_minimal()

correlation_plot

ggsave(
  filename = "figures/correlation_trends.png",
  plot = correlation_plot,
  width = 7,
  height = 4,
  dpi = 300
)

getwd()
file.exists("figures/correlation_trends.png")

# -------------------------
# EXPLORATORY VISUALIZATIONS
# -------------------------

# Education vs. Democratic vote share
ggplot(
  analysis_data,
  aes(x = bachelors_pct, y = dem_vote_share)
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Educational Attainment and Democratic Vote Share",
    subtitle = "County-level relationships across presidential elections",
    x = "Bachelor's Degree or Higher",
    y = "Democratic Vote Share"
  ) +
  theme_minimal()

# Income vs. Democratic vote share
ggplot(
  analysis_data,
  aes(x = median_income, y = dem_vote_share)
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Median Household Income and Democratic Vote Share",
    subtitle = "County-level relationships across presidential elections",
    x = "Median Household Income",
    y = "Democratic Vote Share"
  ) +
  theme_minimal()


# -------------------------
# MODEL COMPARISON
# -------------------------

# Compare income-only, education-only, and combined models
model_results = analysis_data %>%
  group_by(year) %>%
  group_modify(~ {
    
    income_model = lm(
      dem_vote_share ~ median_income,
      data = .x
    )
    
    education_model = lm(
      dem_vote_share ~ bachelors_pct,
      data = .x
    )
    
    combined_model = lm(
      dem_vote_share ~ median_income + bachelors_pct,
      data = .x
    )
    
    tibble(
      model = c(
        "Income Only",
        "Education Only",
        "Income + Education"
      ),
      
      r_squared = c(
        summary(income_model)$r.squared,
        summary(education_model)$r.squared,
        summary(combined_model)$r.squared
      ),
      
      rmse = c(
        sqrt(mean(residuals(income_model)^2)),
        sqrt(mean(residuals(education_model)^2)),
        sqrt(mean(residuals(combined_model)^2))
      )
    )
  }) %>%
  ungroup()

model_results

# Figure 2: model comparison
model_plot = ggplot(
  model_results,
  aes(
    x = factor(year),
    y = r_squared,
    group = model,
    linetype = model
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(
      label = round(r_squared, 2),
      vjust = case_when(
        model == "Education Only" ~ 1.8,
        TRUE ~ -1
      )
    ),
    show.legend = FALSE,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, 0.36),
    breaks = seq(0, 0.35, 0.05)
  ) +
  labs(
    x = "Election Year",
    y = expression(R^2),
    linetype = "Model"
  ) +
  theme_minimal()

model_plot

ggsave(
  filename = "figures/model_comparison.png",
  plot = model_plot,
  width = 7,
  height = 4,
  dpi = 300
)

# -------------------------
# STANDARDIZED COEFFICIENTS
# -------------------------

# Standardize predictors so income and education coefficients are comparable
coefficient_results = analysis_data %>%
  group_by(year) %>%
  group_modify(~ {
    
    model = lm(
      dem_vote_share ~
        scale(median_income) +
        scale(bachelors_pct),
      data = .x
    )
    
    tibble(
      variable = c("Income", "Education"),
      coefficient = coef(model)[2:3]
    )
  }) %>%
  ungroup()

coefficient_results

# Figure 3: standardized coefficients
coefficient_plot = ggplot(
  coefficient_results,
  aes(
    x = factor(year),
    y = coefficient,
    group = variable,
    linetype = variable
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(
      label = round(coefficient, 3),
      vjust = case_when(
        variable == "Education" ~ -1,
        TRUE ~ 1.8
      )
    ),
    show.legend = FALSE,
    size = 3
  ) +
  labs(
    x = "Election Year",
    y = "Standardized Coefficient",
    linetype = "Variable"
  ) +
  theme_minimal()

coefficient_plot

ggsave(
  filename = "figures/standardized_coefficients.png",
  plot = coefficient_plot,
  width = 7,
  height = 4,
  dpi = 300
)

# -------------------------
# CROSS-VALIDATION
# -------------------------

# 10-fold cross-validation for out-of-sample model comparison
set.seed(123)

cv_results = analysis_data %>%
  group_by(year) %>%
  group_modify(~ {
    
    data = .x %>%
      mutate(
        fold = sample(rep(1:10, length.out = n()))
      )
    
    map_dfr(1:10, function(i) {
      
      train = data %>%
        filter(fold != i)
      
      test = data %>%
        filter(fold == i)
      
      income_model = lm(
        dem_vote_share ~ median_income,
        data = train
      )
      
      education_model = lm(
        dem_vote_share ~ bachelors_pct,
        data = train
      )
      
      combined_model = lm(
        dem_vote_share ~ median_income + bachelors_pct,
        data = train
      )
      
      tibble(
        fold = i,
        model = c(
          "Income Only",
          "Education Only",
          "Income + Education"
        ),
        
        rmse = c(
          sqrt(mean(
            (test$dem_vote_share -
               predict(income_model, test))^2
          )),
          
          sqrt(mean(
            (test$dem_vote_share -
               predict(education_model, test))^2
          )),
          
          sqrt(mean(
            (test$dem_vote_share -
               predict(combined_model, test))^2
          ))
        )
      )
    })
  }) %>%
  ungroup()

# Average cross-validation performance across folds
cv_summary = cv_results %>%
  group_by(year, model) %>%
  summarise(
    mean_rmse = mean(rmse),
    sd_rmse = sd(rmse),
    .groups = "drop"
  )

cv_summary

# Cross-validated model performance
ggplot(
  cv_summary,
  aes(
    x = factor(year),
    y = mean_rmse,
    group = model,
    linetype = model
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Cross-Validated Model Performance",
    subtitle = "10-fold cross-validation predicting Democratic county vote share",
    x = "Election Year",
    y = "Mean RMSE",
    linetype = "Model"
  ) +
  theme_minimal()