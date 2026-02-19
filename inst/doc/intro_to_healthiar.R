## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, eval=TRUE, include=TRUE)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
options(knitr.kable.max_rows = 10)
set.seed(1)

## Avoid using pacman here, as it causes error in installation if it's not installed already
library(healthiar)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(knitr)
library(terra)
library(sf)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# results_pm_copd <- attribute_health(
#   exp_central = 8.85,
#   rr_central = 1.369,
#   rr_increment = 10,
#   erf_shape = "log_linear",
#   cutoff_central = 5,
#   bhd_central = 30747
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# results_pm_copd <- attribute_health(
#   erf_shape = "log_linear",
#   rr_central = exdat_pm$relative_risk,
#   rr_increment = 10,
#   exp_central = exdat_pm$mean_concentration,
#   cutoff_central = exdat_pm$cut_off_value,
#   bhd_central = exdat_pm$incidence
# )

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
results_pm_copd <- attribute_health(
  approach_risk = "relative_risk", # If you do not call this argument, "relative_risk" will be assigned by default.
  erf_shape = "log_linear",
  rr_central = exdat_pm$relative_risk, 
  rr_increment = 10, 
  exp_central = exdat_pm$mean_concentration,
  cutoff_central = exdat_pm$cut_off_value,
  bhd_central = exdat_pm$incidence
)

## ----include=TRUE, eval=TRUE, echo=TRUE---------------------------------------
results_pm_copd$health_main

## -----------------------------------------------------------------------------
results_pm_copd$health_main |> 
  dplyr::select(exp, bhd, rr, erf_ci, pop_fraction, impact_rounded) |> 
  knitr::kable() # For formatting reasons only: prints tibble in nice layout

## -----------------------------------------------------------------------------
results_noise_ha <- attribute_health(
  approach_risk = "absolute_risk", # default is "relative_risk"
  exp_central = c(57.5, 62.5, 67.5, 72.5, 77.5), # mean of the exposure categories
  pop_exp = c(387500, 286000, 191800, 72200, 7700), # population exposed per exposure category
  erf_eq_central = "78.9270-3.1162*c+0.0342*c^2" # exposure-response function
)

## ----echo=FALSE---------------------------------------------------------------
results_noise_ha |> 
  purrr::pluck("health_main") |>
  dplyr::select(erf_eq, erf_ci, impact_rounded) |> 
  knitr::kable() # Prints tibble in a minimal layout

## ----eval=FALSE, include=TRUE, echo=TRUE--------------------------------------
# results_noise_ha$health_detailed$results_raw

## ----echo=FALSE, include=TRUE, eval=TRUE--------------------------------------
results_noise_ha[["health_detailed"]][["results_raw"]] |> 
  select(exp_category, exp, pop_exp, impact) |> 
  knitr::kable()

## ----eval=FALSE, include=TRUE, echo=TRUE--------------------------------------
# erf_eq_central <-
#   "exp(0.2969*log((pmax(0,c-2.4)/1.9+1))/(1+exp(-(pmax(0,c-2.4)-12)/40.2)))"

## -----------------------------------------------------------------------------
results_noise_ha <- attribute_health(
  approach_risk = "absolute_risk",
  exp_central = 57.5,
  pop_exp = 387500,
  erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_noise_ha$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_noise_ha[["health_main"]] |> 
  dplyr::select(exp_category, impact) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_noise_ha)

## -----------------------------------------------------------------------------
results_iteration <- attribute_health(
    # Names of Swiss cantons
    geo_id_micro = c("Zurich", "Basel", "Geneva", "Ticino", "Jura"),
    # Names of languages spoken in the selected Swiss cantons
    geo_id_macro = c("German","German","French","Italian","French"),
    rr_central = 1.369,
    rr_increment = 10, 
    cutoff_central = 5,
    erf_shape = "log_linear",
    exp_central = c(11, 11, 10, 8, 7),
    bhd_central = c(4000, 2500, 3000, 1500, 500)
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration[["health_main"]] |> 
  dplyr::select(geo_id_macro, impact_rounded, erf_ci, exp_ci, bhd_ci) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration$health_detailed$results_raw

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration[["health_detailed"]][["results_raw"]] |> 
  dplyr::select(geo_id_micro, impact_rounded, geo_id_macro) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_iteration)

## -----------------------------------------------------------------------------
data <- exdat_noise |> 
  ## Filter for urban and rural regions
  dplyr::filter(region == "urban" | region == "rural")

## -----------------------------------------------------------------------------
results_iteration_ar <- attribute_health( 
    # Both the rural and urban areas belong to the higher-level "total" region
    geo_id_macro = "total",
    geo_id_micro = data$region,
    approach_risk = "absolute_risk",
    exp_central = data$exposure_mean,
    pop_exp = data$exposed,
    erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration_ar$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration_ar[["health_main"]] |> 
  dplyr::select(geo_id_macro, impact_rounded, erf_ci, exp_ci) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration_ar$health_detailed$results_by_geo_id_micro

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration_ar[["health_detailed"]][["results_by_geo_id_micro"]] |> 
  dplyr::select(geo_id_micro, geo_id_macro, impact) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_iteration_ar, data)

## -----------------------------------------------------------------------------
results_pm_copd <- attribute_health(
    erf_shape = "log_linear",
    rr_central = 1.369, 
    rr_lower = 1.124, # lower 95% confidence interval (CI) bound of RR
    rr_upper = 1.664, # upper 95% CI bound of RR
    rr_increment = 10, 
    exp_central = 8.85, 
    exp_lower = 8, # lower 95% CI bound of exposure
    exp_upper = 10, # upper 95% CI bound of exposure
    cutoff_central = 5,
    bhd_central = 30747, 
    bhd_lower = 28000, # lower 95% confidence interval estimate of BHD
    bhd_upper = 32000 # upper 95% confidence interval estimate of BHD
) 

## ----eval=FALSE, echo=TRUE, include=FALSE-------------------------------------
# results_pm_copd$health_detailed$results_raw

## ----echo=FALSE---------------------------------------------------------------
results_pm_copd$health_detailed$results_raw |> 
  dplyr::select(erf_ci, exp_ci, bhd_ci, impact_rounded) |> 
  dplyr::slice(1:9) |> 
  knitr::kable() # Prints tibble in a minimal layout

## -----------------------------------------------------------------------------
results_pm_copd_summarized <- 
  summarize_uncertainty(
    output_attribute = results_pm_copd,
    n_sim = 100
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_copd_summarized$uncertainty_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_copd_summarized$uncertainty_main |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_copd_summarized$uncertainty_detailed$impact_by_sim

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_copd_summarized$uncertainty_detailed$impact_by_sim |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_pm_copd_summarized)

## -----------------------------------------------------------------------------
results_pm_copd_mr_brt <- attribute_health(
  exp_central = 8.85,
  bhd_central = 30747,
  cutoff_central = 0,
  # Specify the function based on x-y point pairs that lie on the ERF
  erf_eq_central = splinefun(
    x = c(0, 5, 10, 15, 20, 25, 30, 50, 70, 90, 110),
    y = c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.23, 1.35, 1.45, 1.53, 1.60),
    method = "natural")
)

## ----fig.alt="ERF curve", eval=TRUE, include=TRUE, echo=FALSE-----------------

x <- c(0, 5, 10, 15, 20, 25, 30, 50, 70, 90, 110)
y <- c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.23, 1.35, 1.45, 1.53, 1.60)
spline_func <- 
  stats::splinefun(
    x = x,
    y = y,
    method = "natural")
## Generate finer x-values
x_dense <- seq(min(x), max(x), length.out = 200)
## Evaluate the spline function at these points
y_dense <- spline_func(x_dense)
## Plot
plot(x, 
     y, 
     # main = "User-defined ERF", 
     main = "", 
     xlab = "Exposure",
     ylab = "Relative risk",
     col = "blue", 
     pch = 19)
lines(x_dense, y_dense, col = "red", lwd = 2)
legend("topleft", legend = c("Original points", "Spline curve"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

## -----------------------------------------------------------------------------
results_age_group <- attribute_health(
        approach_risk = "relative_risk",
        age = c("below_65", "65_plus"),
        exp_central = c(8, 7),
        cutoff_central = c(5, 5),
        bhd_central = c(1000, 5000),
        rr_central = 1.06,
        rr_increment = 10,
        erf_shape = "log_linear"
      )

## -----------------------------------------------------------------------------
results_age_group$health_detailed$results_by_age_group |> 
  dplyr::select(age_group, impact_rounded, exp, bhd) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_age_group)

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
results_sex <- attribute_health(
        approach_risk = "relative_risk",
        sex = c("female", "male"),
        exp_central = c(8, 8),
        cutoff_central = c(5, 5),
        bhd_central = c(1000, 1100),
        rr_central = 1.06,
        rr_increment = 10,
        erf_shape = "log_linear"
      )

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
results_sex$health_detailed$results_by_sex |> 
  dplyr::select(sex, impact_rounded, exp, bhd) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_sex)

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
output_attribute <- healthiar::attribute_health(
    rr_central = 1.063,
    rr_increment = 10,
    erf_shape = "log_linear",
    cutoff_central =  0,
    exp_central = c(6, 7, 8,
                    7, 8, 9,
                    8, 9, 10,
                    9, 10, 11),
    bhd_central = c(600, 700, 800,
                    700, 800, 900,
                    800, 900, 1000,
                    900, 1000, 1100),
    geo_id_micro = rep(c("a", "b", "c", "d"), each = 3),
    info = data.frame(
      education = rep(c("secondary", "bachelor", "master"), times = 4)) # education level
  )

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
output_stratified <- output_attribute$health_detailed$results_raw |>
      dplyr::group_by(info_column_1) |>
      dplyr::summarize(mean_impact = mean(impact))|>
      dplyr::pull(mean_impact) |>
      print()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(output_attribute, output_stratified)

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
output_attribute <- healthiar::attribute_health(
    rr_central = 1.063,
    rr_increment = 10,
    erf_shape = "log_linear",
    cutoff_central =  0,
    age_group = base::rep(c("50_and_younger", "50_plus"), each = 4, times= 2),
    sex = base::rep(c("female", "male"), each = 2, times = 4),
    exp_central = c(6, 7, 8, 7, 8, 9, 8, 9,
                    10, 9, 10, 11, 10, 11, 12, 13),
    bhd_central = c(600, 700, 800, 700, 800, 900, 800, 900,
                    1000, 900, 1000, 1100, 1000, 1100, 1200, 1000),
    geo_id_micro = base::rep(c("a", "b"), each = 8),
    info = base::data.frame(
      education = base::rep(c("without_master", "with_master"), times = 8)) # education level
  )

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
output_stratified <- output_attribute$health_detailed$results_raw |>
      dplyr::group_by(info_column_1) |>
      dplyr::summarize(mean_impact = mean(impact))|>
      dplyr::pull(mean_impact) |>
      print()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(output_attribute, output_stratified)

## -----------------------------------------------------------------------------
results_pm_yll <- attribute_lifetable(
  year_of_analysis = 2019, 
  health_outcome = "yll",
  rr_central =  1.118, 
  rr_increment = 10,
  erf_shape = "log_linear",
  exp_central = 8.85,
  cutoff_central = 5,
  min_age = 20, # age from which population is affected by the exposure
  # Life table information
  age_group = exdat_lifetable$age_group,
  sex = exdat_lifetable$sex,
  population = exdat_lifetable$midyear_population,
  # In the life table case, BHD refers to deaths
  bhd_central = exdat_lifetable$deaths
) 

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_yll$health_main |> 
  dplyr::slice_head() |> 
  dplyr::select(impact_rounded, erf_ci, exp_ci, bhd_ci) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results_pm_yll$health_detailed$results_raw |>
  dplyr::summarize(
    .by = year, 
    impact = sum(impact, na.rm = TRUE)
  )

## -----------------------------------------------------------------------------
results_pm_yll$health_detailed$results_raw |>
  dplyr::summarize(
    .by = year, 
    impact = sum(impact, na.rm = TRUE)) |>
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_detailed$intermediate_calculations |>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(impact_by_age_and_year) |>
#   purrr::pluck(1)

## ----echo=FALSE---------------------------------------------------------------
results_pm_yll$health_detailed$intermediate_calculations |> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(impact_by_age_and_year) |>
  purrr::pluck(1) |>
  dplyr::select(age_start, age_end, impact_2019) |> 
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_detailed$intermediate_calculations|>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(projection_if_exposed_by_age_and_year) |>
#   purrr::pluck(1)

## ----echo=FALSE---------------------------------------------------------------
results_pm_yll$health_detailed$intermediate_calculations|> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(projection_if_exposed_by_age_and_year) |>
  purrr::pluck(1) |>   
  dplyr::select(age_start, midyear_population_2019, midyear_population_2020, 
                midyear_population_2021, midyear_population_2022) |>    
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |>  
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_detailed$intermediate_calculations|>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(projection_if_unexposed_by_age_and_year) |>
#   purrr::pluck(1)

## ----echo=FALSE---------------------------------------------------------------
results_pm_yll$health_detailed$intermediate_calculations|> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(projection_if_unexposed_by_age_and_year) |>
  purrr::pluck(1) |>   
  dplyr::select(age_start, midyear_population_2019, midyear_population_2020, 
                midyear_population_2021, midyear_population_2022) |>    
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |>  
  knitr::kable()

## -----------------------------------------------------------------------------
results_pm_deaths <- attribute_lifetable(
  health_outcome = "deaths",
  year_of_analysis = 2019,
  rr_central =  1.118, 
  rr_increment = 10,
  erf_shape = "log_linear",
  exp_central = 8.85,
  cutoff_central = 5,
  min_age = 20, # age from which population is affected by the exposure   
  # Life table information
  age_group = exdat_lifetable$age_group,   
  sex = exdat_lifetable$sex,
  population = exdat_lifetable$midyear_population, 
  bhd_central = exdat_lifetable$deaths
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths$health_main$impact

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_deaths$health_main |> 
  dplyr::slice_head() |> 
  dplyr::select(impact_rounded, erf_ci, exp_ci, bhd_ci) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths$health_detailed$intermediate_calculations|>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(projection_if_unexposed_by_age_and_year) |>
#   purrr::pluck(1)

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_yll$health_detailed$intermediate_calculations|> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(projection_if_unexposed_by_age_and_year) |>
  purrr::pluck(1) |>
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |>  
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_pm_deaths)

## -----------------------------------------------------------------------------
results_pm_copd_yld  <- attribute_health(
  rr_central = 1.1, 
  rr_increment = 10, 
  erf_shape = "log_linear",  
  exp_central = 8.85,
  cutoff_central = 5,
  bhd_central = 1000,
  duration_central = 10,
  dw_central = 0.2
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_copd_yld$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_copd_yld$health_main |> 
  dplyr::select(erf_ci, impact) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results_daly <- daly(
     output_attribute_yll = results_pm_yll,
     output_attribute_yld = results_pm_copd_yld
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_daly$health_main |>
#   select(impact_yll_rounded, impact_yld_rounded, impact_rounded)

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_daly$health_main |> 
  dplyr::select(impact_yll_rounded, impact_yld_rounded, impact_rounded) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_daly, results_pm_yll, results_pm_copd_yld)

## -----------------------------------------------------------------------------
scenario_A <- attribute_health(
    exp_central = 8.85,   # EXPOSURE 1
    cutoff_central = 5, 
    bhd_central = 25000,
    approach_risk = "relative_risk",
    erf_shape = "log_linear",
    rr_central = 1.118,
    rr_increment = 10)

## -----------------------------------------------------------------------------
scenario_B <- attribute_mod(
  output_attribute = scenario_A, 
  exp_central = 6
)

## -----------------------------------------------------------------------------
scenario_B <- attribute_health(
    exp_central = 6,     # EXPOSURE 2
    cutoff_central = 5, 
    bhd_central = 25000,
    approach_risk = "relative_risk",
    erf_shape = "log_linear",
    rr_central = 1.118,
    rr_increment = 10)

## -----------------------------------------------------------------------------
scenario_A <- attribute_health(
    exp_central = 8.85,   # EXPOSURE 1
    cutoff_central = 5, 
    bhd_central = 25000,
    approach_risk = "relative_risk",
    erf_shape = "log_linear",
    rr_central = 1.118,
    rr_increment = 10)

## -----------------------------------------------------------------------------
scenario_B <- attribute_mod(
  output_attribute = scenario_A, 
  exp_central = 6
)

## -----------------------------------------------------------------------------

results_comparison <- healthiar::compare(
  approach_comparison = "delta", # or "pif" (population impact fraction)
  output_attribute_scen_1 = scenario_A,
  output_attribute_scen_2 = scenario_B
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_comparison$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_comparison[["health_main"]] |> 
  dplyr::select(
    impact, impact_rounded,
    impact_scen_1, impact_scen_2,
    bhd,
    dplyr::starts_with("exp_"),
    -dplyr::starts_with("exp_ci"), # remove col "exp_ci"
    dplyr::starts_with("rr_con")) |> 
  dplyr::slice_head() |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_comparison, scenario_A, scenario_B)

## -----------------------------------------------------------------------------
results_pm <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.369, 
  rr_increment = 10,
  exp_central = 8.85,
  cutoff_central = 5,
  bhd_central = 30747
) 

results_no2 <- attribute_mod(
  output_attribute = results_pm,
  exp_central = 10.9,
  rr_central = 1.031
)

results_multiplicative <- multiexpose(
  output_attribute_exp_1 = results_pm,
  output_attribute_exp_2 = results_no2,
  exp_name_1 = "pm2.5",
  exp_name_2 = "no2",
  approach_multiexposure = "multiplicative"
)

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_no2, results_pm)

## ----eval=FALSE, include=TRUE, echo=TRUE--------------------------------------
# results_multiplicative$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_multiplicative$health_main |> 
  dplyr::select(impact_rounded) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_multiplicative)

## -----------------------------------------------------------------------------
output_attribute <- attribute_health(
  rr_central = 1.063,
  rr_increment = 10,
  erf_shape = "log_linear",
  cutoff_central =  0,
  age_group = c("below_40", "above_40"),
  exp_central = c(8.1, 10.9),
  bhd_central = c(1000, 4000),
  population = c(100000, 500000)
  )

results <- standardize(
  output_attribute = output_attribute,
  age_group = c("below_40", "above_40"),
  ref_prop_pop = c(0.5, 0.5)
  )



## -----------------------------------------------------------------------------
print(results$health_main$impact_per_100k_inhab)  

## -----------------------------------------------------------------------------
print(results$health_detailed$results_raw$impact_per_100k_inhab)  

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results)

## -----------------------------------------------------------------------------
# exdat_pwm_1 = Pollution grid data
exdat_pwm_1 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))

# exdat_pwm_2 = Data with the geo units and population data. This is pre-loaded in healthiar.
# If your raw data are in .gpkg format, you can use e.g.  sf::st_read() 

pwm <- healthiar::prepare_exposure(
  poll_grid = exdat_pwm_1, # Formal class SpatRaster,
  geo_units = exdat_pwm_2, # sf of the geographic sub-units
  population = sf::st_drop_geometry(exdat_pwm_2$population), # population per geographic sub-unit
  geo_id_macro = sf::st_drop_geometry(exdat_pwm_2$region)) # higher-level IDs to aggregate

## ----echo=FALSE---------------------------------------------------------------
 knitr::kable(pwm$main)

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(exdat_pwm_1)
rm(exdat_pwm_2)
rm(pwm)

## -----------------------------------------------------------------------------
#setting up function parameters
threshold_effect <- 45
RR <- 1.055
threshold_calculation <- 55
rr_increment <- 10

# define categorical function, the ifelse condition enables the case distinction
erf_function <- function(c){
  output <- ifelse(c<threshold_calculation, 1, exp((log(RR)/rr_increment)*(c-threshold_effect)))
  return(output)
}
# attribute_health
results_catERF_different_calc_thesh <- healthiar::attribute_health(
  approach_risk = "relative_risk",
  erf_eq_central = erf_function,
  prop_pop_exp = c(300000,200000,150000,120000,100000,70000,60000)/10000000,
  exp_central = c(47,52,57,62,67,72,77),
  cutoff_central=0,
  bhd_central=50000)$health_main$impact_rounded

## ----fig.alt="ERF curve", eval=TRUE, include=TRUE, echo=FALSE-----------------

# Evaluate exp_central points
x <- c(47,52,57,62,67,72,77) # central exposure values
y <- erf_function(x)

# Generate finer x-values
x_dense <- seq(min(x), max(x), length.out = 200)
# Evaluate exp_central points (finer version)
y_dense <- erf_function(x_dense)

## Plot
plot(x,
     y # main = "User-defined ERF"
     ,
     main = "",
     xlab = "Exposure",
     ylab = "Relative risk",
     col = "blue",
     pch = 19)

lines(x_dense,y_dense,col = "red",lwd = 2)

legend("topleft",
       legend = c("Original points", "Categorical ERF curve"),
       col = c("blue", "red"),
       pch = c(19, NA),
       lty = c(NA, 1),
       lwd = c(NA, 2))


## -----------------------------------------------------------------------------
monetized_pm_copd <- monetize(
    output_attribute = results_pm_copd,
    discount_shape = "exponential",
    discount_rate = 0.03,
    n_years = 5,
    valuation = 50000 # E.g. EURO
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# monetized_pm_copd$monetization_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
monetized_pm_copd$monetization_main |> 
  select(erf_ci, monetized_impact) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results <- monetize(
  impact = 1151,
  valuation = 100
)

## -----------------------------------------------------------------------------
cba <- cba(
    output_attribute = results_pm_copd,
    valuation = 50000,
    cost = 100000000,
    discount_shape = "exponential",
    discount_rate_benefit = 0.03,
    discount_rate_cost = 0.03,
    n_years_benefit = 5,
    n_years_cost = 5
)

## -----------------------------------------------------------------------------
cba$cba_main |>  
  dplyr::select(benefit, cost, net_benefit) |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(cba)

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
 health_impact <- healthiar::attribute_health(
   age_group = exdat_socialize$age_group,
   exp_central = exdat_socialize$pm25_mean,
   cutoff_central = 0,
   rr_central = exdat_socialize$rr,
   erf_shape = "log_linear",
   rr_increment = 10,
   bhd_central = exdat_socialize$mortality,
   population = exdat_socialize$population,
   geo_id_micro = exdat_socialize$geo_unit)

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
social_t <- healthiar::socialize(
  output_attribute = health_impact,
  age_group = exdat_socialize$age_group, # They have to be the same in socialize() and in attribute_health()
  ref_prop_pop = exdat_socialize$ref_prop_pop, # Population already provided in output_attribute
  geo_id_micro = exdat_socialize$geo_unit,
  social_indicator = exdat_socialize$score,
  n_quantile = 10,
  increasing_deprivation = TRUE)


## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
social <- healthiar::socialize(
  impact = health_impact$health_detailed$results_by_age_group$impact,
  age_group = exdat_socialize$age_group, # They have to be the same in socialize() and in attribute_health()
  ref_prop_pop = exdat_socialize$ref_prop_pop,
  geo_id_micro = exdat_socialize$geo_unit,
  social_indicator = exdat_socialize$score,
  population = exdat_socialize$population, # Population has to be provided because no output_attribute
  n_quantile = 10,
  increasing_deprivation = TRUE)


## ----echo=FALSE---------------------------------------------------------------
social$social_main |> 
  dplyr::select(c(parameter, difference_type, 
                  difference_compared_with, difference_value, 
                  comment)) |>
  print()

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
mdi <- prepare_mdi(
  geo_id_micro = exdat_prepare_mdi$id,
  edu = exdat_prepare_mdi$edu,
  unemployed = exdat_prepare_mdi$unemployed,
  single_parent = exdat_prepare_mdi$single_parent,
  pop_change = exdat_prepare_mdi$pop_change,
  no_heating = exdat_prepare_mdi$no_heating,
  n_quantile = 10,
  verbose = FALSE
)

## ----eval=FALSE, include=TRUE, echo=TRUE--------------------------------------
# mdi$mdi_main |>
#   select(geo_id_micro, MDI, MDI_index)

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
mdi$mdi_main |> 
  dplyr::select(geo_id_micro, MDI, MDI_index) |> 
  knitr::kable()

## ----fig.alt="Boxplot of Normalized Indicators and MDI", eval=TRUE, include=TRUE, echo=TRUE----
eval(mdi$mdi_detailed$boxplot)

## ----fig.alt="Histogram of MDI with normal curve", eval=TRUE, include=TRUE, echo=TRUE----
eval(mdi$mdi_detailed$histogram)

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(mdi)

## ----eval=FALSE, include=TRUE, echo = TRUE------------------------------------
# exdat_noise |>
#   (\(df) {
#     healthiar::attribute_health(
#       approach_risk = df$risk_estimate_type,
#       exp_central = df$exposure_mean,
#       pop_exp = df$exposed,
#       erf_eq_central = df$erf
#       )$health_main$impact_rounded
#     })()

## -----------------------------------------------------------------------------
exdat_noise |>
      (\(df) {
        with(df, healthiar::attribute_health(
         approach_risk = risk_estimate_type,
         exp_central = exposure_mean,
         pop_exp = exposed,
         erf_eq_central = erf
         ))$health_main$impact_rounded
        })()

## ----eval=FALSE, include=TRUE, echo = TRUE------------------------------------
# exdat_noise %>%
#   {
#     healthiar::attribute_health(
#       approach_risk = .$risk_estimate_type,
#       exp_central = .$exposure_mean,
#       pop_exp = .$exposed,
#       erf_eq_central = .$erf
#     )$health_main$impact_rounded
#   }

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# write.csv(x = results_pm_copd$health_main, file = "exported_results/results_pm_copd.csv")

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# save(results_pm_copd, file = "exported_results/results_pm_copd.Rdata")

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# openxlsx::write.xlsx(x = results_pm_copd$health_main, file = "exported_results/results_pm_copd.xlsx")

