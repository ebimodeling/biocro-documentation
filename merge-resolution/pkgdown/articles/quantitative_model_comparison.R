## ----solve_latex_color_problems,include=FALSE,eval=TRUE-----------------------

## Versions of knitr before 1.39 used the color package, but we want
## to use xcolor instead (together with the "dvipsnames" option), so
## we use a hook to do a substitution.  (This hook is a no-op for
## knitr version 1.39, assuming the color package is not explicitly
## used by the Rnw file.)

knitr::knit_hooks$set(document = function(x) {
  sub('\\usepackage[]{color}', '\\usepackage[dvipsnames]{xcolor}', x, fixed = TRUE)
})

## Version 1.39 of knitr *does* use the xcolor package, but we still
## have to set it to use the dvipsnames option.  (This option setting
## is a no-op for knitr versions before 1.39, assuming the xcolor
## package is not explicitly used by the Rnw file.)

knitr::opts_knit$set(latex.options.xcolor = 'dvipsnames')


## ----preliminaries,echo=FALSE,error=TRUE--------------------------------------
knitr::opts_chunk$set(error=TRUE) # don't stop on errors; display them
                                  # in results; this is the default;
                                  # we override this below when
                                  # loading needed packages

## ----loading_libraries,echo=FALSE,error=FALSE---------------------------------
library(BioCro, quietly=TRUE)
library(lattice, quietly=TRUE)

## ----version_info,echo=FALSE,comment=''---------------------------------------
# Show the current commit hash and the date of that commit.
cat(
  paste0(
    system2('git',
            args = c('show',
                     '-s',
                     '--format="This document was generated from the version of BioCro specified as follows:%n%nCommit Hash: %h%nDate: %aD"'
                    ),
            stdout = TRUE
    ),
    sep = '',
    collapse = '\n'
  ),
  "\nBranch:",
  system2('git',
          args = c('branch',
                   '--show-current'),
          stdout = TRUE
  ),
  "\n"
)

## ----plotting_tools,echo=FALSE------------------------------------------------
time_range <- c(142, 298)
biomass_range <- c(-0.5, 6.5)
ppfd_range <- c(-100, 1200)
assim_range <- c(-2, 32)
biomass_columns <- c('time', 'Grain', 'Leaf', 'Root', 'Stem')

## ----helping_functions,echo=FALSE---------------------------------------------
# Define a list of atmospheric CO2 values for each year
catm <- catm_data$Catm
names(catm) <- catm_data$year

# Define a function that runs the clock modules to determine the photoperiod
# length during a year's worth of weather data, adding it to the data so it can
# be used as a driver in future simulations
add_photoperiod_length <- function(weather_data) {
    clock_output <- with(soybean_clock, {run_biocro(
        initial_values,
        parameters,
        weather_data,
        direct_modules,
        differential_modules,
        ode_solver
    )})
    weather_data[['day_length']] <- clock_output[['day_length']]
    return(weather_data)
}

# Define a helping function that runs the circadian clock model for the entire
# year and then truncates the data to the appropriate time range
process_weather <- function(weather_data) {
    weather_data <- add_photoperiod_length(weather_data)
    weather_data <-
        weather_data[weather_data[['doy']] >= 152 &
          weather_data[['doy']] <= 288,]
}

# Define a list of processed weather data
soy_weather <- lapply(weather, process_weather)

# Define a function to help save PDFs of the figures. Here the important part
# is setting `useDingbats` to FALSE, since dingbats causes problems when opening
# PDFs in editing software such as Adobe Illustrator.
pdf_print <- function(
    plot_object,
    file_name_string,
    width = 6,
    height = 6
)
{
   pdf(
       file = file_name_string,
       width = width,
       height = height,
       useDingbats = FALSE
   )
   print(plot_object)
   dev.off()
}

## ----fvcb_result_2002---------------------------------------------------------
cmi_soybean <- within(soybean, {
  direct_modules = append(direct_modules, 'BioCro:total_biomass')
})

fvcb_result_2002 <- with(cmi_soybean, {run_biocro(
  initial_values,
  parameters,
  soy_weather[['2002']],
  direct_modules,
  differential_modules,
  ode_solver
)})

final_biomass <- function(df) {
  df[nrow(df), 'total_biomass']
}

final_biomass_fvcb_2002 <- final_biomass(fvcb_result_2002)

## ----rue_2002-----------------------------------------------------------------
# The first six arguments are the same as for `run_biocro`
rue_2002 <- with(cmi_soybean, {partial_run_biocro(
  initial_values,
  within(parameters, {alpha_rue = NA}),
  soy_weather[['2002']],
  within(direct_modules, {canopy_photosynthesis = 'BioCro:ten_layer_rue_canopy'}),
  differential_modules,
  ode_solver,
  'alpha_rue'  # here we specify the names of any quantities whose values
)})            # should not be fixed

## ----rue_fvcb_square_difference-----------------------------------------------
rue_fvcb_square_difference = function(alpha_rue) {
    (final_biomass(rue_2002(alpha_rue)) - final_biomass_fvcb_2002)^2
}

## ----alpha_rue_optimization---------------------------------------------------
min_alpha <- 0.021
max_alpha <- 0.031
opt_par = optim(
    0.03,
    rue_fvcb_square_difference,
    method='Brent',
    lower=min_alpha,
    upper=max_alpha
)

best_alpha_rue = opt_par$par

## ----figure_s1,echo=FALSE,results=FALSE---------------------------------------
alpha_rue_sequence = seq(min_alpha, max_alpha, by = 2.5e-4)
differences = sapply(alpha_rue_sequence, rue_fvcb_square_difference)

alpha_rue_optimization_plot <- xyplot(
    differences ~ alpha_rue_sequence,
    type = 'l',
    grid = TRUE,
    auto = TRUE,
    xlab = 'alpha_rue (C per photon)',
    ylab = '(RUE biomass - FvCB biomass)^2 at end of season',
    main = paste0('Year 2002: best_alpha_rue = ', best_alpha_rue),
    panel = function(...) {
        panel.xyplot(...)
        panel.points(
            rue_fvcb_square_difference(best_alpha_rue) ~ best_alpha_rue,
            type = 'p',
            col = 'red',
            pch = 16
        )
    }
)

pdf_print(alpha_rue_optimization_plot, 'alpha_rue_optimization_plot.pdf')

## ----optimal_rue_result_2002--------------------------------------------------
optimal_rue_result_2002 <- with(cmi_soybean, {run_biocro(
  initial_values,
  within(parameters, {alpha_rue = best_alpha_rue}),
  soy_weather[['2002']],
  within(direct_modules, {canopy_photosynthesis = 'BioCro:ten_layer_rue_canopy'}),
  differential_modules,
  ode_solver
)})

## ----figure_4a,echo=FALSE,results=FALSE---------------------------------------
biomass_comparison_2002 <- rbind(
    within(fvcb_result_2002[biomass_columns], {model = 'FvCB'}),
    within(optimal_rue_result_2002[biomass_columns], {model = 'RUE'})
)

biomass_comparison_2002_plot <- xyplot(
    Leaf + Stem + Root + Grain ~ time,
    group = model,
    data = biomass_comparison_2002,
    type = 'l',
    auto = TRUE,
    grid = TRUE,
    xlim = time_range,
    ylim = biomass_range,
    xlab = 'Day of year (2002)',
    ylab = 'Biomass (Mg / ha)',
)

pdf_print(biomass_comparison_2002_plot, 'biomass_comparison_2002_plot.pdf')

## ----extract_aq_scatter-------------------------------------------------------
extract_aq_scatter <- function(biocro_output) {
  light_column_names <- grep(
    '(sunlit|shaded)_incident_ppfd_layer_[0-9]',
    names(biocro_output),
    value = TRUE
  )

  assim_column_names <- grep(
    '(sunlit|shaded)_GrossAssim_layer_[0-9]',
    names(biocro_output),
    value=TRUE
  )

  aq_scatter <- data.frame(
    incident_ppfd = unlist(biocro_output[light_column_names]),
    gross_assimilation = unlist(biocro_output[assim_column_names]),
    row.names = NULL
  )

  return(aq_scatter)
}

## ----figure_4b,echo=FALSE,results=FALSE---------------------------------------
fvcb_aq_scatter_2002 <- extract_aq_scatter(fvcb_result_2002)
rue_aq_scatter_2002 <- extract_aq_scatter(optimal_rue_result_2002)

# Plot a fraction of the points, chosen at equally-spaced intervals. To plot all
# the points, set `frac_to_plot` to 1.
frac_to_plot <- 0.06
points_to_plot <- seq(
  from = 1,
  to = nrow(fvcb_aq_scatter_2002),
  length.out = frac_to_plot * nrow(fvcb_aq_scatter_2002)
)

fvcb_aq_scatter_plot <- xyplot(
    gross_assimilation ~ incident_ppfd,
    data = fvcb_aq_scatter_2002[points_to_plot,],
    type = 'p',
    pch = 16,
    xlim = ppfd_range,
    ylim = assim_range,
    xlab = 'Incident PPFD (micromol / m^2 / s)',
    ylab = 'Gross CO2 assimilation rate (micromol / m^2 / s)',
    grid = TRUE,
    main = '(Ag, Q) scatter plot from the FvCB model in 2002'
)

pdf_print(fvcb_aq_scatter_plot, 'fvcb_aq_scatter_plot.pdf')

rue_aq_scatter_plot <- xyplot(
    gross_assimilation ~ incident_ppfd,
    data = rue_aq_scatter_2002[points_to_plot,],
    type = 'p',
    pch = 16,
    xlim = ppfd_range,
    ylim = assim_range,
    xlab = 'Incident PPFD (micromol / m^2 / s)',
    ylab = 'Gross CO2 assimilation rate (micromol / m^2 / s)',
    grid = TRUE,
    main = '(Ag, Q) scatter plot from the RUE model in 2002'
)

pdf_print(rue_aq_scatter_plot, 'rue_aq_scatter_plot.pdf')

## ----fvcb_light_curve_inputs--------------------------------------------------
# Choose a set of incident PPFD values to use (micromol / m^2 / s)
incident_ppfd <- seq(0, 1000, length.out = 501)

# Determine corresponding absorbed PPFD values (micromol / m^2 / s) using the
# soybean leaf reflectance and transmittance
absorbed_ppfd <- incident_ppfd *
    (1 - cmi_soybean$parameters$leaf_reflectance -
        cmi_soybean$parameters$leaf_transmittance)

# Determine corresponding incident PAR values (J / m^2 / s) using the average
# energy per micromole of photosynthetically active photons in sunlight
incident_par <- incident_ppfd * cmi_soybean$parameters$par_energy_content

# Determine the corresponding incident shorwave values using the fraction of
# solar energy that lies in the PAR band (J / m^2 / s)
incident_shortwave <- incident_par / cmi_soybean$parameters$par_energy_fraction

# Determine the corresponding absorbed shortwave energy values using the
# shortwave reflectance and transmittance of the leaf (J / m^2 / s)
average_absorbed_shortwave <-
    incident_shortwave *
    (1 - cmi_soybean$parameters$leaf_reflectance -
        cmi_soybean$parameters$leaf_transmittance) /
    (1 - cmi_soybean$parameters$leaf_transmittance)

# Make a data frame with the incident PPFD and absorbed shortwave values, where
# we also include values of a few other required parameters
light_curve_inputs <- data.frame(
    incident_ppfd = incident_ppfd,
    absorbed_ppfd = absorbed_ppfd,
    average_absorbed_shortwave = average_absorbed_shortwave,
    rh = 0.75,
    temp = 25,
    windspeed = 3.28,
    Catm = catm[['2002']],
    StomataWS = 0.99,
    height = 0.75
)

## ----assim_sensitivity--------------------------------------------------------
assim_sensitivity <- function(
    varname,
    base_inputs,
    relative_perturbation = 1e-6
)
{
  module <- 'BioCro:c3_leaf_photosynthesis'

  var_center <- base_inputs[[varname]]
  gross_assim_center <- evaluate_module(module, base_inputs)$GrossAssim

  neg_inputs <- base_inputs
  neg_var <- base_inputs[[varname]] * (1 - relative_perturbation)
  neg_inputs[[varname]] <- neg_var
  gross_assim_neg <- evaluate_module(module, neg_inputs)$GrossAssim

  pos_inputs <- base_inputs
  pos_var <- base_inputs[[varname]] * (1 + relative_perturbation)
  pos_inputs[[varname]] <- pos_var
  gross_assim_pos <- evaluate_module(module, pos_inputs)$GrossAssim

  dadx = (gross_assim_pos - gross_assim_neg) / (pos_var - neg_var)
  return(dadx / (gross_assim_center / var_center))
}

## ----fvcb_assim_sensitivity---------------------------------------------------
fvcb_light_curve_sensitivity_variables <-
  c('Catm', 'rh', 'temp', 'StomataWS', 'windspeed')

fvcb_sensitivity_light_curve_result <- data.frame(
  incident_ppfd = light_curve_inputs[['incident_ppfd']]
)

# For each variable of interest, calculate sensitivity at each of the light
# intensities in `light_curve_inputs`
for (varname in fvcb_light_curve_sensitivity_variables) {
  fvcb_sensitivity_light_curve_result[[varname]] <-
    apply(
      light_curve_inputs,
      1,
      function(x) {assim_sensitivity(
        varname,
        c(within(cmi_soybean$parameters, {rm(Catm)}), as.list(x))
      )}
    )
}

## ----figure_5a,echo=FALSE,results=FALSE---------------------------------------
# Create Figure 5a
fvcb_light_curve_sensitivity_plot <- xyplot(
  Catm + rh + temp + StomataWS + windspeed ~ incident_ppfd,
  data = fvcb_sensitivity_light_curve_result,
  type = 'l',
  auto = TRUE,
  grid = TRUE,
  xlab = 'Incident PPFD (micromol / m^2 / s)',
  ylab = 'Normalized sensitivity coefficient',
  xlim = c(-100, 1100),
  ylim = c(-1, 1)
)

pdf_print(fvcb_light_curve_sensitivity_plot, 'fvcb_light_curve_sensitivity_plot.pdf')

## ----biomass_driver_sensitivity-----------------------------------------------
biomass_driver_sensitivity <- function(
    varname,
    sens_parameters,
    canopy_photosynthesis_module,
    relative_perturbation = 1e-5
)
{
  c_to_k <- 273.15

  default_drivers <- within(soy_weather[['2002']], {temp = temp + c_to_k})

  default_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    sens_parameters,
    within(default_drivers, {temp = temp - c_to_k}),
    within(direct_modules, {canopy_photosynthesis = canopy_photosynthesis_module}),
    differential_modules,
    ode_solver
  )})

  neg_drivers <- default_drivers
  neg_drivers[[varname]] <- default_drivers[[varname]] * (1 - relative_perturbation)
  neg_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    sens_parameters,
    within(neg_drivers, {temp = temp - c_to_k}),
    within(direct_modules, {canopy_photosynthesis = canopy_photosynthesis_module}),
    differential_modules,
    ode_solver
  )})

  pos_drivers <- default_drivers
  pos_drivers[[varname]] <- default_drivers[[varname]] * (1 + relative_perturbation)
  pos_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    sens_parameters,
    within(pos_drivers, {temp = temp - c_to_k}),
    within(direct_modules, {canopy_photosynthesis = canopy_photosynthesis_module}),
    differential_modules,
    ode_solver
  )})

  dMdx <-
    (pos_result[['total_biomass']] - neg_result[['total_biomass']]) /
    (pos_drivers[[varname]] - neg_drivers[[varname]])

  normalized_sensitivity <-
    dMdx / (default_result[['total_biomass']] / default_drivers[[varname]])

  return(
    data.frame(
      normalized_sensitivity = normalized_sensitivity,
      time = default_result[['time']]
    )
  )
}

## ----biomass_temp_sensitivity-------------------------------------------------
biomass_temp_sensitivity_fvcb <- biomass_driver_sensitivity(
  'temp',
  cmi_soybean$parameters,
  'BioCro:ten_layer_c3_canopy'
)

biomass_temp_sensitivity_rue <- biomass_driver_sensitivity(
  'temp',
  within(cmi_soybean$parameters, {alpha_rue = best_alpha_rue}),
  'BioCro:ten_layer_rue_canopy'
)

## ----figure_5b,echo=FALSE,results=FALSE---------------------------------------
# Combine the data frames
biomass_temp_sensitivity <- rbind(
  within(biomass_temp_sensitivity_fvcb, {model = 'FvCB'}),
  within(biomass_temp_sensitivity_rue, {model = 'RUE'})
)

# Create Figure 5b
biomass_temp_sensitivity_plot <- xyplot(
  normalized_sensitivity ~ time,
  group = model,
  data = biomass_temp_sensitivity,
  type = 'l',
  auto = TRUE,
  grid = TRUE,
  xlim = time_range,
  ylim = c(-20, 35),
  xlab = 'Day of year (2002)',
  ylab = 'dM / dT / (M / T)'
)

pdf_print(biomass_temp_sensitivity_plot, 'biomass_temp_sensitivity_plot.pdf')

## ----biomass_parameter_sensitivity--------------------------------------------
biomass_parameter_sensitivity <- function(
    varname,
    sens_parameters,
    canopy_photosynthesis_module,
    relative_perturbation = 1e-6
)
{
  default_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    sens_parameters,
    soy_weather[['2002']],
    within(direct_modules, {canopy_photosynthesis = canopy_photosynthesis_module}),
    differential_modules,
    ode_solver
  )})

  neg_parameters <- sens_parameters
  neg_parameters[[varname]] <- sens_parameters[[varname]] * (1 - relative_perturbation)
  neg_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    neg_parameters,
    soy_weather[['2002']],
    within(direct_modules, {canopy_photosynthesis = canopy_photosynthesis_module}),
    differential_modules,
    ode_solver
  )})

  pos_parameters <- sens_parameters
  pos_parameters[[varname]] <- sens_parameters[[varname]] * (1 + relative_perturbation)
  pos_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    pos_parameters,
    soy_weather[['2002']],
    within(direct_modules, {canopy_photosynthesis = canopy_photosynthesis_module}),
    differential_modules,
    ode_solver
  )})

  dMdx <-
    (pos_result[['total_biomass']] - neg_result[['total_biomass']]) /
    (pos_parameters[[varname]] - neg_parameters[[varname]])

  normalized_sensitivity <-
    dMdx / (default_result[['total_biomass']] / sens_parameters[[varname]])

  return(
    data.frame(
      normalized_sensitivity = normalized_sensitivity,
      time = default_result[['time']]
    )
  )
}

## ----biomass_catm_sensitivity-------------------------------------------------
biomass_catm_sensitivity_fvcb <- biomass_parameter_sensitivity(
  'Catm',
  cmi_soybean$parameters,
  'BioCro:ten_layer_c3_canopy'
)

biomass_catm_sensitivity_rue <- biomass_parameter_sensitivity(
  'Catm',
  within(cmi_soybean$parameters, {alpha_rue = best_alpha_rue}),
  'BioCro:ten_layer_rue_canopy'
)

## ----figure_5c,echo=FALSE,results=FALSE---------------------------------------
# Combine the data frames
biomass_catm_sensitivity <- rbind(
  within(biomass_catm_sensitivity_fvcb, {model = 'FvCB'}),
  within(biomass_catm_sensitivity_rue, {model = 'RUE'})
)

# Create Figure 5c
biomass_catm_sensitivity_plot <- xyplot(
  normalized_sensitivity ~ time,
  group = model,
  data = biomass_catm_sensitivity,
  type = 'l',
  auto = TRUE,
  grid = TRUE,
  xlim = time_range,
  ylim = c(-60, 60),
  xlab = 'Day of year (2002)',
  ylab = 'dM / dCa / (M / Ca)'
)

pdf_print(biomass_catm_sensitivity_plot, 'biomass_catm_sensitivity_plot.pdf')

## ----biomass_comparison_2006--------------------------------------------------
fvcb_result_2006 <- with(cmi_soybean, {run_biocro(
  initial_values,
  within(parameters, {Catm = catm[['2006']]}),
  soy_weather[['2006']],
  direct_modules,
  differential_modules,
  ode_solver
)})

# Run the RUE model with the optimal value for alpha_rue determined for 2002
rue_result_2006 <- with(cmi_soybean, {run_biocro(
  initial_values,
  within(parameters, {alpha_rue = best_alpha_rue; Catm = catm[['2006']]}),
  soy_weather[['2006']],
  within(direct_modules, {canopy_photosynthesis = 'BioCro:ten_layer_rue_canopy'}),
  differential_modules,
  ode_solver
)})

## ----figure_6a,echo=FALSE,results=FALSE---------------------------------------
# Combine the RUE and FvCB results into one data frame for plotting
biomass_columns <- c('time', 'Leaf', 'Stem', 'Root', 'Grain')
biomass_comparison_2006 <- rbind(
  within(fvcb_result_2006[biomass_columns], {model = 'FvCB'}),
  within(rue_result_2006[biomass_columns], {model = 'RUE'})
)

# Make Figure 6a
biomass_comparison_2006_plot <- xyplot(
    Leaf + Stem + Root + Grain ~ time,
    group = model,
    data = biomass_comparison_2006,
    type = 'l',
    auto = TRUE,
    grid = TRUE,
    xlim = time_range,
    ylim = biomass_range,
    xlab = 'Day of year (2006)',
    ylab = 'Biomass (Mg / ha)',
)

pdf_print(biomass_comparison_2006_plot, 'biomass_comparison_2006_plot.pdf')

## ----multiyear_comparison-----------------------------------------------------
# Decide which years to use
years <- as.character(seq(1995, 2020))

# Initialize vectors to store final biomass and atmospheric CO2 values
final_biomass_seq_rue <- numeric(length(years))
final_biomass_seq_fvcb <- numeric(length(years))
catm_seq <- numeric(length(years))

# Get final biomass values for each year in each model
for (i in seq_along(years)) {
  # Run the RUE soybean model for this year, ensuring that we're using the
  # correct value for the atmospheric CO2 concentration
  rue_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    within(parameters, {alpha_rue = best_alpha_rue; Catm = catm[[years[i]]]}),
    soy_weather[[years[i]]],
    within(direct_modules, {canopy_photosynthesis = 'BioCro:ten_layer_rue_canopy'}),
    differential_modules,
    ode_solver
  )})

  # Run the FvCB soybean model for this year, ensuring that we're using the
  # correct value for the atmospheric CO2 concentration
  fvcb_result <- with(cmi_soybean, {run_biocro(
    initial_values,
    within(parameters, {Catm = catm[[years[i]]]}),
    soy_weather[[years[i]]],
    direct_modules,
    differential_modules,
    ode_solver
  )})

  # Store the final biomass and atmospheric CO2 values
  final_biomass_seq_rue[i] <- final_biomass(rue_result)
  final_biomass_seq_fvcb[i] <- final_biomass(fvcb_result)
  catm_seq[i] <- catm[[years[i]]]
}

## ----figure_6bc,echo=FALSE,results=FALSE--------------------------------------
# Form a data frame for plotting and calculate a few new columns
multiyear_comparison <- data.frame(
  catm = catm_seq,
  year = as.numeric(years),
  final_biomass_rue = final_biomass_seq_rue,
  final_biomass_fvcb = final_biomass_seq_fvcb
)

multiyear_comparison <- within(multiyear_comparison, {
  final_mass_difference = final_biomass_rue - final_biomass_fvcb
  final_mass_diff_percent = final_mass_difference / final_biomass_fvcb * 100
})

# Make Figure 6b
multiyear_biomass_plot <- xyplot(
  final_biomass_rue + final_biomass_fvcb ~ year,
  data = multiyear_comparison,
  type = 'l',
  auto = TRUE,
  grid = TRUE,
  ylim = c(7, 12),
  xlab = 'Year',
  ylab = 'Final biomass (Mg / ha)'
)

pdf_print(multiyear_biomass_plot, 'multiyear_biomass_plot.pdf')

# Make Figure 6c
multiyear_biomass_difference_plot <- xyplot(
  final_mass_diff_percent ~ catm,
  data = multiyear_comparison,
  type = 'l',
  auto = TRUE,
  grid = TRUE,
  ylim = c(-10, 10),
  xlab = 'Atmospheric CO2 concentration (ppm)',
  ylab = '(M_RUE - M_FvCB) / M_FvCB (%)'
)

pdf_print(multiyear_biomass_difference_plot, 'multiyear_biomass_difference_plot.pdf')

