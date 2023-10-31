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
knitr::opts_chunk$set(fig.width=5, fig.height=3)

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

## ----loading_libraries,error=FALSE--------------------------------------------
library(BioCro)
library(lattice)

## ----installing_lattice,eval=FALSE--------------------------------------------
#  install.packages('lattice')

## ----help_example,eval=FALSE--------------------------------------------------
#  # Access documentation for a BioCro function when the package is loaded
#  ?run_biocro
#  
#  # Access documentation for a BioCro data set, even if the package is not loaded
#  ?BioCro::soybean
#  
#  # Access documentation for a base R function
#  ?list
#  
#  # Access documentation for an R operator, which must be quoted using ', `, or "
#  ?`<-`

## ----run_biocro---------------------------------------------------------------
soybean_result <- run_biocro(
  soybean$initial_values,
  soybean$parameters,
  soybean_weather$'2002',
  soybean$direct_modules,
  soybean$differential_modules,
  soybean$ode_solver
)

## ----example_module_vector----------------------------------------------------
modules <- c(
    'libA:Module_1',
    'libB:Module_2'
)

## ----example_vector_access----------------------------------------------------
print(modules[1])
modules[1] <- 'libA:Module_3'
print(modules)

## ----example_module_list------------------------------------------------------
differential_modules <- list(
   'BioCro:partitioning_growth',
   thermal_time_module = 'BioCro:thermal_time_linear'
)

## ----example_module_swap------------------------------------------------------
differential_modules$thermal_time_module <- 'BioCro:thermal_time_trilinear'

## ----example_module_paste-----------------------------------------------------
differential_modules <- module_paste('BioCro', list(
  'partitioning_growth',
  thermal_time_module = 'thermal_time_linear'
))

## ----viewing_soybean_modules--------------------------------------------------
str(soybean$differential_modules)

## ----example_parameter_list---------------------------------------------------
parameters <- list(
    parameter_1 = 2.3,
    parameter_2 = 8.9
)

## ----example_drivers----------------------------------------------------------
hour <- seq(0, 23, 3)
temp <- 20 + 8 * sin((hour / 24) * pi)^2 # we use "vector arithmetic" to form `temp`
drivers <- data.frame(
    hour = hour,
    temp = temp
)

## ----example_view_drivers-----------------------------------------------------
print(drivers)

## ----view_ode_solver----------------------------------------------------------
str(soybean$ode_solver)

## ----run_biocro_error_quantity------------------------------------------------
soybean_result <- run_biocro(
  within(soybean$initial_values, rm(Leaf)),         # remove the initial `Leaf` value
  within(soybean$parameters, rm(leaf_reflectance)), # remove `leaf_reflectance`
  soybean_weather$'2002',
  soybean$direct_modules,
  soybean$differential_modules,
  soybean$ode_solver
)

## ----run_biocro_error_module--------------------------------------------------
soybean_result <- run_biocro(
  soybean$initial_values,
  soybean$parameters,
  soybean_weather$'2002',
  append(soybean$direct_modules, 'BioCro:nonexistent_module'), # add a nonexistent module
  soybean$differential_modules,
  soybean$ode_solver
)

## ----validate_inputs,eval=FALSE-----------------------------------------------
#  # This code is not evaluated here since it produces a large amount of text
#  valid <- validate_dynamical_system_inputs(
#    soybean$initial_values,
#    soybean$parameters,
#    soybean_weather$'2002',
#    rev(soybean$direct_modules), # Reverse the order of the direct modules
#    soybean$differential_modules
#  )

## ----view_data_frame,eval=FALSE-----------------------------------------------
#  View(soybean_result)

## ----print_column_names-------------------------------------------------------
soybean_model_outputs <- colnames(soybean_result)

## ----print_one_column---------------------------------------------------------
str(soybean_result$doy)

## ----print_subset-------------------------------------------------------------
str(soybean_result[c('doy', 'hour', 'Leaf')])

## ----print_subset_narrow------------------------------------------------------
str(soybean_result[round(soybean_result$doy) == 250, c('doy', 'hour', 'Leaf')])

## ----soybean_plot_1-----------------------------------------------------------
soybean_plot_v1 <- xyplot(
  soybean_result$Leaf ~ soybean_result$time
)
print(soybean_plot_v1)

## ----soybean_plot_v2----------------------------------------------------------
soybean_plot_v2 <- xyplot(
  Leaf ~ time,
  data = soybean_result
)
print(soybean_plot_v2)

## ----soybean_plot_v3----------------------------------------------------------
soybean_plot_v3 = xyplot(
  Stem + Leaf + Root ~ time,                   # Specify multiple data series using `+`
  data = soybean_result,                       # Plot data from `soybean_result`
  type = 'b',                                  # Plot using both points and a line (use
                                               # 'l' for just a line or 'p' for points)
  pch = 20,                                    # Use a small solid circle for the points
  ylab = 'Biomass (Mg / ha)',                  # Y label
  xlab = 'Day of year',                        # X label
  auto.key = list(space = 'right'),            # Add a legend on the right side
  grid = TRUE,                                 # Add horizontal and vertical lines
  main = 'Soybean biomass calculated in 2002', # Add a main title
  xlim = c(204, 206),                          # Specify the X axis limits
  ylim = c(0, 3)                               # Specify the Y axis limits
)
print(soybean_plot_v3)

## ----c3_module_info-----------------------------------------------------------
module_info('BioCro:c3_assimilation')

## ----c3_assimilation_v1-------------------------------------------------------
outputs <- evaluate_module('BioCro:c3_assimilation', soybean$parameters)

## ----c3_assimilation_v2-------------------------------------------------------
outputs <- evaluate_module(
  'BioCro:c3_assimilation',
  within(soybean$parameters, {
    rh = 0.7      # dimensionless
    Qabs = 1800   # micromol / m^2 / s
    Tleaf = 27    # degrees C
    gbw = 1.2     # mol / m^2 / s
    StomataWS = 1 # dimensionless; 1 indicates no water stress
    temp = 25     # degrees C
  })
)

## ----c3_light_response_curve--------------------------------------------------
rc <- module_response_curve(
  'BioCro:c3_assimilation',
  within(soybean$parameters, {
    rh = 0.7
    Tleaf = 27
    gbw = 1.2
    StomataWS = 1
    temp = 25
  }),
  data.frame(Qabs = seq(from = 0, to = 2000, length.out = 501))
)

caption <- paste0(
  'Soybean response curve calculated with\nTleaf = ', unique(rc$Tleaf),
  ' degrees C and RH = ', unique(rc$rh), '\nusing the `',
  unique(rc$module_name), '` module'
)

xyplot(
  Assim ~ Qabs,
  data = rc,
  type = 'l',
  xlab = 'Absorbed PPFD (micromol / m^2 / s)',
  ylab = 'Net CO2 assimilation rate\n(micromol / m^2 / s)',
  main = caption,
  grid = TRUE
)

## ----leaf_information---------------------------------------------------------
all_quantities <- get_all_quantities('BioCro')
leaf_quantity_subset <- all_quantities[all_quantities$quantity_name == 'Leaf', ]
leaf_modules <- unique(leaf_quantity_subset$module_name)
cat(leaf_modules, sep = '\n')

## ----total_biomass_info-------------------------------------------------------
info <- module_info('BioCro:total_biomass', verbose = FALSE)
str(info)

## ----run_biocro_with,eval=FALSE-----------------------------------------------
#  soybean_result <- with(soybean, {run_biocro(
#    initial_values,
#    parameters,
#    soybean_weather$'2002',
#    direct_modules,
#    differential_modules,
#    ode_solver
#  )})

## ----within-------------------------------------------------------------------
# Create a small list
original_list <- list(a = 1, b = 2, c = 3)

# Create a new list from the original one by removing the `a` element and
# changing the value of the `c` element
new_list <- within(original_list, {
  rm(a)
  c = 4
})

# We don't need to actually store the new list; instead we can pass it directly
# to another function. Here we perform the same operations (but separate them
# with `;` instead of writing them on separate lines) and pass the result
# directly to `str` without storing it as a named object.
str(within(original_list, {rm(a); c = 4}))

## ----append-------------------------------------------------------------------
str(append(original_list, 5))

