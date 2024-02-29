# Load data ####
data("input_data_morbidities")
#load("R/sysdata.rda")

# Function call ####
#devtools::load_all()
# Run function only for row 12 of "input_data_morbidities", which is the HO "asthma incidence 20+"
# output_function_call <- attribute_health_singlebhd_rr(
#   exp = input_data_morbidities$exp[12],
#   prop_pop_exp = 1,
#   cutoff = input_data_morbidities$cutoff[12],
#   bhd = input_data_morbidities$bhd_absolute[12],
#   rr = unlist(input_data_morbidities[12, c("rr_mean", "rr_lowci", "rr_highci")]),
#   rr_increment = 10,
#   erf_shape = "loglinear",
#   info_pollutant = input_data_morbidities$pollutant[12],
#   info_outcome = input_data_morbidities$outcome_metric[12],
#   info_exp = NULL,
#   info_cutoff = NULL,
#   info_rr = NULL,
#   info_bhd = NULL)

# Function code ####
#exp <- input_data_morbidities$exp[12]
exp <- c(8, 9, 10)
prop_pop_exp <- c(0.2,0.3,0.5)
cutoff <- input_data_morbidities$cutoff[12]
bhd <- input_data_morbidities$bhd_absolute[12]
rr <- unlist(input_data_morbidities[12, c("rr_mean", "rr_lowci", "rr_highci")])
rr_increment <- 10 
erf_shape <- "loglinear"
info_pollutant <- input_data_morbidities$pollutant[12] 
info_outcome <- input_data_morbidities$outcome_metric[12]
info_exp <- NULL
info_cutoff <- NULL
info_rr <- NULL
info_bhd <- NULL

## Check input data ####
stopifnot(exprs = {
  length(exp) == length(prop_pop_exp)
                        })

# OLD Combine data wrangling steps ####
# input <-
#   data.frame(
#     rr = rr,
#     rr_increment = rr_increment,
#     # Assign "mean", "low" or "high" to row with corresponding rr value
#     ci =  ifelse(rr %in% min(rr), "low",
#                  ifelse(rr %in% max(rr), "high",
#                         "mean")),
#     erf_shape = erf_shape,
#     exp = exp,
#     prop_pop_exp = prop_pop_exp,
#     cutoff = cutoff,
#     bhd = bhd,
#     approach_id = paste0("lifetable_", erf_shape)) %>%
#   # In case of same value in rr assign "mean" to first row
#   dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci)) %>%
#   # Add additional information (info_x variables)
#   dplyr::mutate(
#     info_pollutant = ifelse(is.null(info_pollutant), NA, info_pollutant),
#     info_outcome = ifelse(is.null(info_outcome), NA, info_outcome),
#     info_exp = ifelse(is.null(info_exp), NA, info_exp),
#     info_cutoff = ifelse(is.null(info_cutoff), NA, info_cutoff),
#     info_rr = ifelse(is.null(info_rr), NA, info_rr),
#     info_bhd = ifelse(is.null(info_bhd), NA, info_bhd)) %>%
#   dplyr::mutate(
#     rr_forPaf =
#       rescale_rr(rr = rr,
#                  exp = exp,
#                  cutoff = cutoff,
#                  rr_increment = rr_increment,
#                  method = {{erf_shape}}
#                  #{{}} ensures that the
#                  # value from the function argument is used
#                  # instead of from an existing column
#       ))


# Combine steps new ####
input <-
  data.frame(
    # Compile input data
    rr = rr,
    rr_increment = rr_increment,
    erf_shape = erf_shape,
    cutoff = cutoff,
    bhd = bhd,
    approach_id = paste0("lifetable_", erf_shape),
    # Assign mean, low and high rr values
    rr_ci = ifelse(rr %in% min(rr), "low",
                   ifelse(rr %in% max(rr), "high",
                          "mean"))) %>%
  # In case of same value in mean and low or high, assign value randomly
  dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci)) %>% 
  # Add exposure categories to rr's with a cross join to produce all likely combinations between exp categories and rr estimates (central, upper & lower CI)
  dplyr::cross_join(., 
                    data.frame(exp = exp,
                      prop_pop_exp = prop_pop_exp)) %>% 
  # rescale rr's for PAF
  dplyr::mutate(
    rr_forPaf =
      bestcost::rescale_rr(rr = rr,
                 exp = exp,
                 cutoff = cutoff,
                 rr_increment = rr_increment,
                 method = {{erf_shape}}))
                 #{{}} ensures that the
                 # value from the function argument is used
                 # instead of from an existing column
      

# Original method ####
# # Compile rr data to assign categories
# rr_data <-
#   data.frame(
#     rr = rr,
#     rr_increment = rr_increment,
#     erf_shape = erf_shape,
#     # Assign mean, low and high rr values
#     rr_ci = ifelse(rr %in% min(rr), "low",
#                    ifelse(rr %in% max(rr), "high",
#                           "mean"))) %>%
#   # In case of same value in mean and low or high, assign value randomly
#   dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci)) 
# 
# input_original <-
#   data.frame(
#     exp = exp,
#     prop_pop_exp = prop_pop_exp,
#     cutoff = cutoff,
#     bhd = bhd,
#     approach_id = paste0("lifetable_", erf_shape)) %>%
#   # Add rr with a cross join to produce all likely combinations
#   dplyr::cross_join(., rr_data)
# 
# # Calculate health impact attributable to exposure
# input_original <-
#   input_original %>%
#   dplyr::mutate(
#     rr_forPaf =
#       rescale_rr(rr = rr,
#                  exp = exp,
#                  cutoff = cutoff,
#                  rr_increment = rr_increment,
#                  method = {{erf_shape}}
#                  #{{}} ensures that the
#                  # value from the function argument is used
#                  # instead of from an existing column
#       ))

## Calculate population attributable fraction (PAF) ####
paf <-
  input %>%
  # Group by exp in case that there are different exposure categories
  dplyr::group_by(rr) %>%
  # Calculate PAFs per row & then reduce nrow by summing PAFs belonging to same rr
  dplyr::summarize(paf = bestcost::get_paf(rr_conc = rr_forPaf,
                                            prop_pop_exp = prop_pop_exp))

## Data wrangling ####
# Only if exposure distribution (multiple exposure categories)
# then reduce the number of rows to keep the same number as in rr
if(length(exp)>1){
  input <-
    input %>%
    dplyr::mutate(
      # Add a column for the average exp (way to summarize exposure)
      exp_mean = mean(exp), # THIS COLUMN IS NOW NOT RETURNED --> COULD BE DELETED? ####
      # Replace the actual values with "multiple" (i.e. vector) to enable reduction of rows
      exp = paste(exp, collapse = ", "),
      prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
      rr_forPaf = paste(rr_forPaf, collapse = ", ")) %>%
    # Keep only rows that are distinct
    dplyr::distinct(.)
}

# Join the input table with paf values
input <-
  input %>%
  dplyr::left_join(paf,
                   input,
                   by = "rr")

# Build the result table adding the paf to the input table
output_code <-
  input %>%
  dplyr::mutate(impact = paf * bhd,
                impact_rounded = round(impact, 0)) %>%
  # Add additional information (info_x variables)
  dplyr::mutate(
    info_pollutant = ifelse(is.null(info_pollutant), NA, info_pollutant),
    info_outcome = ifelse(is.null(info_outcome), NA, info_outcome),
    info_exp = ifelse(is.null(info_exp), NA, info_exp),
    info_cutoff = ifelse(is.null(info_cutoff), NA, info_cutoff),
    info_rr = ifelse(is.null(info_rr), NA, info_rr),
    info_bhd = ifelse(is.null(info_bhd), NA, info_bhd)) %>% 
  # Order columns
  dplyr::select(exp, prop_pop_exp, cutoff, bhd,
                rr, rr_forPaf, rr_increment, ci, erf_shape,
                paf, impact, impact_rounded,
                starts_with("info_"))
