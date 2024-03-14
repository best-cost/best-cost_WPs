# Load data ####
data("input_data_morbidities")
load("R/sysdata.rda")

# Function call ####
# Run function only for row 12 of "input_data_morbidities", which is the HO "asthma incidence 20+"
output_function_call <- attribute_yld_singlebhd_rr(
  dw = 0.5,
  exp = c(input_data_morbidities$exp[12], input_data_morbidities$exp[12], input_data_morbidities$exp[12]),
  prop_pop_exp = c(0.2, 0.2, 0.6),
  cutoff = input_data_morbidities$cutoff[12],
  bhd = input_data_morbidities$bhd_absolute[12],
  rr = unlist(input_data_morbidities[12, c("rr_mean", "rr_lowci", "rr_highci")]),
  rr_increment = 10,
  erf_shape = "log_linear")

# Test function
exp <- c(input_data_morbidities$exp[12],input_data_morbidities$exp[12],input_data_morbidities$exp[12])
prop_pop_exp <- 1
cutoff <- input_data_morbidities$cutoff[12]
bhd <- input_data_morbidities$bhd_absolute[12]
rr <- unlist(input_data_morbidities[12, c("rr_mean", "rr_lowci", "rr_highci")])
rr_increment <- 10
erf_shape <- "log_linear"
