# Objective script: facilitate testing of asssess_impact_single()

library(bestcost) ; library(dplyr) ; library(purrr)
devtools::load_all()

# Subset one line of input_data_morbidities
input_data <- input_data_morbidities[input_data_morbidities$crf_id=="PM2.5_incidence_asthma_children_appendix_HEI_2022_sensitivity" &
                                       input_data_morbidities$year==2019,]

# Test base function ####
test_base <- bestcost::assess_impact_single(
  exp = input_data_morbidities$exp[1],  # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
  cf = input_data_morbidities$cf[1],    # PM2.5=5, NO2=10, i.e. WHO AQG 2021 
  bhd = input_data_morbidities$bhd_absolute[1],
  crf = unlist(input_data_morbidities[1, 
                                      c("crf_mean", "crf_lowci", "crf_highci")]) ,
  crf_per = 10, 
  crf_rescale_method = "loglinear", 
  info_pollutant = input_data_morbidities$pollutant[1], 
  info_outcome = input_data_morbidities$outcome_metric[1])

# Define function input variables ####
exp = input_data_morbidities$exp[1]
cf = input_data_morbidities$cf[1]
bhd = input_data_morbidities$bhd_absolute[1]
crf = unlist(input_data_morbidities[1, c("crf_mean", "crf_lowci", "crf_highci")])
crf_per = 10
crf_rescale_method = "loglinear" 
info_pollutant = input_data_morbidities$pollutant[1]
info_outcome = input_data_morbidities$outcome_metric[1]
info_exp = NULL
info_cf = NULL
info_crf = NULL
info_bhd = NULL
dw = unlist(disability_weights[disability_weights$Sequela=="Partially controlled asthma", "dw_central"]) ## MOD ####

# Modify function code ####
assess_impact_single_mod <-
  function(exp, cf, crf, bhd,
           crf_per, crf_rescale_method,
           dw, ## MOD ####
           info_pollutant = NULL,
           info_outcome = NULL,
           info_exp = NULL,
           info_cf = NULL,
           info_crf = NULL,
           info_bhd = NULL){
    
    # Input data in data frame ####
    input <-
      data.frame(
        crf = crf,
        exp = exp,
        cf = cf,
        bhd = bhd,
        crf_per = crf_per,
        crf_rescale_method = crf_rescale_method,
        approach_id = paste0("lifetable_", crf_rescale_method)) %>%
      # Add additional information (info_x variables)
      dplyr::mutate(
        info_pollutant = ifelse(is.null(info_pollutant), NA, info_pollutant),
        info_outcome = ifelse(is.null(info_outcome), NA, info_outcome),
        info_exp = ifelse(is.null(info_exp), NA, info_exp),
        info_cf = ifelse(is.null(info_cf), NA, info_cf),
        info_crf = ifelse(is.null(info_crf), NA, info_crf),
        info_bhd = ifelse(is.null(info_bhd), NA, info_bhd))
    
    
    # Calculate health impact attributable to exposure ####
    # (using crf estimate which corresponds to the exposure depending on the method)
    calculation <-
      input %>%
      dplyr::mutate(
        crf_forPaf = rescale_crf(crf = crf,
                                 exp = exp,
                                 cf = cf,
                                 crf_per = crf_per,
                                 method = {{crf_rescale_method}}
                                 #{{}} ensures that the
                                 # value from the function argument is used
                                 # instead of from an existing column
        ),
        crf_ci = ifelse(crf %in% min(crf), "low",
                        ifelse(crf %in% max(crf), "high",
                               "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(crf), "mean", ci)) %>%
      
      
      # Calculate population attributable fraction (PAF) as well as impact
      dplyr::mutate(paf =  bestcost::get_paf(crf_conc = crf_forPaf),
                    impact = round(paf * bhd, 0),
                    yld = round(impact * dw, 0)) %>% ## MOD ####
      # Order columns
      dplyr::select(exp, cf, bhd,
                    crf, crf_forPaf, crf_per, ci, crf_rescale_method,
                    paf, impact,
                    yld, ## MOD ####
                    starts_with("info_"))
    
    
    return(calculation)
  }

# Test modified function ####
test_mod <- assess_impact_single_mod(exp = input_data_morbidities$exp[1],  # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
                                     cf = input_data_morbidities$cf[1],    # PM2.5=5, NO2=10, i.e. WHO AQG 2021 
                                     bhd = input_data_morbidities$bhd_absolute[1],
                                     crf = unlist(input_data_morbidities[1, 
                                                                         c("crf_mean", "crf_lowci", "crf_highci")]) ,
                                     crf_per = 10, 
                                     crf_rescale_method = "loglinear", 
                                     info_pollutant = input_data_morbidities$pollutant[1], 
                                     info_outcome = input_data_morbidities$outcome_metric[1],
                                     dw = unlist(disability_weights[disability_weights$Sequela=="Partially controlled asthma", "dw_central"]))
