#### Description: Sensitivity and scenario analyses of a State-Transition Model ####
#### Conventions: n = single number, v = vector, df = dataframe, m = matrix, a = array, l = list ####

# Clear workspace
rm(list = ls())

# Load custom functions
source("Model setup.R")            # Model setup and definitions

#### Model Inputs ----
df_input_owsa <- f_input(n_sim = n_sim, setting = n_setting) # generate input parameters (min and max values for OWSA) with default n_sim
df_input_owsa <- df_input_owsa[, apply(df_input_owsa, 2, var, na.rm = TRUE) != 0] # only select parameters with variance > 0

#### Obtain deterministic sensitivity analyses ----
##### Obtain OWSA object ##### 
obj_owsa_dam <- run_owsa_det( # generate dampack OWSA object
  params_range = data.frame( # dataframe to be used for OWSA
    pars = names(df_input_owsa), # parameter names
    min = stack(sapply(df_input_owsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for owsa
    max = stack(sapply(df_input_owsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for owsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  #lapply(df_input_owsa, mean), # alternative to f_input(n_sim = 1, setting = n_setting) = mean of psa inputs 
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB", "iQALY", "iCost", "iCER", "iNMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_owsa_det

##### Inputs for TWSA ##### 
df_twsa_pairs <- if(n_setting == 1) {
  data.frame(
    twsa_1 = c("cost_t2", 
               "disutility_arm_lymphedema"),
    twsa_2 = c("cost_t2", 
               "hr_prev_arm_lymphedema"),
    twsa_3 = c("cost_t2", 
               "cost_prev_arm_lymphedema_event"),
    twsa_4 = c("disutility_arm_lymphedema", 
               "hr_prev_arm_lymphedema"),
    twsa_5 = c("disutility_arm_lymphedema", 
               "cost_prev_arm_lymphedema_event"),
    twsa_6 = c("hr_prev_arm_lymphedema", 
               "cost_prev_arm_lymphedema_event"),
    twsa_7 = c("p_arm_lymphedema_m72", 
               "cost_arm_lymphedema"),
    twsa_8 = c("p_AI_se_arm_lymphedema", 
               "p_AI_sp_arm_lymphedema")
  )} else if(n_setting == 2) {
    data.frame(
      twsa_1 = c("cost_t2", 
                 "disutility_arm_lymphedema"),
      twsa_2 = c("cost_t2", 
                 "hr_prev_arm_lymphedema"),
      twsa_3 = c("cost_t2", 
                 "cost_prev_arm_lymphedema_event"),
      twsa_4 = c("disutility_arm_lymphedema", 
                 "hr_prev_arm_lymphedema"),
      twsa_5 = c("disutility_arm_lymphedema", 
                 "cost_prev_arm_lymphedema_event"),
      twsa_6 = c("hr_prev_arm_lymphedema", 
                 "cost_prev_arm_lymphedema_event"),
      twsa_7 = c("p_arm_lymphedema_m72", 
                 "cost_arm_lymphedema"),
      twsa_8 = c("p_AI_se_arm_lymphedema", 
                 "p_AI_sp_arm_lymphedema")
    )} else if(n_setting == 3) { 
      data.frame(
        twsa_1 = c("cost_t2", 
                   "disutility_arm_lymphedema"),
        twsa_2 = c("cost_t2", 
                   "hr_prev_arm_lymphedema"),
        twsa_3 = c("cost_t2", 
                   "cost_prev_arm_lymphedema_event"),
        twsa_4 = c("disutility_arm_lymphedema", 
                   "hr_prev_arm_lymphedema"),
        twsa_5 = c("disutility_arm_lymphedema", 
                   "cost_prev_arm_lymphedema_event"),
        twsa_6 = c("hr_prev_arm_lymphedema", 
                   "cost_prev_arm_lymphedema_event"),
        twsa_7 = c("p_arm_lymphedema_m72", 
                   "cost_arm_lymphedema"),
        twsa_8 = c("p_AI_se_arm_lymphedema", 
                   "p_AI_sp_arm_lymphedema")
      )  
    }

##### Obtain TWSA objects ##### 
df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_1] 
obj_twsa_dam_1 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_2] 
obj_twsa_dam_2 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_3] 
obj_twsa_dam_3 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_4] 
obj_twsa_dam_4 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_5] 
obj_twsa_dam_5 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_6] 
obj_twsa_dam_6 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_7] 
obj_twsa_dam_7 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, df_twsa_pairs$twsa_8] 
obj_twsa_dam_8 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1, setting = n_setting), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

#### Deterministic sensitivity analyses results ----
##### Optimal strategy plots ##### 
png(file = paste0("plots/Setting_", n_setting, "_opt_strat_", "QALY.png"), width = 500, height = 500)
owsa_opt_strat( # gives error as no parameter leads to changes in optimal strategy as they vary
  obj_owsa_dam$owsa_QALY,
  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
  n_x_ticks = 5
) # owsa_opt_strat end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_opt_strat_", "Cost.png"), width = 500, height = 500)
owsa_opt_strat( # gives error as no parameter leads to changes in optimal strategy as they vary
  obj_owsa_dam$owsa_Cost,
  maximize = FALSE, # need to minimize costs
  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
  n_x_ticks = 5
) # owsa_opt_strat end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_opt_strat_", "NMB.png"), width = 500, height = 500)
owsa_opt_strat(
  obj_owsa_dam$owsa_NMB,
  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
  n_x_ticks = 5
) # owsa_opt_strat end
dev.off()

##### One way sensitivity analyses plots ##### 
png(file = paste0("plots/Setting_", n_setting, "_owsa_", "QALY.png"), width = 1000, height = 700)
plot(
  obj_owsa_dam$owsa_QALY,
  n_y_ticks = 3, 
  n_x_ticks = 2
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_owsa_", "Cost.png"), width = 1000, height = 700)
plot(
  obj_owsa_dam$owsa_Cost,
  n_y_ticks = 3, 
  n_x_ticks = 2
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_owsa_", "NMB.png"), width = 1000, height = 700)
plot(
  obj_owsa_dam$owsa_NMB,
  n_y_ticks = 3, 
  n_x_ticks = 2
) # plot end
dev.off()

##### Tornado plots ##### 
png(file = paste0("plots/Setting_", n_setting, "_tornado_", "iQALY.png"), width = 700, height = 500)
owsa_tornado(
  obj_owsa_dam$owsa_iQALY,
  min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_tornado_", "iCosts.png"), width = 700, height = 500)
owsa_tornado(
  obj_owsa_dam$owsa_iCost,
  min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_tornado_", "iNMB.png"), width = 700, height = 500)
owsa_tornado(
  obj_owsa_dam$owsa_iNMB,
  min_rel_diff = 0.5 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_tornado_", "iCER.png"), width = 700, height = 500)
owsa_tornado(
  obj_owsa_dam$owsa_iCER,
  min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end
dev.off()

#####  TWSA plots Cost ##### 
png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "1.png"), width = 700, height = 500)
plot(obj_twsa_dam_1$twsa_Cost)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "2.png"), width = 700, height = 500)
plot(obj_twsa_dam_2$twsa_Cost)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "3.png"), width = 700, height = 500)
plot(obj_twsa_dam_3$twsa_Cost)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "4.png"), width = 700, height = 500)
plot(obj_twsa_dam_4$twsa_Cost)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "5.png"), width = 700, height = 500)
plot(obj_twsa_dam_5$twsa_Cost)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "6.png"), width = 700, height = 500)
plot(obj_twsa_dam_6$twsa_Cost)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "7.png"), width = 700, height = 500)
plot(obj_twsa_dam_7$twsa_Cost)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_cost_", "8.png"), width = 700, height = 500)
plot(obj_twsa_dam_8$twsa_Cost)
dev.off()

##### TWSA plots QALY ##### 
png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "1.png"), width = 700, height = 500)
plot(obj_twsa_dam_1$twsa_QALY)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "2.png"), width = 700, height = 500)
plot(obj_twsa_dam_2$twsa_QALY)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "3.png"), width = 700, height = 500)
plot(obj_twsa_dam_3$twsa_QALY)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "4.png"), width = 700, height = 500)
plot(obj_twsa_dam_4$twsa_QALY)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "5.png"), width = 700, height = 500)
plot(obj_twsa_dam_5$twsa_QALY)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "6.png"), width = 700, height = 500)
plot(obj_twsa_dam_6$twsa_QALY)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "7.png"), width = 700, height = 500)
plot(obj_twsa_dam_7$twsa_QALY)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_qaly_", "8.png"), width = 700, height = 500)
plot(obj_twsa_dam_8$twsa_QALY)
dev.off()

##### TWSA plots NMB ##### 
png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "1.png"), width = 700, height = 500)
plot(obj_twsa_dam_1$twsa_NMB)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "2.png"), width = 700, height = 500)
plot(obj_twsa_dam_2$twsa_NMB)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "3.png"), width = 700, height = 500)
plot(obj_twsa_dam_3$twsa_NMB)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "4.png"), width = 700, height = 500)
plot(obj_twsa_dam_4$twsa_NMB)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "5.png"), width = 700, height = 500)
plot(obj_twsa_dam_5$twsa_NMB)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "6.png"), width = 700, height = 500)
plot(obj_twsa_dam_6$twsa_NMB)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "7.png"), width = 700, height = 500)
plot(obj_twsa_dam_7$twsa_NMB)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_twsa_nmb_", "8.png"), width = 700, height = 500)
plot(obj_twsa_dam_8$twsa_NMB)
dev.off()

#### Deterministic scenario analyses ---- 
##### 0: Base-case ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)
m_results_det_scenario_0 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_0 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_0[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_0[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) 

##### 1: det_scenario: Perfect biomarker diagnostic accuracy ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$p_se_biomarker <- 1
df_input_det_scenario$p_sp_biomarker <- 1

m_results_det_scenario_1 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_1 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_1[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_1[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 2: det_scenario: Biomarker has no prognostic value ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$tp_fn_ds1_ds2 <- df_input_det_scenario$tp_tn_ds1_ds2       
df_input_det_scenario$tp_fn_ds1_death <- df_input_det_scenario$tp_tn_ds1_death   
df_input_det_scenario$tp_fn_ds2_death <- df_input_det_scenario$tp_tn_ds2_death 

m_results_det_scenario_2 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_2 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_2[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_2[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 3: det_scenario: Treatment is not effective in 'negative' patients ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$hr_ds1_ds2_negatives <- 1
df_input_det_scenario$hr_ds1_death_negatives <- 1
df_input_det_scenario$hr_ds2_death_negatives <- 1

m_results_det_scenario_3 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_3 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_3[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_3[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 4: det_scenario: Current practice = treat all ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$p_se_CP <- 1
df_input_det_scenario$p_sp_CP <- 0

m_results_det_scenario_4 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_4 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_4[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_4[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 5: det_scenario: Combine scenarios 3 and 4 ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$hr_ds1_ds2_negatives <- 1
df_input_det_scenario$hr_ds1_death_negatives <- 1
df_input_det_scenario$hr_ds2_death_negatives <- 1

df_input_det_scenario$p_se_CP <- 1
df_input_det_scenario$p_sp_CP <- 0

m_results_det_scenario_5 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_5 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_5[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_5[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 6: det_scenario: Biomarker has no predictive value ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$hr_ds1_ds2_negatives <- df_input_det_scenario$hr_ds1_ds2_positives
df_input_det_scenario$hr_ds1_death_negatives <- df_input_det_scenario$hr_ds1_death_positives
df_input_det_scenario$hr_ds2_death_negatives <- df_input_det_scenario$hr_ds2_death_positives

m_results_det_scenario_6 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_6 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_6[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_6[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 7: det_scenario: Treatment only affects transition to health state 2 ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$hr_ds1_death_positives <- 1
df_input_det_scenario$hr_ds2_death_positives <- 1

df_input_det_scenario$hr_ds1_death_negatives <- 1
df_input_det_scenario$hr_ds2_death_negatives <- 1

m_results_det_scenario_7 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_7 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_7[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_7[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 8: det_scenario: Decreased biomarker costs ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$cost_biomarker <- 4000 

m_results_det_scenario_8 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_8 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_8[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_8[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 9: det_scenario: Combine scenarios 4 and 6 ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$p_se_CP <- 1
df_input_det_scenario$p_sp_CP <- 0

df_input_det_scenario$hr_ds1_ds2_negatives <- df_input_det_scenario$hr_ds1_ds2_positives
df_input_det_scenario$hr_ds1_death_negatives <- df_input_det_scenario$hr_ds1_death_positives
df_input_det_scenario$hr_ds2_death_negatives <- df_input_det_scenario$hr_ds2_death_positives

m_results_det_scenario_9 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_9 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_9[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_9[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### Deterministic scenario results Table ##### 
sink(file = paste0("text/Setting_", n_setting, "_Deterministic_scenario_analyses.txt"))
cat("\n")
cat("Deterministic base-case")
cat("\n")

obj_icers_det_scenario_0 

cat("\n")
cat("Deterministic scenario 1: Perfect biomarker diagnostic accuracy")
cat("\n")

obj_icers_det_scenario_1 

cat("\n")
cat("Deterministic scenario 2: Biomarker has no prognostic value (only predictive value)")
cat("\n")

obj_icers_det_scenario_2 

cat("\n")
cat("Deterministic scenario 3: Treatment is not effective in 'negative' patients")
cat("\n")

obj_icers_det_scenario_3 

cat("\n")
cat("Deterministic scenario 4: Current practice = treat all")
cat("\n")

obj_icers_det_scenario_4 

cat("\n")
cat("Deterministic scenario 5: Combine scenarios 3 and 4")
cat("\n")

obj_icers_det_scenario_5 

cat("\n")
cat("Deterministic scenario 6: Biomarker has no predictive value (only prognostic value)")
cat("\n")

obj_icers_det_scenario_6

cat("\n")
cat("Deterministic scenario 7: Treatment only affects transition to health state 2")
cat("\n")

obj_icers_det_scenario_7

cat("\n")

cat("\n")
cat("Deterministic scenario 8: Decreased biomarker costs")
cat("\n")

obj_icers_det_scenario_8

cat("\n")

cat("\n")
cat("Deterministic scenario 9: Combine scenarios 4 and 6")
cat("\n")

obj_icers_det_scenario_9

cat("\n")
sink()

##### Deterministic scenario results Figures #####
# Cost effectiveness frontiers for scenarios
png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_0", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_0, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_1", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_1, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_2", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_2, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_3", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_3, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_4", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_4, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_5", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_5, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_6", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_6, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_7", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_7, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_8", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_8, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_det_scenario_9", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_det_scenario_9, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

#### Probabilistic scenario analyses ---- 
##### 0: Base-case ##### 
m_results_prob_scenario_0 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

for (x in 1:n_sim) m_results_prob_scenario_0[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_0[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_0 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 1: prob_scenario: Perfect biomarker diagnostic accuracy ##### 
m_results_prob_scenario_1 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$p_se_biomarker <- 1
df_input_prob_scenario$p_sp_biomarker <- 1

for (x in 1:n_sim) m_results_prob_scenario_1[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_1[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_1 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 2: prob_scenario: Biomarker has no prognostic value ##### 
m_results_prob_scenario_2 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$tp_fn_ds1_ds2 <- df_input_prob_scenario$tp_tn_ds1_ds2       
df_input_prob_scenario$tp_fn_ds1_death <- df_input_prob_scenario$tp_tn_ds1_death   
df_input_prob_scenario$tp_fn_ds2_death <- df_input_prob_scenario$tp_tn_ds2_death 

for (x in 1:n_sim) m_results_prob_scenario_2[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_2[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_2 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 3: prob_scenario: Treatment is not effective in 'negative' patients ##### 
m_results_prob_scenario_3 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$hr_ds1_ds2_negatives <- 1
df_input_prob_scenario$hr_ds1_death_negatives <- 1
df_input_prob_scenario$hr_ds2_death_negatives <- 1

for (x in 1:n_sim) m_results_prob_scenario_3[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_3[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_3 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 4: prob_scenario: Current practice = treat all ##### 
m_results_prob_scenario_4 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$p_se_CP <- 1
df_input_prob_scenario$p_sp_CP <- 0

for (x in 1:n_sim) m_results_prob_scenario_4[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_4[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_4 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 5: prob_scenario: Combine scenarios 3 and 4 ##### 
m_results_prob_scenario_5 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$hr_ds1_ds2_negatives <- 1
df_input_prob_scenario$hr_ds1_death_negatives <- 1
df_input_prob_scenario$hr_ds2_death_negatives <- 1

df_input_prob_scenario$p_se_CP <- 1
df_input_prob_scenario$p_sp_CP <- 0

for (x in 1:n_sim) m_results_prob_scenario_5[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_5[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_5 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 6: prob_scenario: Biomarker has no predictive value ##### 
m_results_prob_scenario_6 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$hr_ds1_ds2_negatives <- df_input_prob_scenario$hr_ds1_ds2_positives
df_input_prob_scenario$hr_ds1_death_negatives <- df_input_prob_scenario$hr_ds1_death_positives
df_input_prob_scenario$hr_ds2_death_negatives <- df_input_prob_scenario$hr_ds2_death_positives

for (x in 1:n_sim) m_results_prob_scenario_6[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_6[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_6 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 7: prob_scenario: Treatment only affects transition to health state 2 ##### 
m_results_prob_scenario_7 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$hr_ds1_death_positives <- 1
df_input_prob_scenario$hr_ds2_death_positives <- 1

df_input_prob_scenario$hr_ds1_death_negatives <- 1
df_input_prob_scenario$hr_ds2_death_negatives <- 1

for (x in 1:n_sim) m_results_prob_scenario_7[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_7[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_7 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 8: prob_scenario: Decreased biomarker costs ##### 
m_results_prob_scenario_8 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$cost_biomarker <- generate_gamma(4000, 500, n_sim, TRUE)

for (x in 1:n_sim) m_results_prob_scenario_8[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_8[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_8 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### 9: prob_scenario: Combine scenarios 4 and 6 ##### 
m_results_prob_scenario_9 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$p_se_CP <- 1
df_input_prob_scenario$p_sp_CP <- 0

df_input_prob_scenario$hr_ds1_ds2_negatives <- df_input_prob_scenario$hr_ds1_ds2_positives
df_input_prob_scenario$hr_ds1_death_negatives <- df_input_prob_scenario$hr_ds1_death_positives
df_input_prob_scenario$hr_ds2_death_negatives <- df_input_prob_scenario$hr_ds2_death_positives

for (x in 1:n_sim) m_results_prob_scenario_9[x, ] <- f_model(df_input_prob_scenario[x, ])

v_out_mean <- as.vector(colMeans(m_results_prob_scenario_9[, 1:(n_treatments * 2)])) # calculate average results

obj_icers_prob_scenario_9 <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:n_treatments], # mean costs per strategy
  effect = v_out_mean[(n_treatments + 1):(n_treatments * 2)], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

##### Probabilistic scenario results Table ##### 
sink(file = paste0("text/Setting_", n_setting, "_Probabilistic_scenario_analyses.txt"))
cat("\n")
cat("Probabilistic base-case")
cat("\n")

obj_icers_prob_scenario_0

cat("\n")
cat("Probabilistic scenario analysis 1: Perfect biomarker diagnostic accuracy")
cat("\n")

obj_icers_prob_scenario_1

cat("\n")
cat("Probabilistic scenario analysis 2: Biomarker has no prognostic value (only predictive value)")
cat("\n")

obj_icers_prob_scenario_2

cat("\n")
cat("Probabilistic scenario analysis 3: Treatment is not effective in 'negative' patients")
cat("\n")

obj_icers_prob_scenario_3

cat("\n")
cat("Probabilistic scenario analysis 4: Current practice = treat all")
cat("\n")

obj_icers_prob_scenario_4

cat("\n")
cat("Probabilistic scenario analysis 5: Combine scenarios 3 and 4")
cat("\n")

obj_icers_prob_scenario_5

cat("\n")
cat("Probabilistic scenario analysis 6: Biomarker has no predictive value (only prognostic value)")
cat("\n")

obj_icers_prob_scenario_6

cat("\n")
cat("Probabilistic scenario 7: Treatment only affects transition to health state 2")
cat("\n")

obj_icers_prob_scenario_7

cat("\n")

cat("\n")
cat("Probabilistic scenario 8: Decreased biomarker costs")
cat("\n")

obj_icers_prob_scenario_8

cat("\n")

cat("\n")
cat("Probabilistic scenario 9: Combine scenarios 4 and 6")
cat("\n")

obj_icers_prob_scenario_9

cat("\n")
sink()

##### Probabilistic scenario results Figures #####
# Cost effectiveness frontiers for scenarios
png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_0", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_0, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_1", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_1, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_2", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_2, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_3", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_3, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_4", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_4, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_5", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_5, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_6", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_6, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_7", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_7, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_8", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_8, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_CE_frontier_prob_scenario_9", ".png"), width = 1500, height = 1500)
plot(
  x = obj_icers_prob_scenario_9, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end
dev.off()
