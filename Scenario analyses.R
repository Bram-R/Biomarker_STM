#### Description: Scenario analyses of a State-Transition Model ####
#### Conventions: n = single number, v = vector, df = dataframe, m = matrix, a = array, l = list ####

# Clear workspace
rm(list = ls())

# Load custom functions
source("Model setup.R")            # Model setup and definitions

#### Deterministic scenario analyses ---- 
##### 0: Base-case ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)
m_results_det_scenario_0 <- f_model(df_input_det_scenario)

obj_icers_det_scenario_0 <- calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_det_scenario_0[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_det_scenario_0[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) 

##### 1: det_scenario: Imperfect biomarker diagnostic accuracy ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_prob_scenario$p_se_biomarker <- generate_sp_se_cor(
  mean_sens = 0.850, mean_spec = 0.680,
  sd_sens = 0.005, sd_spec = 0.011, rho = -0.50,
  n = n_sim, is_psa = FALSE, seed = 1234)[,1]
df_input_prob_scenario$p_sp_biomarker <- generate_sp_se_cor(
  mean_sens = 0.850, mean_spec = 0.680,
  sd_sens = 0.005, sd_spec = 0.011, rho = -0.50,
  n = n_sim, is_psa = FALSE, seed = 1234)[,2]

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

##### 8: det_scenario: Alternative biomarker costs ##### 
df_input_det_scenario <- f_input(n_sim = 1, setting = n_setting)

df_input_det_scenario$cost_biomarker <- generate_gamma(mu = 4000, sigma = 500, n = n_sim, is_psa = FALSE) 

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
cat("Deterministic scenario 1: Imperfect biomarker diagnostic accuracy")
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
cat("Deterministic scenario 8: Alternative biomarker costs")
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

##### 1: prob_scenario: Imperfect biomarker diagnostic accuracy ##### 
m_results_prob_scenario_1 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$p_se_biomarker <- generate_sp_se_cor(
  mean_sens = 0.850, mean_spec = 0.680,
  sd_sens = 0.005, sd_spec = 0.011, rho = -0.50,
  n = n_sim, is_psa = TRUE, seed = 1234)[,1]
df_input_prob_scenario$p_sp_biomarker <- generate_sp_se_cor(
  mean_sens = 0.850, mean_spec = 0.680,
  sd_sens = 0.005, sd_spec = 0.011, rho = -0.50,
  n = n_sim, is_psa = TRUE, seed = 1234)[,2]

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

##### 8: prob_scenario: Alternative biomarker costs ##### 
m_results_prob_scenario_8 <- matrix( # Matrix to store result 
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

df_input_prob_scenario <- f_input(n_sim = n_sim, setting = n_setting)

df_input_prob_scenario$cost_biomarker <- generate_gamma(mu = 4000, sigma = 500, n = n_sim, is_psa = TRUE) 

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
cat("Probabilistic scenario analysis 1: Imperfect biomarker diagnostic accuracy")
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
cat("Probabilistic scenario 8: Alternative biomarker costs")
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
