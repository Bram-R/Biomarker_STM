f_input <- function(n_sim, seed = 12345, setting) {
  #' Generate model input parameters
  #'
  #' Generates a complete set of model input parameters for deterministic
  #' analysis (DA) or probabilistic sensitivity analysis (PSA). The function
  #' samples uncertain parameters from their respective probability
  #' distributions and combines these with deterministic assumptions into a
  #' single data frame that serves as input for the state-transition model.
  #'
  #' The generated inputs include:
  #' \itemize{
  #'   \item Decision tree parameters (disease prevalence and diagnostic performance).
  #'   \item State-transition probabilities.
  #'   \item Treatment effectiveness (hazard ratios).
  #'   \item Health-state utilities.
  #'   \item Costs (diagnostic testing, treatment, health states and events).
  #'   \item Country-specific discount rates.
  #' }
  #'
  #' @param n_sim Integer. Number of parameter sets to generate. Use
  #'   `n_sim = 1` for deterministic analysis and `n_sim > 1` for PSA.
  #' @param seed Integer. Random seed used to ensure reproducibility.
  #' @param setting Integer. Country setting used to apply country-specific
  #'   discount rates:
  #'   \itemize{
  #'     \item `1` = United Kingdom (UK)
  #'     \item `2` = France (FR)
  #'     \item `3` = The Netherlands (NL)
  #'   }
  #'
  #' @details
  #' Parameter uncertainty is represented using beta, gamma and log-normal
  #' distributions where appropriate. Parameters without uncertainty are
  #' generated as static values.
  #'
  #' After sampling, dependent parameters are derived automatically. For
  #' example, utilities and health-state costs for true positives, false
  #' positives, false negatives and true negatives are assumed equal where
  #' specified in the model assumptions.
  #'
  #' The returned object contains one row per simulation and is intended as
  #' input for `f_model()`.
  #'
  #' @return A data frame with `n_sim` rows containing one complete set of
  #' model input parameters per simulation.
  #'
  #' @examples
  #' # Deterministic analysis
  #' df_input <- f_input(
  #'   n_sim = 1,
  #'   seed = 12345,
  #'   setting = 3
  #' )
  #'
  #' # Probabilistic sensitivity analysis
  #' df_input_psa <- f_input(
  #'   n_sim = 5000,
  #'   seed = 12345,
  #'   setting = 3
  #' )
  #'
  #' @export
  
  source("f_input_helper.R") # Load helper functions
  
  set.seed(seed)
  is_psa <- n_sim > 1
  
  # Parameter data frame
  df_input_common <- data.frame(
    
    #### Country independent parameters ----
    ##### Decision tree parameters ----
    # Disease prevalence
    p_prevalence = generate_beta(, , n_sim, is_psa, alpha = 150, beta = 50),    
    
    # Diagnostic performance 
    p_se_biomarker = generate_static(1, n_sim, is_psa), 
    p_sp_biomarker = generate_static(1, n_sim, is_psa), 
    p_se_CP = generate_static(0, n_sim, is_psa),                                                          # Assumption = all receive treatment for negatives 
    p_sp_CP = generate_static(1, n_sim, is_psa),                                                          # Assumption = all receive treatment for negatives 
    
    # Example for diagnostic performance with sp and se correlation
    # p_se_biomarker = generate_sp_se_cor(                           
    #   mean_sens = 0.850, mean_spec = 0.680, 
    #   sd_sens = 0.005, sd_spec = 0.011, rho = -0.50,             
    #   n = n_sim, is_psa = is_psa, seed = seed)[,1],
    # p_sp_biomarker = generate_sp_se_cor(                           
    #   mean_sens = 0.850, mean_spec = 0.680, 
    #   sd_sens = 0.005, sd_spec = 0.011, rho = -0.50,             
    #   n = n_sim, is_psa = is_psa, seed = seed)[,2],
    
    # Test costs
    cost_biomarker = generate_gamma(6000, 500, n_sim, is_psa),        
    cost_CP = generate_static(0, n_sim, is_psa),           
    
    ##### State-transition parameters ----
    # State-transition model probabilities (for patients receiving CAU)
    tp_fn_ds1_ds2 = generate_beta(0.200, 0.021, n_sim, is_psa),    
    tp_fn_ds1_death = generate_beta(0.100, 0.021, n_sim, is_psa),  
    tp_fn_ds2_death = generate_beta(0.200, 0.021, n_sim, is_psa),  
    
    tp_tn_ds1_ds2 = generate_beta(0.100, 0.021, n_sim, is_psa),    
    tp_tn_ds1_death = generate_beta(0.050, 0.021, n_sim, is_psa),  
    tp_tn_ds2_death = generate_beta(0.075, 0.021, n_sim, is_psa),  
    
    # Relative treatment effectiveness for TP and FP (to calculate tp for treated patients)
    hr_ds1_ds2_positives = generate_lognormal(mean = 0.50, ci_low = 0.35, ci_high = 0.85, n_sim, is_psa),
    hr_ds1_death_positives = generate_lognormal(mean = 0.80, ci_low = 0.70, ci_high = 0.95, n_sim, is_psa),
    hr_ds2_death_positives = generate_lognormal(mean = 0.80, ci_low = 0.70, ci_high = 0.95, n_sim, is_psa),
    
    hr_ds1_ds2_negatives = generate_lognormal(mean = 0.90, ci_low = 0.85, ci_high = 1.00, n_sim, is_psa),
    hr_ds1_death_negatives = generate_lognormal(mean = 0.90, ci_low = 0.85, ci_high = 1.00, n_sim, is_psa),
    hr_ds2_death_negatives = generate_lognormal(mean = 0.90, ci_low = 0.85, ci_high = 1.00, n_sim, is_psa),
    
    # Health state utilities
    utility_tp_ds_1 = generate_beta(0.700, 0.020, n_sim, is_psa),                                               
    utility_tp_ds_2 = generate_beta(0.600, 0.050, n_sim, is_psa),      
    
    utility_fp_ds_1 = generate_static(NA, n_sim, is_psa),                                                # Set equal to tp below                                 
    utility_fp_ds_2 = generate_static(NA, n_sim, is_psa),                                                # Set equal to tp belo
    
    utility_fn_ds_1 = generate_static(NA, n_sim, is_psa),                                                # Set equal to tp below                                 
    utility_fn_ds_2 = generate_static(NA, n_sim, is_psa),                                                # Set equal to tp belo
    
    utility_tn_ds_1 = generate_static(NA, n_sim, is_psa),                                                # Set equal to tp below                                 
    utility_tn_ds_2 = generate_static(NA, n_sim, is_psa),                                                # Set equal to tp below
    
    utility_death = generate_static(0, n_sim, is_psa),                                                   # Assumption
    
    # Treatment costs positives (for patients in states 1, 2 and 4, 5)
    cost_treatment_positives = generate_gamma(400, 50, n_sim, is_psa),                                    # Cost per monthly cycle
    cost_treatment_duration_positives = generate_gamma(4, 4, n_sim, is_psa),                             # Treatment duration; number of monthly cycles
    
    # Treatment costs negatives (for patients in states 7, 8 and 10, 11)
    cost_treatment_negatives = generate_gamma(25, 5, n_sim, is_psa),                                    # Cost per monthly cycle
    cost_treatment_duration_negatives = generate_gamma(8, 1, n_sim, is_psa),                             # Treatment duration; number of monthly cycles  
    
    # Health state costs
    cost_tp_ds_1 = generate_gamma(20, 5, n_sim, is_psa),         
    cost_tp_ds_2 = generate_gamma(40, 10, n_sim, is_psa),   
    
    cost_fp_ds_1 = generate_static(NA, n_sim, is_psa),                                                   # Set equal to tp below                                 
    cost_fp_ds_2 = generate_static(NA, n_sim, is_psa),                                                   # Set equal to tp belo
    
    cost_fn_ds_1 = generate_static(NA, n_sim, is_psa),                                                   # Set equal to tp below                                 
    cost_fn_ds_2 = generate_static(NA, n_sim, is_psa),                                                   # Set equal to tp belo
    
    cost_tn_ds_1 = generate_static(NA, n_sim, is_psa),                                                   # Set equal to tp below                                 
    cost_tn_ds_2 = generate_static(NA, n_sim, is_psa),                                                   # Set equal to tp below
    
    cost_death = generate_static(0, n_sim, is_psa),                                                      # Assumption
    
    # Progression and mortality costs - one-off 
    cost_ds_2_event = generate_gamma(600, 100, n_sim, is_psa),                               
    cost_death_event = generate_gamma(5000, 500, n_sim, is_psa)        
  )
  
  #### UK specific parameters ----
  if(setting == 1) { 
    df_input_country_specific <- data.frame(
      # Model settings
      discount_costs = generate_static(0.035, n_sim, is_psa),       # Discount rate for costs (Source: Standard)
      discount_qalys = generate_static(0.035, n_sim, is_psa),       # Discount rate for QALYs (Source: Standard)
      discount_lys = generate_static(0.000, n_sim, is_psa),         # Discount rate for life-years (Source: Standard)
      
      discount_costs_30y = generate_static(0.035, n_sim, is_psa),   # Discount rate for costs (Source: Standard)
      discount_qalys_30y = generate_static(0.035, n_sim, is_psa),   # Discount rate for QALYs (Source: Standard)
      discount_lys_30y = generate_static(0.000, n_sim, is_psa)      # Discount rate for life-years (Source: Standard)
    )
  }
  
  #### FR specific parameters ----
  if(setting == 2) { 
    df_input_country_specific <- data.frame(
      # Model settings
      discount_costs = generate_static(0.025, n_sim, is_psa),       # Discount rate for costs (Source: Standard)
      discount_qalys = generate_static(0.025, n_sim, is_psa),       # Discount rate for QALYs (Source: Standard)
      discount_lys = generate_static(0.000, n_sim, is_psa),         # Discount rate for life-years (Source: Standard)
      
      discount_costs_30y = generate_static(0.015, n_sim, is_psa),   # Discount rate for costs (Source: Standard)
      discount_qalys_30y = generate_static(0.015, n_sim, is_psa),   # Discount rate for QALYs (Source: Standard)
      discount_lys_30y = generate_static(0.000, n_sim, is_psa)      # Discount rate for life-years (Source: Standard)
    )
  }
  
  #### NL specific parameters ----
  if(setting == 3) { 
    df_input_country_specific <- data.frame(
      # Model settings
      discount_costs = generate_static(0.030, n_sim, is_psa),       # Discount rate for costs (Source: Standard)
      discount_qalys = generate_static(0.015, n_sim, is_psa),       # Discount rate for QALYs (Source: Standard)
      discount_lys = generate_static(0.000, n_sim, is_psa),         # Discount rate for life-years (Source: Standard)
      
      discount_costs_30y = generate_static(0.030, n_sim, is_psa),   # Discount rate for costs (Source: Standard)
      discount_qalys_30y = generate_static(0.015, n_sim, is_psa),   # Discount rate for QALYs (Source: Standard)
      discount_lys_30y = generate_static(0.000, n_sim, is_psa)      # Discount rate for life-years (Source: Standard)
    )
  }
  
  # Safeguard for invalid setting value
  if (!exists("df_input_country_specific")) {
    stop("Invalid setting: must be 1 (UK), 2 (FR), or 3 (NL)")
  }
  
  # Assembly of df_input
  df_input <- data.frame(
    df_input_common,
    df_input_country_specific
  )
  
  # Dependent calculations
  df_input$utility_fp_ds_1 <- df_input$utility_tp_ds_1  
  df_input$utility_fp_ds_2 <- df_input$utility_tp_ds_2 
    
  df_input$utility_fn_ds_1 <- df_input$utility_tp_ds_1   
  df_input$utility_fn_ds_2 <- df_input$utility_tp_ds_2   
  
  df_input$utility_tn_ds_1 <- df_input$utility_tp_ds_1   
  df_input$utility_tn_ds_2 <- df_input$utility_tp_ds_2   
  
  
  df_input$cost_fp_ds_1 <- df_input$cost_tp_ds_1  
  df_input$cost_fp_ds_2 <- df_input$cost_tp_ds_2 
  
  df_input$cost_fn_ds_1 <- df_input$cost_tp_ds_1   
  df_input$cost_fn_ds_2 <- df_input$cost_tp_ds_2   
  
  df_input$cost_tn_ds_1 <- df_input$cost_tp_ds_1   
  df_input$cost_tn_ds_2 <- df_input$cost_tp_ds_2   
  
  
  return(df_input)
}
