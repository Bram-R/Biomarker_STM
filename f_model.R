f_model <- function(params, intermediate = FALSE) {
  #' State-Transition Model
  #'
  #' This function simulates a state-transition model using a Markov framework. It calculates 
  #' the expected costs, quality-adjusted life years (QALYs), and life-years (LYs) for two 
  #' treatment strategies over a specified time horizon.
  #'
  #' @param params A named list containing model parameters, including:
  #' \itemize{
  #'   \item **Transition Probabilities:**
  #'     \itemize{
  #'       \item \code{tp_ef_ef}: Probability of remaining in "Event-Free".
  #'       \item \code{p_event_lrr}, \code{p_event_death}: Probability of event being locoregional recurrence (LRR) or death.
  #'       \item \code{tp_lrr_dm}, \code{tp_lrr_death}: Probability of transitioning from LRR to distant metastasis (DM) or death.
  #'       \item \code{tp_dm_death}: Probability of transitioning from DM to death.
  #'     }
  #'   \item **Health State Utilities:**
  #'     \itemize{
  #'       \item \code{utility_ef}, \code{utility_lrr}, \code{utility_dm}, \code{utility_death}: QALY weights per state.
  #'     }
  #'   \item **Costs:**
  #'     \itemize{
  #'       \item \code{cost_t1}, \code{cost_t2}: Strategy-specific costs.
  #'       \item \code{cost_ef_y6}, \code{cost_lrr}, \code{cost_dm}, \code{cost_death}: Costs per health state.
  #'       \item \code{cost_lrr_event}, \code{cost_dm_event}, \code{cost_death_event}: Event-related costs.
  #'       \item \code{cost_prev_arm_lymphedema_event}, \code{cost_prev_pain_event}, \code{cost_prev_fatigue_event}, \code{cost_prev_fibrosis_induration_event}: Costs of toxicity prevention.
  #'     }
  #'   \item **Discount Rates:**
  #'     \itemize{
  #'       \item \code{discount_costs}, \code{discount_qalys}, \code{discount_lys}: Discount rates for costs, QALYs, and life-years.
  #'     }
  #' }
  #'
  #' @param intermediate Logical. If \code{FALSE} (default), the function returns a named vector of total 
  #' costs, QALYs, and LYs for each treatment strategy. If \code{TRUE}, it returns detailed 
  #' intermediate cycle-level outputs in a matrix format for further analysis.
  #'
  #' @details
  #' This function:
  #' \itemize{
  #'   \item Defines transition matrices for each treatment.
  #'   \item Ensures transition probabilities are consistent (e.g., adjusting for background mortality).
  #'   \item Estimates state transitions.
  #'   \item Estimates **toxicities over time** using `f_interpolate_toxicity()`.
  #'   \item Computes **discounted costs, QALYs, and LYs** for each treatment strategy.
  #'   \item Returns either a named vector with aggregate results or a matrix with detailed cycle-level outputs.
  #' }
  #'
  #' @return 
  #' If \code{intermediate = FALSE}, returns a named vector containing:
  #' \itemize{
  #'   \item \code{Cost_Current_practice}, \code{Cost_New_treatment}
  #'   \item \code{QALY_Current_practice}, \code{QALY_New_treatment}
  #'   \item \code{LY_Current_practice}, \code{LY_New_treatment}
  #' }
  #'
  #' If \code{intermediate = TRUE}, returns a **matrix** containing:
  #' \itemize{
  #'   \item **Cycle-level health state costs, toxicity costs, and event costs** for each treatment.
  #'   \item **Cycle-level QALYs and LYs** for each treatment.
  #'   \item **Markov trace** (state occupancy probabilities) for each treatment.
  #'   \item **Toxicity prevalence over time** for each treatment.
  #' }
  #'
  #' @examples
  #' # Run model with aggregate results
  #' results <- f_model(f_input(n_sim = 1))
  #' print(results)
  #'
  #' # Run model with intermediate results
  #' results_intermediate <- f_model(f_input(n_sim = 1), intermediate = TRUE)
  #' head(results_intermediate)
  #'
  #' @export
  
  # params <- f_input(n_sim = 1, setting = 3) # for validation purposes
  
  #### Transition matrices ----
  # Initialize transition probability matrices and transition dynamics matrices
  a_transition <- a_transition_dynamics <- array(
    data = 0,
    dim = c(n_treatments, n_t, n_states, n_states),
    dimnames = list(v_treatments, 1:n_t, v_states, v_states)
  )
  
  ## Transition calculations
  # Incorporate treatment effect for positively tested
  tp_tp_ds1_ds2 <- 1 - (1 - params$tp_fn_ds1_ds2) ^ params$hr_ds1_ds2_positives
  tp_tp_ds1_death <- 1 - (1 - params$tp_fn_ds1_death) ^ params$hr_ds1_death_positives
  tp_tp_ds2_death <- 1 - (1 - params$tp_fn_ds2_death) ^ params$hr_ds2_death_positives
  
  tp_fp_ds1_ds2 <- 1 - (1 - params$tp_tn_ds1_ds2) ^ params$hr_ds1_ds2_negatives
  tp_fp_ds1_death <- 1 - (1 - params$tp_tn_ds1_death) ^ params$hr_ds1_death_negatives
  tp_fp_ds2_death <- 1 - (1 - params$tp_tn_ds2_death) ^ params$hr_ds2_death_negatives
  
  # Ensure transitions to death are > age and gender matched general population mortality 
  tp_tp_ds1_death <- pmax(m_gen_pop_mortality$prob, rep(tp_tp_ds1_death, n_t)) 
  tp_tp_ds2_death <- pmax(m_gen_pop_mortality$prob, rep(tp_tp_ds2_death, n_t)) 
  tp_fp_ds1_death <- pmax(m_gen_pop_mortality$prob, rep(tp_fp_ds1_death, n_t)) 
  tp_fp_ds2_death <- pmax(m_gen_pop_mortality$prob, rep(tp_fp_ds2_death, n_t)) 
  
  tp_fn_ds1_death <- pmax(m_gen_pop_mortality$prob, rep(params$tp_fn_ds1_death, n_t)) 
  tp_fn_ds2_death <- pmax(m_gen_pop_mortality$prob, rep(params$tp_fn_ds2_death, n_t)) 
  tp_tn_ds1_death <- pmax(m_gen_pop_mortality$prob, rep(params$tp_tn_ds1_death, n_t)) 
  tp_tn_ds2_death <- pmax(m_gen_pop_mortality$prob, rep(params$tp_tn_ds2_death, n_t)) 
  
  ## Transition probabilities for treatment 1 for TP
  # From health state 1 "Disease state 1"
  a_transition[1, , v_states[1], v_states[2]] <- tp_tp_ds1_ds2        # Transition to health state 2 
  a_transition[1, , v_states[1], v_states[3]] <- tp_tp_ds1_death      # Transition to health state 3 
  
  # From health state 2 "Disease state 2"
  a_transition[1, , v_states[2], v_states[3]] <- tp_tp_ds2_death      # Transition to health state 3 
  
  # From health state 3 "Death"
  # NA: no transitions from the death health state
  
  ## Transition probabilities for treatment 1 for FP
  # From health state 4 "Disease state 1"
  a_transition[1, , v_states[4], v_states[5]] <- tp_fp_ds1_ds2        # Transition to health state 5 
  a_transition[1, , v_states[4], v_states[6]] <- tp_fp_ds1_death      # Transition to health state 6 
  
  # From health state 5 "Disease state 2"
  a_transition[1, , v_states[5], v_states[6]] <- tp_fp_ds2_death      # Transition to health state 6 
  
  # From health state 6 "Death"
  # NA: no transitions from the death health state
  
  ## Transition probabilities for treatment 1 for FN
  # From health state 7 "Disease state 1"
  a_transition[1, , v_states[7], v_states[8]] <- params$tp_fn_ds1_ds2 # Transition to health state 8 
  a_transition[1, , v_states[7], v_states[9]] <- tp_fn_ds1_death      # Transition to health state 9 
  
  # From health state 8 "Disease state 2"
  a_transition[1, , v_states[8], v_states[9]] <- tp_fn_ds2_death      # Transition to health state 9 
  
  # From health state 9 "Death"
  # NA: no transitions from the death health state
  
  ## Transition probabilities for treatment 1 for TN
  # From health state 10 "Disease state 1"
  a_transition[1, , v_states[10], v_states[11]] <- params$tp_tn_ds1_ds2 # Transition to health state 11 
  a_transition[1, , v_states[10], v_states[12]] <- tp_tn_ds1_death      # Transition to health state 12
  
  # From health state 11 "Disease state 2"
  a_transition[1, , v_states[11], v_states[12]] <- tp_tn_ds2_death      # Transition to health state 12 
  
  # From health state 12 "Death"
  # NA: no transitions from the death health state
  
  ## Diagonals (staying in the health state) are 1 - sum of probs on the same row
  for(i in 1:n_states){
    a_transition[1, , i, i] <- 1 - rowSums(a_transition[1, , i, -i])
  }
  
  ## Transition probabilities for treatment 2
  a_transition[2, , , ] <- a_transition[1, , , ]                                                          # Copy from treatment 1
  
  # checking whether transitions sum to 1 using testthat package
  # for (i_treatment in 1:n_treatments){ # loop through the treatment options
  #   for (t in 1:n_t){ # loop through the number of cycles
  #     testthat::expect_equal(as.vector(rowSums(a_transition[i_treatment, t, 1:n_states,])), rep(1, n_states))
  #   } # close for loop for cycles
  # } # close for loop for treatments
  
  # check min and max transition probabilities
  # min(a_transition)
  # max(a_transition)
  
  # check transition matrix for t1, cycle 1
  # a_transition[1, 1, , 1:3]
  # a_transition[1, 1, , 4:6]
  # a_transition[1, 1, , 7:9]
  # a_transition[1, 1, , 10:12]
  
  #### Decision tree ----
  decision_tree <- c(
    tp_biomarker = params$p_prevalence * params$p_se_biomarker,
    fp_biomarker = (1 - params$p_prevalence) * (1 - params$p_sp_biomarker),
    fn_biomarker = params$p_prevalence * (1 - params$p_se_biomarker),
    tn_biomarker = (1 - params$p_prevalence) * params$p_sp_biomarker,
    
    tp_cp = params$p_prevalence * params$p_se_CP,
    fp_cp = (1 - params$p_prevalence) * (1 - params$p_sp_CP),
    fn_cp = params$p_prevalence * (1 - params$p_se_CP),
    tn_cp = (1 - params$p_prevalence) * params$p_sp_CP
  )
  
  # checking decision tree results
  # decision_tree 
  # sum(decision_tree[1:4]) == 1 
  # sum(decision_tree[5:8]) == 1 
  # sum(decision_tree[c(1,3)]) == params$p_prevalence
  # sum(decision_tree[c(5,7)]) == params$p_prevalence 
  
  #### Markov trace ----
  # Initialize Markov trace
  a_state_trace <- array(
    data = NA,
    dim = c(n_treatments, n_t + 1, n_states),
    dimnames = list(v_treatments, 0:n_t, v_states)
  )
  
  # Starting health state based on decisions tree
  a_state_trace[1, 1, ] <- c(decision_tree[1], 0, 0,
                             decision_tree[2], 0, 0,
                             decision_tree[3], 0, 0,
                             decision_tree[4], 0, 0)
  
  a_state_trace[2, 1, ] <- c(decision_tree[5], 0, 0,
                             decision_tree[6], 0, 0,
                             decision_tree[7], 0, 0,
                             decision_tree[8], 0, 0)
  
  # State transitions using nested loops 
  for (i_treatment in 1:n_treatments) {
    for (t in 1:n_t) {
      a_state_trace[i_treatment, t + 1, ] <- a_state_trace[i_treatment, t, ] %*% a_transition[i_treatment, t, , ] # estimate Markov trace for next cycle
      a_transition_dynamics[i_treatment, t, , ] <- diag(a_state_trace[i_treatment, t, ]) %*% a_transition[i_treatment, t, , ] # estimate transition dynamics for next cycle
    }
  }
  
  # checking whether cohort sums to 1 (each cycle) using testthat package
  # for (i_treatment in 1:n_treatments){ # loop through the treatment options
  #     testthat::expect_equal(as.vector(rowSums(a_state_trace[i_treatment, , ])), rep(1, n_t + 1))
  # } # close for loop for treatments
  
  # min(a_state_trace)
  # max(a_state_trace)
  # min(a_transition_dynamics)
  # max(a_transition_dynamics)
  
  #### Outcomes ----
  # calculate discount weight for each cycle
  m_discount <- cbind((1 + params$discount_costs)^(-(0:n_t)/12),
                      (1 + params$discount_qalys)^(-(0:n_t)/12),
                      (1 + params$discount_lys)^(-(0:n_t)/12))
  
  m_discount[1:12, ] <- 1                                                              # No discounting in the 1st year
  m_discount[361:(n_t+1), ] <- cbind((1 + params$discount_costs_30y)^(-(360:n_t)/12),  # Different discounting 30Y+ (FR setting) 
                                     (1 + params$discount_qalys_30y)^(-(360:n_t)/12),
                                     (1 + params$discount_lys_30y)^(-(360:n_t)/12))
  
  ## Cost and (dis)utility matrices over time
  # Health state costs
  m_cost <- matrix(c(params$cost_tp_ds_1,        # Cost for health state 1 
                     params$cost_tp_ds_2,        # Cost for health state 2 
                     params$cost_death,          # Cost for health state 3 
                     params$cost_fp_ds_1,        # Cost for health state 4 
                     params$cost_fp_ds_2,        # Cost for health state 5 
                     params$cost_death,          # Cost for health state 6 
                     params$cost_fn_ds_1,        # Cost for health state 7 
                     params$cost_fn_ds_2,        # Cost for health state 8 
                     params$cost_death,          # Cost for health state 9
                     params$cost_tn_ds_1,        # Cost for health state 10 
                     params$cost_tn_ds_2,        # Cost for health state 11 
                     params$cost_death),         # Cost for health state 12 
                   nrow = n_t + 1, ncol = n_states, byrow = TRUE)
  
  # Treatment costs (only for positively tested patients that are alive, i.e. health states 1, 2, 4 and 5)
  m_treatment_costs <- m_cost * 0
  m_treatment_costs[1:(round(params$cost_treatment_duration)), c(1, 2, 4, 5)] <- params$cost_treatment
  
  # Health state utility
  m_utility <- matrix(c(params$utility_tp_ds_1,  # Utility for health state 1
                        params$utility_tp_ds_2,  # Utility for health state 2 
                        params$utility_death,    # Utility for health state 3 
                        params$utility_fp_ds_1,  # Utility for health state 4 
                        params$utility_fp_ds_2,  # Utility for health state 5 
                        params$utility_death,    # Utility for health state 6 
                        params$utility_fn_ds_1,  # Utility for health state 7 
                        params$utility_fn_ds_2,  # Utility for health state 8 
                        params$utility_death,    # Utility for health state 9 
                        params$utility_tn_ds_1,  # Utility for health state 10
                        params$utility_tn_ds_2,  # Utility for health state 11
                        params$utility_death),   # Utility for health state 12
                      nrow = n_t + 1, ncol = n_states, byrow = TRUE)
  
  m_utility <-  pmin(m_utility, m_gen_pop_utility$utility)          # Ensure utility  are =< age and gender matched general population utility
  
  # Calculate LYs, QALYs and costs per cycle
  a_costs <- a_state_trace[, , ] * 
    rep(m_cost, each = n_treatments) *                        # Multiply by cost matrix
    rep(m_discount[, 1], each = n_treatments)                 # Multiply by discount factor
  
  a_tx_costs <- a_state_trace[, , ] * 
    rep(m_treatment_costs, each = n_treatments) *             # Multiply by treatment cost matrix
    rep(m_discount[, 1], each = n_treatments)                 # Multiply by discount factor
  a_tx_costs[1, 1, ] <- a_tx_costs[1, 1, ] + 
    a_state_trace[1, 1, ] * params$cost_biomarker             # Add testing costs to cycle 1
  a_tx_costs[2, 1, ] <- a_tx_costs[2, 1, ] + 
    a_state_trace[2, 1, ] * params$cost_CP                    # Add testing costs to cycle 1
  
  a_qalys <- a_state_trace[, , ] *
    rep(m_utility, each = n_treatments) *                     # Multiply by utlity matrix
    rep(m_discount[, 2], each = n_treatments) *               # Multiply by discount factor
    1/12                                                      # Multiply by time correction (monthly cycles)
  
  a_lys <- a_state_trace[, , c(-3, -6, -9, -12)] *            # Remove death health states
    rep(m_discount[, 3], each = n_treatments) *               # Multiply by discount factor
    1/12                                                      # Multiply by time correction  (monthly cycles) 
  
  # Event related costs 
  m_event_costs <- matrix((a_transition_dynamics[, , 1, 2] * params$cost_ds_2_event +          # Costs related to developing disease state 2
                             a_transition_dynamics[, , 4, 5] * params$cost_ds_2_event +        # Costs related to developing disease state 2
                             a_transition_dynamics[, , 7, 8] * params$cost_ds_2_event +        # Costs related to developing disease state 2
                             a_transition_dynamics[, , 10, 11] * params$cost_ds_2_event +      # Costs related to developing disease state 2
                             
                             a_transition_dynamics[, , 1, 3] * params$cost_death_event +      # Costs related to end of life
                             a_transition_dynamics[, , 2, 3] * params$cost_death_event +      # Costs related to end of life
                             a_transition_dynamics[, , 4, 6] * params$cost_death_event +      # Costs related to end of life
                             a_transition_dynamics[, , 5, 6] * params$cost_death_event +      # Costs related to end of life
                             a_transition_dynamics[, , 7, 9] * params$cost_death_event +      # Costs related to end of life
                             a_transition_dynamics[, , 8, 9] * params$cost_death_event +      # Costs related to end of life
                             a_transition_dynamics[, , 10, 12] * params$cost_death_event +    # Costs related to end of life
                             a_transition_dynamics[, , 11, 12] * params$cost_death_event) *   # Costs related to end of life
                            rep(m_discount[-1, 1], each = n_treatments),                      # Multiply by discount factor
                          nrow = n_treatments, ncol = n_t)
  
  #### Results ----
  if(intermediate == FALSE) { 
    v_results <- setNames(
      c(rowSums(a_costs) + rowSums(m_event_costs) + rowSums(a_tx_costs),  
        rowSums(a_qalys), 
        rowSums(a_lys)
      ), # end c     
      c(paste0("Cost_", v_treatments),                          
        paste0("QALY_", v_treatments), 
        paste0("LY_", v_treatments)
      ) # end c
    ) # end setNames
    return(v_results)
  } else {
    m_res_intermediate <- 
      matrix(data = NA, 
             nrow = n_t + 1, # number of cycles
             ncol = 3 * n_treatments * (length(v_states) + length(v_tox) + 1), # number of results (number of outcomes * number of treatments * number of health states/events/toxicities)
             dimnames = list( # name dimensions
               paste0("cycle_", 1:(n_t + 1)),
               c(paste0("Cost_", v_states, "_", v_treatments[1]),   # Cost per health state for t1
                 paste0("Cost_", v_tox, "_", v_treatments[1]),      # Tox cost for t1
                 paste0("Cost_Event_", v_treatments[1]),            # Event cost for t1
                 
                 paste0("Cost_", v_states, "_", v_treatments[2]),   # Cost per health state for t2
                 paste0("Cost_", v_tox, "_", v_treatments[2]),      # Tox cost for t2
                 paste0("Cost_Event_", v_treatments[2]),            # Event cost for t2
                 
                 paste0("QALY_", v_states, "_", v_treatments[1]),   # QALYs per health state for t1
                 paste0("QALY_", v_tox, "_", v_treatments[1]),      # Tox QALY for t1
                 paste0("QALY_Event_", v_treatments[1]),            # Event QALY for t1
                 
                 paste0("QALY_", v_states, "_", v_treatments[2]),   # QALYs per health state for t2
                 paste0("QALY_", v_tox, "_", v_treatments[2]),      # Tox QALY for t2
                 paste0("QALY_Event_", v_treatments[2]),            # Event QALY for t2
                 
                 paste0("LY_", v_states, "_", v_treatments[1]),     # LYs per health state for t1 (Markov trace)
                 paste0("Incidence_", v_tox, "_", v_treatments[1]), # Tox incidence for t1
                 paste0("LY_Event_", v_treatments[1]),              # Event LYs for t1
                 
                 paste0("LY_", v_states, "_", v_treatments[2]),     # LYs per health state for t2 (Markov trace)
                 paste0("Incidence_", v_tox, "_", v_treatments[2]), # Tox incidence for t2
                 paste0("LY_Event_", v_treatments[2])               # Event LYs for t2
               ) # end c
             ) # close dimnames 
      ) # end matrix 
    
    # Costs
    m_res_intermediate[, 1:n_states] <- a_costs[1,,]                                                         # health state costs t1 
    m_res_intermediate[, (n_states + 1):(n_states + length(v_tox))] <- a_costs_tox[1,,]                      # toxicity costs t1 
    m_res_intermediate[, (n_states + 1 + length(v_tox))] <- c(params$cost_t1 + 0,                            # event costs t1 (no n_tox_prev_costs)
                                                              m_event_costs[1,])           
    
    n_dim <- dim(m_res_intermediate)[2] / 3 * 0.5
    m_res_intermediate[, n_dim + 1:n_states] <- a_costs[2,,]                                                 # health state costs t2
    m_res_intermediate[, n_dim + (n_states + 1):(n_states + length(v_tox))] <- a_costs_tox[2,,]              # toxicity costs t2
    m_res_intermediate[, n_dim + (n_states + 1 + length(v_tox))] <- c(params$cost_t2 + n_tox_prev_costs,     # event costs t2
                                                                      m_event_costs[2,])    
    
    # QALYs
    n_dim <- dim(m_res_intermediate)[2] / 3 * 1 
    m_res_intermediate[, n_dim + 1:n_states] <- a_qalys[1,,]                                                 # health state QALYs t1
    m_res_intermediate[, n_dim + (n_states + 1):(n_states + length(v_tox))] <- a_qalys_tox[1,,]              # toxicity QALYs t1
    m_res_intermediate[, n_dim + (n_states + 1 + length(v_tox))] <- 0                                        # event QALYs t1 (none for current practice)
    
    n_dim <- dim(m_res_intermediate)[2] / 3 * 1.5 
    m_res_intermediate[, n_dim + 1:n_states] <- a_qalys[2,,]                                                 # health state QALYs t2
    m_res_intermediate[, n_dim + (n_states + 1):(n_states + length(v_tox))] <- a_qalys_tox[2,,]              # toxicity QALYs t2
    m_res_intermediate[, n_dim + (n_states + 1 + length(v_tox))] <- c(n_tox_prev_disutility, rep(0, n_t)) +  # event QALYs t2 including process utility  
      v_process_utility
    
    # LYs / incidence
    n_dim <- dim(m_res_intermediate)[2] / 3 * 2 
    m_res_intermediate[, n_dim + 1:n_states] <- cbind(a_lys[1,,], rep(0, n_t + 1))                           # health state LYs t1
    m_res_intermediate[, n_dim + (n_states + 1):(n_states + length(v_tox))] <- a_toxicity[1,,]               # toxicity incidence t1
    m_res_intermediate[, n_dim + (n_states + 1 + length(v_tox))] <- 0                                        # event LYs t1 (none)
    
    n_dim <- dim(m_res_intermediate)[2] / 3 * 2.5 
    m_res_intermediate[, n_dim + 1:n_states] <- cbind(a_lys[2,,], rep(0, n_t + 1))                           # health state LYs t2
    m_res_intermediate[, n_dim + (n_states + 1):(n_states + length(v_tox))] <- a_toxicity[2,,]               # toxicity incidence t2
    m_res_intermediate[, n_dim + (n_states + 1 + length(v_tox))] <- 0                                        # event LYs t1 (none)  
    
    return(m_res_intermediate)
    
    # Check if there are any NA values
    # any(is.na(m_res_intermediate))
    # Returns TRUE if at least one NA exists
    
    # Check if there are any negative values
    # any(m_res_intermediate[, 1:9] < 0)
    # any(m_res_intermediate[, 10:18] < 0)
    # any(m_res_intermediate[, 19:27] < 0) # these might be negative (disutilies)
    # any(m_res_intermediate[, 28:36] < 0) # these might be negative (disutilies)
    # any(m_res_intermediate[, 37:45] < 0)
    # any(m_res_intermediate[, 46:54] < 0)
    
  } # close if statement
}

