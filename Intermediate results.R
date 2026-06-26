#### Description: Intermediate results of a State-Transition Model ####
#### Conventions: n = single number, v = vector, df = dataframe, m = matrix, a = array, l = list ####

# Clear workspace
rm(list = ls())

# Load custom functions
source("Model setup.R")            # Model setup and definitions

#### Model Inputs ----
# Create a dataframe for probabilistic sensitivity analysis (PSA) inputs
df_input <- f_input(n_sim = n_sim, setting = n_setting)

##### Obtain intermediate results ----
# Probabilistic results (intermediate)
a_out_interm <- f_run_intermediate(df_input)

# str(a_out_interm[,,]) # inspect
# names(a_out_interm[1, , 1]) # inspect

##### Summary of intermediate outcomes ----
v_res <- rowMeans(colSums(a_out_interm, dims = 1))
v_res

# Validate intermediate outcomes (comparing the results below with the probabilistic base-case results: v_results_mean)
# sum(v_res[1:14])
# sum(v_res[15:28])
# sum(v_res[1:14]) - sum(v_res[15:28])
# sum(v_res[29:42])
# sum(v_res[43:56])
# sum(v_res[29:42]) - sum(v_res[43:56])
v_res[1:14] - v_res[15:28] # check intermediate difference in costs
v_res[29:42] - v_res[43:56] # check intermediate difference in qalys

# Create df_tmp (required for dfSummary())
df_tmp <- as.data.frame(t(colSums(a_out_interm, dims = 1)))

txt <- capture.output(
  print(
    dfSummary(
      df_tmp, 
      round.digits = 3,
      #style = "grid",
      plain.ascii = FALSE,
      graph.magnif = 1.2,
      headings = FALSE,
      display.labels = FALSE,
      escape.pipe = TRUE,
      varnumbers = FALSE,
      labels.col = FALSE
    )
  )
)
writeLines(txt, paste0("text/Setting_", n_setting, "_Intermediate_results.txt"))
rm(df_tmp, txt)

##### Costs and QALYs per cycle ----
a_out_cycle_res <- array(
  c(apply(a_out_interm[, 1:14, ], c(1, 3), sum),
    apply(a_out_interm[, 15:28, ], c(1, 3), sum),
    apply(a_out_interm[, 29:42, ], c(1, 3), sum),
    apply(a_out_interm[, 43:56, ], c(1, 3), sum)
  ),
  dim = c(dim(a_out_interm)[1], dim(a_out_interm)[2], 4)
)

a_out_cycle_res <- aperm(a_out_cycle_res, perm = c(1, 3, 2))

m_out_cycle_res <- rowMeans(a_out_cycle_res[, 1:4, ], dims = 2)
m_out_cycle_res_percentiles <- cbind(
  rowQuantiles(a_out_cycle_res[, 1, ], probs = c(0.025, 0.975)),
  rowQuantiles(a_out_cycle_res[, 2, ], probs = c(0.025, 0.975)),
  rowQuantiles(a_out_cycle_res[, 3, ], probs = c(0.025, 0.975)),
  rowQuantiles(a_out_cycle_res[, 4, ], probs = c(0.025, 0.975))
)

# Probabilistic costs
png(file = paste0("plots/Setting_", n_setting, "_costs_vs_time_", v_treatments[1], ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 1]),
    cumsum(m_out_cycle_res_percentiles[, 1]), 
    cumsum(m_out_cycle_res_percentiles[, 2])
  ),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Cumulative costs (probabilistic) ", v_treatments[1]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_costs_vs_time_", v_treatments[2], ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 2]),
    cumsum(m_out_cycle_res_percentiles[, 3]), 
    cumsum(m_out_cycle_res_percentiles[, 4])
  ),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Cumulative costs (probabilistic) ", v_treatments[2]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_costs_vs_time", ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 1]),
    cumsum(m_out_cycle_res[, 2])
  ),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Cumulative costs (probabilistic) ", v_treatments[1], " and ", v_treatments[2]),
  col = c(2, 2),
  lty = c(1, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  v_treatments,
  cex = 0.5,
  col = c(2, 2),
  lty = c(1, 2),
  bty = "n"
)
dev.off()

# Probabilistic QALYs 
png(file = paste0("plots/Setting_", n_setting, "_qalys_vs_time_", v_treatments[1], ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 3]),
    cumsum(m_out_cycle_res_percentiles[, 5]), 
    cumsum(m_out_cycle_res_percentiles[, 6])
  ),
  type = "l",
  ylab = "QALYs",
  xlab = "Cycle",
  main = paste0("Cumulative QALYs (probabilistic) ", v_treatments[1]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "bottomright", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_qalys_vs_time_", v_treatments[2], ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 4]),
    cumsum(m_out_cycle_res_percentiles[, 7]), 
    cumsum(m_out_cycle_res_percentiles[, 8])
  ),
  type = "l",
  ylab = "QALYs",
  xlab = "Cycle",
  main = paste0("Cumulative QALYs (probabilistic) ", v_treatments[2]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "bottomright", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)
dev.off()

png(file = paste0("plots/Setting_", n_setting, "_qalys_vs_time", ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 3]),
    cumsum(m_out_cycle_res[, 4])
  ),
  type = "l",
  ylab = "QALYs",
  xlab = "Cycle",
  main = paste0("Cumulative QALYs (probabilistic) ", v_treatments[1], " and ", v_treatments[2]),
  col = c(2, 2),
  lty = c(1, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  v_treatments,
  cex = 0.5,
  col = c(2, 2),
  lty = c(1, 2),
  bty = "n"
)
dev.off()

##### State occupancy per cycle ----
m_trace <- rowMeans(a_out_interm[, 57:80, ], dims = 2)

# Probabilistic state occupancy
png(file = paste0("plots/Setting_", n_setting, "_ly_by_hs_vs_time", ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = m_trace, 
  ylim = c(0, 1),
  type = "l",
  ylab = "LYs",
  xlab = "Cycle",
  main = paste0("Average LYs for ", v_treatments[1], " and ", v_treatments[2]),
  col = rep(2:7, 4), 
  lty =  c(rep(2, 6), rep(3, 6), rep(4, 6), rep(6, 6))
) # matplot end
legend(
  "topleft", 
  inset = c(0.05, 0),
  dimnames(m_trace)[[2]], 
  cex = 0.75,
  col = rep(2:7, 4), 
  lty =  c(rep(2, 6), rep(3, 6), rep(4, 6), rep(6, 6)),
  bty = "n"
) # legend end
dev.off()

##### Disaggregated costs and QALYs per cycle ----
m_out_dis_cycle_res <- rowMeans(a_out_interm[, 1:56, ], dims = 2)

# Health state costs
png(file = paste0("plots/Setting_", n_setting, "_costs_by_hs_vs_time", ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = m_out_dis_cycle_res[, 1:28], 
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Average health state costs for ", v_treatments[1], " and ", v_treatments[2]),
  col = rainbow(14),
  lty = c(rep(2, 14), rep(4, 14))
) # matplot end
legend(
  "topright", 
  inset = c(0.05, 0),
  dimnames(m_out_dis_cycle_res[, 1:18])[[2]], 
  cex = 0.75,
  col = rainbow(14),
  lty = c(rep(2, 14), rep(4, 14)),
  bty = "n"
) # legend end
dev.off()

# Health state QALYs
png(file = paste0("plots/Setting_", n_setting, "_qalys_by_hs_vs_time", ".png"), width = 1500, height = 1500)
matplot(
  x = 0:n_t,
  y = m_out_dis_cycle_res[, 29:56], 
  #ylim = c(0, 1),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Average health state QALYs ", v_treatments[1], " and ", v_treatments[2]),
  col = rainbow(14),
  lty = c(rep(2, 14), rep(4, 14))
) # matplot end
legend(
  "topright", 
  inset = c(0.05, 0),
  dimnames(m_out_dis_cycle_res[, 19:36])[[2]], 
  cex = 0.75,
  col = rainbow(14),
  lty = c(rep(2, 14), rep(4, 14)),
  bty = "n"
) # legend end
dev.off()
