#### Description: State-Transition Model Figure ####
#### Conventions: n = single number, v = vector, df = dataframe, m = matrix, a = array, l = list ####

# Clear workspace
rm(list = ls())

# Load custom functions
source("Model setup.R")            # Model setup and definitions

#### Model structure ----
# Model part 1
state_labels <- list(
  state1 = c(v_states[1], "2,2!"),
  state2 = c(v_states[2], "0,0!"),
  state3 = c(v_states[3], "4,0!")
)

transitions <- data.frame(
  from = c("state1", "state1", "state1", 
           "state2", "state2", 
           "state3"),
  to = c("state1", "state2", "state3",
         "state2", "state3",
         "state3"),
  label = rep("", 6) 
)

# f_stm_diagram(state_labels, transitions) # Shows model Figure in Viewer

# Model part 2
rsvg_png( # Save model structure figure as .png
  charToRaw(export_svg(
    f_stm_diagram(state_labels, transitions)
  )), file = "plots/Model_structure_1.png"
)

state_labels <- list(
  state1 = c(v_states[4], "2,2!"),
  state2 = c(v_states[5], "0,0!"),
  state3 = c(v_states[6], "4,0!")
)

transitions <- data.frame(
  from = c("state1", "state1", "state1", 
           "state2", "state2", 
           "state3"),
  to = c("state1", "state2", "state3",
         "state2", "state3",
         "state3"),
  label = rep("", 6) 
)

# f_stm_diagram(state_labels, transitions) # Shows model Figure in Viewer

# Model part 3
rsvg_png( # Save model structure figure as .png
  charToRaw(export_svg(
    f_stm_diagram(state_labels, transitions)
  )), file = "plots/Model_structure_2.png"
)

state_labels <- list(
  state1 = c(v_states[7], "2,2!"),
  state2 = c(v_states[8], "0,0!"),
  state3 = c(v_states[9], "4,0!")
)

transitions <- data.frame(
  from = c("state1", "state1", "state1", 
           "state2", "state2", 
           "state3"),
  to = c("state1", "state2", "state3",
         "state2", "state3",
         "state3"),
  label = rep("", 6) 
)

# f_stm_diagram(state_labels, transitions) # Shows model Figure in Viewer

# Model part 4
rsvg_png( # Save model structure figure as .png
  charToRaw(export_svg(
    f_stm_diagram(state_labels, transitions)
  )), file = "plots/Model_structure_3.png"
)

state_labels <- list(
  state1 = c(v_states[10], "2,2!"),
  state2 = c(v_states[11], "0,0!"),
  state3 = c(v_states[12], "4,0!")
)

transitions <- data.frame(
  from = c("state1", "state1", "state1", 
           "state2", "state2", 
           "state3"),
  to = c("state1", "state2", "state3",
         "state2", "state3",
         "state3"),
  label = rep("", 6) 
)

# f_stm_diagram(state_labels, transitions) # Shows model Figure in Viewer

rsvg_png( # Save model structure figure as .png
  charToRaw(export_svg(
    f_stm_diagram(state_labels, transitions)
  )), file = "plots/Model_structure_4.png"
)

rm(state_labels, transitions)
rm(validate_state_labels, validate_transitions, generate_nodes, generate_edges, get_self_loop_style)

img_1 <- image_read("plots/model_structure_1.png")
img_2 <- image_read("plots/model_structure_2.png")
img_3 <- image_read("plots/model_structure_3.png")
img_4 <- image_read("plots/model_structure_4.png")

# 2 × 2 layout
top_row <- image_append(c(img_1, img_2))
bottom_row <- image_append(c(img_3, img_4))

combined <- image_append(c(top_row, bottom_row), stack = TRUE)

image_write(
  combined,
  path = "plots/model_structure_combined.png"
)

rm(img_1, img_2, img_3, img_4, top_row, bottom_row, combined)
