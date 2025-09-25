# =============================================================================
# Week 1: Debugging Exercises – "Meet the Penguins"====
# =============================================================================
# Objective:
#   - Practice interpreting and fixing common R errors
#   - Reflect on AI-assisted debugging and responsible AI use
#   - Before starting this exercise you MUST select all .Rdata options to No in Project Options
#   - Restart R and confirm your Environment is empty before starting
# =============================================================================
#
# -------------------------------
# Exercise 1:====
# -------------------------------
#
# Intent: Read CSV data using a file path

penguins_raw <- read_csv("penguins_raw.csv")

# ERROR MESSAGE
# -----------------------------------------------------
# MEANING
# -----------------------------------------------------
# YOUR FIX HERE
# -----------------------------------------------------
#
# -------------------------------
# Exercise 2:====
# -------------------------------
#
# Intent: Print first 6 rows of penguins_raw


hed(penguins_raw)

# ERROR MESSAGE
# -----------------------------------------------------
# MEANING
# -----------------------------------------------------
# YOUR FIX HERE
# -----------------------------------------------------

# -------------------------------
# Exercise 3:====
# -------------------------------
# Intent: Calculate mean body mass

mean(penguins_raw$`Body Mass (g)`)

# ERROR MESSAGE
# -----------------------------------------------------
# MEANING
# -----------------------------------------------------
# YOUR FIX HERE
# -----------------------------------------------------

# -------------------------------
# Exercise 4:====
# -------------------------------
# Intent: Clean column names

clean_names(penguins_raw)

# Hint: can you google the intent and function?
#
# ERROR MESSAGE
# -----------------------------------------------------
# MEANING
# -----------------------------------------------------
# YOUR FIX HERE
# -----------------------------------------------------
#
# -------------------------------
# Exercise 5:====
# -------------------------------
# Intent: Create a boxplot of body mass by species
# Task: Produce a boxplot

penguins_raw |>
  ggplot(aes(x = `Species`, y = `Body Mass (g)`)) |>
  geom_boxplot()

# ERROR MESSAGE
# -----------------------------------------------------
# MEANING
# -----------------------------------------------------
# YOUR FIX HERE
# -----------------------------------------------------

# -------------------------------
# Exercise 6:====
# -------------------------------
# Intent: Calculate mean body mass of Adelie penguins
# Task: Generate a mean of adelie body mass

adelie_penguins <- filter(penguins_raw, species == "Adelie")

adelie_body_mass |>
  summarise(mean = mean(body_mass_g, na.rm = T))


# ERROR MESSAGE
# -----------------------------------------------------
# MEANING
# -----------------------------------------------------
# YOUR FIX HERE
# -----------------------------------------------------

# =============================================================================
# AI Reflection and Responsible Use ====
# =============================================================================
# For each exercise:
# 1. Record the exact error message in your notes.
# 2. After your own attempt, paste the error into an AI tool (ChatGPT or approved tool).
# 3. Ask for explanation, not direct solution.
# 4. Reflect:
#    - Did AI help you understand the error?
#    - How would you explain it to a peer?
#    - What prompt did you use to get a useful response?
# 5. Follow the University Responsible AI guidelines.
# =============================================================================
