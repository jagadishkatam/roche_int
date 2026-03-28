# =============================================================================
# Question 3 - Part 2: AE Visualizations using {ggplot2}
# =============================================================================
# Objective: Create two plots for adverse events reporting
#   Plot 1: AE severity distribution by treatment (stacked bar chart)
#   Plot 2: Top 10 most frequent AEs with 95% Clopper-Pearson CIs
#
# Input:     pharmaverseadam::adae, pharmaverseadam::adsl
# Output:    Two PNG files
#
# Author:    Ram
# Date:      2026-03-28
# =============================================================================

# --- Load Required Libraries -------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(forcats)

# --- Load Input Datasets -----------------------------------------------------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# ===================================================================
# PLOT 1: AE Severity Distribution by Treatment (Stacked Bar Chart)
# ===================================================================
# Uses AESEV variable for severity/intensity (MILD, MODERATE, SEVERE)

cat("=== Plot 1: AE Severity Distribution ===\n")

# Count AEs by treatment arm and severity
ae_severity <- adae %>%
  filter(TRTEMFL == "Y") %>%
  group_by(ACTARM, AESEV) %>%
  summarise(count = n(), .groups = "drop")

# Set severity factor levels in correct order (SEVERE at bottom, MILD at top)
ae_severity <- ae_severity %>%
  mutate(
    AESEV = factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD"))
  )

cat("Severity counts:\n")
print(ae_severity)

# Create the stacked bar chart
p1 <- ggplot(ae_severity, aes(x = ACTARM, y = count, fill = AESEV)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c("MILD" = "#F8766D",        # Coral/salmon for MILD
               "MODERATE" = "#00BA38",      # Green for MODERATE
               "SEVERE" = "#619CFF"),       # Blue for SEVERE
    name = "Severity/Intensity"
  ) +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save Plot 1
ggsave("./question_3_tlg/plot1_ae_severity_by_treatment.png", p1,
       width = 8, height = 6, dpi = 300)
cat("Plot 1 saved to plot1_ae_severity_by_treatment.png\n")

# ===================================================================
# PLOT 2: Top 10 Most Frequent AEs with 95% Clopper-Pearson CIs
# ===================================================================
# Uses AETERM variable for adverse event terms

cat("\n=== Plot 2: Top 10 Most Frequent AEs ===\n")

# Total number of unique subjects
n_subjects <- n_distinct(adsl$USUBJID)
cat("Total subjects (denominator):", n_subjects, "\n")

# Count unique subjects per AETERM
ae_freq <- adae %>%
  filter(TRTEMFL == "Y") %>%
  group_by(AETERM) %>%
  summarise(
    n_subj = n_distinct(USUBJID),
    .groups = "drop"
  ) %>%
  arrange(desc(n_subj)) %>%
  # Take top 10

  slice_head(n = 10) %>%
  mutate(
    pct = n_subj / n_subjects * 100
  )

cat("Top 10 AEs:\n")
print(ae_freq)

# Calculate 95% Clopper-Pearson Confidence Intervals
# Clopper-Pearson is an exact binomial CI
ae_freq <- ae_freq %>%
  rowwise() %>%
  mutate(
    ci_lower = binom.test(n_subj, n_subjects)$conf.int[1] * 100,
    ci_upper = binom.test(n_subj, n_subjects)$conf.int[2] * 100
  ) %>%
  ungroup()

# Order AETERMs by frequency (for the plot y-axis)
ae_freq <- ae_freq %>%
  mutate(
    AETERM = fct_reorder(AETERM, pct)
  )

# Create the forest plot / dot plot with CIs
p2 <- ggplot(ae_freq, aes(x = pct, y = AETERM)) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper),
    height = 0.3, linewidth = 0.5, color = "gray40"
  ) +
  geom_point(size = 3, color = "black") +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subjects, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 40, by = 10)
  )

# Save Plot 2
ggsave("./question_3_tlg/plot2_top10_ae_frequency.png", p2,
       width = 9, height = 6, dpi = 300)
cat("Plot 2 saved to plot2_top10_ae_frequency.png\n")

# --- Log: Code ran successfully ----------------------------------------------
cat("\n============================================\n")
cat("Question 3 Part 2: Visualizations completed successfully!\n")
cat("============================================\n")
