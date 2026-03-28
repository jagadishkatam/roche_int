# =============================================================================
# Question 3 - Part 1: AE Summary Table using {gtsummary}
# =============================================================================
# Objective: Create a summary table of treatment-emergent adverse events (TEAEs)
#            similar to FDA Table 10.
#
# Input:     pharmaverseadam::adae, pharmaverseadam::adsl
# Output:    ae_summary_table.html (also .docx/.pdf)
#
# Rules:
#   - Filter: TRTEMFL == "Y" (treatment-emergent AEs only)
#   - Rows: AESOC (System Organ Class) and AETERM (Preferred Term)
#   - Columns: Treatment groups (ACTARM)
#   - Cell values: Count (n) and Percentage (%)
#   - Include total column with all subjects
#   - Sort by descending frequency
#
# Author:    Ram
# Date:      2026-03-28
# =============================================================================

# --- Load Required Libraries -------------------------------------------------
library(dplyr)
library(tidyr)
library(gtsummary)
library(gt)
library(stringr)

# --- Step 1: Load Input Datasets ---------------------------------------------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

cat("=== ADAE dimensions ===\n")
cat("Rows:", nrow(adae), " Cols:", ncol(adae), "\n")
cat("\n=== ADSL dimensions ===\n")
cat("Rows:", nrow(adsl), " Cols:", ncol(adsl), "\n")

# --- Step 2: Filter for Treatment-Emergent AEs -------------------------------
adae_te <- adae %>%
  filter(TRTEMFL == "Y")

cat("\n=== Treatment-Emergent AE records ===\n")
cat("Records:", nrow(adae_te), "\n")

# --- Step 3: Get the denominator (Big N) from ADSL --------------------------
# Count unique subjects per treatment arm
n_by_arm <- adsl %>%
  group_by(ACTARM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop")

cat("\n=== Subjects per Treatment Arm ===\n")
print(n_by_arm)

# Total N across all arms
n_total <- n_distinct(adsl$USUBJID)

# --- Step 4: Create Summary Statistics by SOC and AETERM ---------------------
# Count unique subjects experiencing each AE per treatment arm

# Overall Treatment-Emergent AEs header row
te_overall <- adae_te %>%
  group_by(ACTARM) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(n_by_arm, by = "ACTARM") %>%
  mutate(
    pct = round(n_subj / N * 100, 0),
    stat = paste0(n_subj, " (", pct, "%)")
  )

# By SOC (System Organ Class)
ae_by_soc <- adae_te %>%
  group_by(ACTARM, AESOC) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(n_by_arm, by = "ACTARM") %>%
  mutate(
    pct = round(n_subj / N * 100, 0),
    stat = paste0(n_subj, " (", pct, "%)")
  )

# By SOC + AETERM (Preferred Term)
ae_by_term <- adae_te %>%
  group_by(ACTARM, AESOC, AETERM) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(n_by_arm, by = "ACTARM") %>%
  mutate(
    pct = round(n_subj / N * 100, 0),
    stat = paste0(n_subj, " (", pct, "%)")
  )

# --- Step 5: Create the GT Summary Table using gtsummary ---------------------
# Prepare data in wide format for the table

# Determine unique subjects per SOC for sorting (descending frequency)
soc_freq <- adae_te %>%
  summarise(n_total = n_distinct(USUBJID), .by = AESOC) %>%
  arrange(desc(n_total))

# Create a one-record-per-subject dataset with AE flags for gtsummary
# First, create indicator variables per SOC and AETERM
adsl_ae <- adsl %>%
  select(USUBJID, ACTARM) %>%
  mutate(
    has_teae = USUBJID %in% unique(adae_te$USUBJID)
  )

# Use gtsummary's tbl_summary for a clean output
# Prepare a dataset with one row per subject, with AE occurrence flags
ae_subject <- adae_te %>%
  distinct(USUBJID, ACTARM, AESOC, AETERM)

# Build the summary table using gtsummary approach
# For the FDA-style table, we'll use gt directly for maximum control

# Pivot SOC-level stats wide
soc_wide <- ae_by_soc %>%
  select(ACTARM, AESOC, stat) %>%
  pivot_wider(names_from = ACTARM, values_from = stat, values_fill = "0 (0%)")

# Pivot AETERM-level stats wide
term_wide <- ae_by_term %>%
  select(ACTARM, AESOC, AETERM, stat) %>%
  pivot_wider(names_from = ACTARM, values_from = stat, values_fill = "0 (0%)")

# Sort SOCs by descending total frequency
soc_order <- soc_freq$AESOC

# Build the final table structure
table_rows <- list()

for (soc in soc_order) {
  # Add SOC header row
  soc_row <- soc_wide %>% filter(AESOC == soc)
  soc_row <- soc_row %>% mutate(label = AESOC, indent = 0) %>% select(-AESOC)

  # Add AETERM rows under this SOC, sorted by frequency
  terms <- term_wide %>%
    filter(AESOC == soc)

  # Sort terms by descending frequency (total across arms)
  term_freq <- adae_te %>%
    filter(AESOC == soc) %>%
    summarise(n = n_distinct(USUBJID), .by = AETERM) %>%
    arrange(desc(n))

  terms <- terms %>%
    left_join(term_freq, by = "AETERM") %>%
    arrange(desc(n)) %>%
    mutate(label = paste0("  ", AETERM), indent = 1) %>%
    select(-AESOC, -AETERM, -n)

  table_rows <- c(table_rows, list(soc_row), list(terms))
}

# Combine all rows
final_table <- bind_rows(table_rows) %>%
  relocate(label, .before = everything())

# Create GT table
gt_table <- final_table %>%
  gt() %>%
  tab_header(
    title = "Table: Summary of Treatment-Emergent Adverse Events",
    subtitle = "Safety Population"
  ) %>%
  cols_label(
    label = md("**Primary System Organ Class<br>Reported Term for the Adverse Event**")
  ) %>%
  tab_source_note("Treatment-emergent AEs defined as TRTEMFL = 'Y'") %>%
  tab_options(
    table.font.size = 10,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  )

# --- Step 6: Save Outputs ----------------------------------------------------
# Save as HTML
gtsave(gt_table, "./question_3_tlg/ae_summary_table.html")
cat("\nSummary table saved to ae_summary_table.html\n")

# --- Log: Code ran successfully ----------------------------------------------
cat("\n============================================\n")
cat("Question 3 Part 1: AE Summary Table completed successfully!\n")
cat("============================================\n")
