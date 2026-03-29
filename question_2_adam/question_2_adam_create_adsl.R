# =============================================================================
# Question 2: ADaM ADSL Dataset Creation using {admiral}
# =============================================================================
# Objective: Create an ADSL (Subject Level Analysis) dataset using SDTM source
#            data, the {admiral} family of packages, and tidyverse tools.
#
# Input datasets:
#   pharmaversesdtm::dm   — Demographics
#   pharmaversesdtm::vs   — Vital Signs
#   pharmaversesdtm::ex   — Exposure
#   pharmaversesdtm::ds   — Disposition
#   pharmaversesdtm::ae   — Adverse Events
#
# Derived variables:
#   AGEGR9   — Age group categories: "<18", "18 - 50", ">50"
#   AGEGR9N  — Numeric age group: 1, 2, 3
#   TRTSDTM  — Treatment start datetime (first valid dose, imputed)
#   TRTSTMF  — Time imputation flag (NOT set if only seconds missing)
#   ITTFL    — Intent-to-treat flag: "Y" if ARM not missing, else "N"
#   LSTAVLDT — Last known alive date (max of VS, AE, DS, EX dates)
#
# Reference: https://pharmaverse.github.io/admiral/cran-release/articles/adsl.html
#
# Author:    Ram
# Date:      2026-03-28
# =============================================================================

# --- Load Required Libraries -------------------------------------------------
library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# --- Step 1: Load SDTM Source Datasets ---------------------------------------
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae


# --- Step 2: Start from DM — basis for ADSL ---------------------------------
# As per the Pharmaverse ADSL example: assign pharmaversesdtm::dm to adsl
adsl <- dm %>%
  select(
    STUDYID, USUBJID, SUBJID, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
    SITEID, AGE, AGEU, SEX, RACE, ETHNIC,
    ARM, ARMCD, ACTARM, ACTARMCD,
    COUNTRY, DTHFL, DTHDTC
  )

cat("\n=== Initial ADSL (from DM) ===\n")
cat("Subjects:", nrow(adsl), "\n")

# --- Step 3: Derive AGEGR9 and AGEGR9N --------------------------------------
# AGEGR9:  Age grouping into categories: "<18", "18 - 50", ">50"
# AGEGR9N: Numeric equivalent: 1, 2, 3
#
# Using admiral's derive_vars_cat() or mutate with case_when
# (derive_vars_cat is available in newer admiral versions)

adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18              ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50              ~ ">50",
      TRUE                  ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGEGR9 == "<18"      ~ 1L,
      AGEGR9 == "18 - 50"  ~ 2L,
      AGEGR9 == ">50"      ~ 3L,
      TRUE                 ~ NA_integer_
    )
  )

cat("\n=== AGEGR9 Distribution ===\n")
print(table(adsl$AGEGR9, useNA = "ifany"))
cat("\n=== AGEGR9N Distribution ===\n")
print(table(adsl$AGEGR9N, useNA = "ifany"))

# --- Step 4: Derive TRTSDTM and TRTSTMF -------------------------------------
# TRTSDTM: Datetime of patient's first exposure observation (EX.EXSTDTC)
#          converted to numeric datetime, sorted in date/time order.
#
# Rules:
#   - Only include observations where patient received a valid dose:
#     Valid dose = (EXDOSE > 0) OR (EXDOSE == 0 AND EXTRT contains 'PLACEBO')
#   - Datepart of EXSTDTC must be complete (at least YYYY-MM-DD)
#   - Imputation:
#       Time completely missing          → impute 00:00:00
#       Partially missing hours          → impute 00
#       Partially missing minutes        → impute 00
#       Partially missing seconds        → impute 00
#       If ONLY seconds are missing      → do NOT populate TRTSTMF
#   - Take the first (earliest) record per subject after sorting by datetime

# Step 4a: Filter EX for valid dose records
ex_valid <- ex %>%
  filter(
    EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))
  ) %>%
  # Ensure datepart of EXSTDTC is complete (>= 10 chars: YYYY-MM-DD)
  filter(!is.na(EXSTDTC) & nchar(EXSTDTC) >= 10)

cat("\n=== Valid EX records for TRTSDTM derivation ===\n")
cat("Records:", nrow(ex_valid), "\n")

ex_dtm <- ex_valid %>% mutate(len=str_length(EXSTDTC), 
                              EXSTDT=as.Date(EXSTDTC),
                    EXSTFL=ifelse(len>10,'N','Y'),
                    EXSTDTC2=ifelse(EXSTFL=='Y',str_c(EXSTDTC,'T00:00:00'),EXSTDTC),
                    TRTSTMF=ifelse(EXSTFL=='Y','H',NA)) %>% arrange(USUBJID,EXTRT,EXSTDTC2) %>% 
                    group_by(USUBJID,EXTRT) %>% slice_head(n=1) %>% select(USUBJID,EXTRT, EXSTDT, EXSTDTC2,TRTSTMF) %>% 
                    ungroup() %>% 
  mutate(TRTSDTM= as.POSIXct(as_datetime(EXSTDTC2), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") )


# Step 4b: Use admiral's derive_vars_dtm() to convert EXSTDTC to datetime
#          with time imputation

# Step 4c: Select earliest exposure per subject (first dose datetime)


# Step 4d: Merge into ADSL
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_dtm,
    by_vars     = exprs(USUBJID),
    new_vars    = exprs(TRTSDTM, TRTSTMF)
  )

# Step 4e: Derive TRTSDT (date part of TRTSDTM) using admiral



cat("\n=== TRTSDTM derivation ===\n")
cat("Subjects with TRTSDTM:", sum(!is.na(adsl$TRTSDTM)), "\n")
cat("\n=== TRTSTMF Distribution ===\n")
print(table(adsl$TRTSTMF, useNA = "ifany"))

# --- Step 5: Derive TRTEDTM (Treatment End Datetime) ------------------------
# Needed for LSTAVLDT source (4): last date of valid treatment administration
# Use last valid EX record's EXENDTC

ex_end <- ex_valid %>%
  derive_vars_dtm(
    dtc         = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "h",
    time_imputation    = "first"
  ) %>%
  arrange(USUBJID, desc(EXENDTM)) %>%
  group_by(USUBJID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(USUBJID, TRTEDTM = EXENDTM)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_end,
    by_vars     = exprs(USUBJID),
    new_vars    = exprs(TRTEDTM)
  )

# Derive TRTEDT (date part)
adsl <- adsl %>%
  derive_vars_dtm_to_dt(
    source_vars = exprs(TRTEDTM)
  )

# --- Step 6: Derive ITTFL (Intent-to-Treat Flag) ----------------------------
# ITTFL = "Y" if DM.ARM is not equal to missing, else "N"
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = dm %>% filter(!is.na(ARM) & ARM != "" & !is.na(RFSTDTC)),
    by_vars     = exprs(USUBJID),
    new_var     = ITTFL,
    condition    = TRUE
  )

# Recode: derive_var_merged_exist_flag sets NA for non-matches, we need "N"
adsl <- adsl %>%
  mutate(ITTFL = if_else(is.na(ITTFL), "N", ITTFL))

cat("\n=== ITTFL Distribution ===\n")
print(table(adsl$ITTFL, useNA = "ifany"))

# --- Step 7: Derive LSTAVLDT (Last Known Alive Date) ------------------------
# Set to the last date patient has documented clinical data showing alive.
# Converted to numeric date. Max of the following 4 sources:
#
# (1) Last complete vital assessment date with a valid test result
#     (VS.VSSTRESN and VS.VSSTRESC not both missing) AND datepart of VS.VSDTC not missing
#
# (2) Last complete onset date of AEs (datepart of AE.AESTDTC)
#
# (3) Last complete disposition date (datepart of DS.DSSTDTC)
#
# (4) Last date of treatment administration where patient received a valid dose
#     (datepart of ADSL.TRTEDTM — already derived above)

# --- Source 1: VS — last vital signs date with valid result ---
# Filter: VSSTRESN and VSSTRESC not both missing, datepart of VSDTC complete
vs_dates <- vs %>%
  filter(
    !is.na(VSDTC) & nchar(VSDTC) >= 10,
    !(is.na(VSSTRESN) & (is.na(VSSTRESC) | VSSTRESC == ""))
  ) %>%
  derive_vars_dt(
    dtc             = VSDTC,
    new_vars_prefix = "VSA"
  ) %>%
  # Get last date per subject
  filter(!is.na(VSADT)) %>%
  group_by(USUBJID) %>%
  summarise(LSTAVL_VS = max(VSADT, na.rm = TRUE), .groups = "drop")

# --- Source 2: AE — last AE onset date ---
ae_dates <- ae %>%
  filter(!is.na(AESTDTC) & nchar(AESTDTC) >= 10) %>%
  derive_vars_dt(
    dtc             = AESTDTC,
    new_vars_prefix = "AEST"
  ) %>%
  filter(!is.na(AESTDT)) %>%
  group_by(USUBJID) %>%
  summarise(LSTAVL_AE = max(AESTDT, na.rm = TRUE), .groups = "drop")

# --- Source 3: DS — last disposition date ---
ds_dates <- ds %>%
  filter(!is.na(DSSTDTC) & nchar(DSSTDTC) >= 10) %>%
  derive_vars_dt(
    dtc             = DSSTDTC,
    new_vars_prefix = "DSST"
  ) %>%
  filter(!is.na(DSSTDT)) %>%
  group_by(USUBJID) %>%
  summarise(LSTAVL_DS = max(DSSTDT, na.rm = TRUE), .groups = "drop")

# --- Source 4: EX — last valid treatment date (TRTEDT already in ADSL) ---
# TRTEDT was derived in Step 5 above from valid EX records

# --- Merge all 4 sources and take max per subject ---
adsl <- adsl %>%
  # Merge Source 1: VS
  derive_vars_merged(
    dataset_add = vs_dates,
    by_vars     = exprs(USUBJID),
    new_vars    = exprs(LSTAVL_VS)
  ) %>%
  # Merge Source 2: AE
  derive_vars_merged(
    dataset_add = ae_dates,
    by_vars     = exprs(USUBJID),
    new_vars    = exprs(LSTAVL_AE)
  ) %>%
  # Merge Source 3: DS
  derive_vars_merged(
    dataset_add = ds_dates,
    by_vars     = exprs(USUBJID),
    new_vars    = exprs(LSTAVL_DS)
  ) %>%
  # Take max of all 4 date sources
  rowwise() %>%
  mutate(
    LSTAVLDT = max(c_across(c(LSTAVL_VS, LSTAVL_AE, LSTAVL_DS, TRTEDT)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Handle all-NA cases (max of empty → -Inf → set to NA)
  mutate(
    LSTAVLDT = if_else(is.infinite(as.numeric(LSTAVLDT)), as.Date(NA), as.Date(LSTAVLDT))
  ) %>%
  # Drop intermediate columns
  select(-LSTAVL_VS, -LSTAVL_AE, -LSTAVL_DS)

cat("\n=== LSTAVLDT Summary ===\n")
print(summary(adsl$LSTAVLDT))
cat("Subjects with LSTAVLDT:", sum(!is.na(adsl$LSTAVLDT)), "\n")

# --- Step 8: Final Variable Selection and Ordering ---------------------------
adsl_final <- adsl %>%
  select(
    # Identifiers
    STUDYID, USUBJID, SUBJID,
    # Reference dates
    RFSTDTC, RFENDTC,
    # Site & Demographics
    SITEID, AGE, AGEU, SEX, RACE, ETHNIC,
    # Treatment Arms
    ARM, ARMCD, ACTARM, ACTARMCD,
    COUNTRY,
    # Derived: Age Grouping
    AGEGR9, AGEGR9N,
    # Derived: Treatment Start
    TRTSDTM, TRTSTMF, 
    # Derived: Treatment End
    TRTEDTM, TRTEDT,
    # Derived: ITT Flag
    ITTFL,
    # Derived: Last Known Alive
    LSTAVLDT,
    # Death
    DTHFL, DTHDTC
  )

# --- Step 9: Quality Checks -------------------------------------------------
cat("\n=== Final ADSL Structure ===\n")
str(adsl_final)

cat("\n=== Final ADSL Summary ===\n")
cat("Number of subjects:", nrow(adsl_final), "\n")
cat("Number of variables:", ncol(adsl_final), "\n")

cat("\n=== Treatment Arm Distribution ===\n")
print(table(adsl_final$ARM, useNA = "ifany"))

cat("\n=== AGEGR9 Distribution ===\n")
print(table(adsl_final$AGEGR9, useNA = "ifany"))

cat("\n=== AGEGR9N Distribution ===\n")
print(table(adsl_final$AGEGR9N, useNA = "ifany"))

cat("\n=== ITTFL Distribution ===\n")
print(table(adsl_final$ITTFL, useNA = "ifany"))

cat("\n=== TRTSDTM - First 10 subjects ===\n")
print(adsl_final %>% select(USUBJID, TRTSDTM, TRTSTMF) %>% head(10))

cat("\n=== LSTAVLDT Summary ===\n")
print(summary(adsl_final$LSTAVLDT))

cat("\n=== First 10 rows of final ADSL ===\n")
print(head(adsl_final, 10))

# --- Step 10: Save Output ----------------------------------------------------
# Save as CSV
write.csv(adsl_final, "./question_2_adam/adsl.csv", row.names = FALSE)
cat("\nADSL saved to adsl.csv\n")

# Save as RDS (preserves R data types)
saveRDS(adsl_final, "./question_2_adam/adsl.rds")
cat("ADSL saved to adsl.rds\n")

# --- Step 11: Create Log File ------------------------------------------------
log_file <- "./question_2_adam/question_2_adam_log.txt"
sink(log_file)
cat("=== ADSL Creation Log ===\n")
cat("Date:", as.character(Sys.time()), "\n")
cat("R Version:", R.version.string, "\n")
cat("admiral version:", as.character(packageVersion("admiral")), "\n\n")
cat("Input datasets:\n")
cat("  DM:", nrow(dm), "rows\n")
cat("  VS:", nrow(vs), "rows\n")
cat("  EX:", nrow(ex), "rows\n")
cat("  DS:", nrow(ds), "rows\n")
cat("  AE:", nrow(ae), "rows\n\n")
cat("Output:\n")
cat("  ADSL:", nrow(adsl_final), "subjects x", ncol(adsl_final), "variables\n\n")
cat("Derived variables:\n")
cat("  AGEGR9  — Age groups: <18, 18-50, >50\n")
cat("  AGEGR9N — Numeric: 1, 2, 3\n")
cat("  TRTSDTM — First valid exposure datetime with time imputation\n")
cat("  TRTSTMF — Time imputation flag (not set if only seconds missing)\n")
cat("  ITTFL   — ITT flag: Y if ARM populated, N otherwise\n")
cat("  LSTAVLDT— Last known alive date from VS, AE, DS, EX\n\n")
cat("Status: COMPLETED SUCCESSFULLY\n")
sink()
cat("\nLog saved to", log_file, "\n")

# --- Completion --------------------------------------------------------------
cat("\n============================================\n")
cat("Question 2: ADSL creation completed successfully!\n")
cat("============================================\n")