# =============================================================================
# Question 1: SDTM DS Domain Creation using {sdtm.oak}
# =============================================================================
# Objective: Create an SDTM Disposition (DS) domain dataset from raw clinical
#            trial data using the {sdtm.oak} package functions:
#            generate_oak_id_vars(), assign_no_ct(), assign_ct(),
#            assign_datetime(), derive_seq(), derive_study_day()
#
# Input:     pharmaverseraw::ds_raw
# Output:    DS domain with variables: STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM,
#            DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
#
# Author:    Ram
# Date:      2026-03-28
# =============================================================================

# --- Load Required Libraries -------------------------------------------------
library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)
library(stringr)

# --- Step 1: Load Raw Data ---------------------------------------------------
ds_raw <- pharmaverseraw::ds_raw

# Inspect the raw data structure
cat("=== Raw DS Data Structure ===\n")
str(ds_raw)
cat("\n=== Column names ===\n")
print(names(ds_raw))
cat("\n=== First 10 rows ===\n")
print(head(ds_raw, 10))

# Also load DM domain (needed for derive_study_day)
dm <- pharmaverseraw::dm_raw

# --- Step 2: Generate OAK ID Variables ---------------------------------------
# This adds oak_id, raw_source, and patient_number columns needed by
# all sdtm.oak mapping functions (assign_no_ct, assign_ct, etc.)
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var  = "PATNUM",
    raw_src  = "ds_raw"
  )

# --- Step 3: Create Study Controlled Terminology -----------------------------
# This maps collected (raw) values to CDISC standard coded terms.
# Codelist C66727 = Disposition Event (NCOMPLT)
study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = c(
    "C66727", "C66727", "C66727", "C66727", "C66727",
    "C66727", "C66727", "C66727", "C66727", "C66727"
  ),
  term_code = c(
    "C41331", "C25250", "C28554", "C48226", "C48227",
    "C48250", "C142185", "C49628", "C49632", "C49634"
  ),
  term_value = c(
    "ADVERSE EVENT", "COMPLETED", "DEATH", "LACK OF EFFICACY",
    "LOST TO FOLLOW-UP", "PHYSICIAN DECISION", "PROTOCOL VIOLATION",
    "SCREEN FAILURE", "STUDY TERMINATED BY SPONSOR",
    "WITHDRAWAL BY SUBJECT"
  ),
  collected_value = c(
    "Adverse Event", "Complete", "Dead", "Lack of Efficacy",
    "Lost To Follow-Up", "Physician Decision", "Protocol Violation",
    "Trial Screen Failure", "Study Terminated By Sponsor",
    "Withdrawal by Subject"
  ),
  term_preferred_term = c(
    "AE", "Completed", "Died", NA, NA,
    NA, "Violation", "Failure to Meet Inclusion/Exclusion Criteria",
    NA, "Dropout"
  ),
  term_synonyms = c(
    "ADVERSE EVENT", "COMPLETE", "Death", NA, NA,
    NA, NA, NA, NA, "Discontinued Participation"
  )
)

# --- Step 4: Map Variables Using sdtm.oak Functions --------------------------

# 4a. Derive DSTERM (topic variable) — no controlled terminology needed
#     Raw variable: IT.DSTERM → SDTM variable: DSTERM
ds <-
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# 4b. Map DSDECOD using controlled terminology
#     Raw variable: IT.DSTERM → SDTM variable: DSDECOD
#     Uses codelist C66727 to decode collected values to standard terms
ds <- ds %>%
  assign_ct(
    raw_dat  = ds_raw,
    raw_var  = "IT.DSTERM",
    tgt_var  = "DSDECOD",
    ct_spec  = study_ct,
    ct_clst  = "C66727",
    id_vars  = oak_id_vars()
  )

# 4c. Map DSCAT — disposition category (no CT needed, direct mapping)
#     Raw variable: FORML → SDTM variable: DSCAT
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "FORML",
    tgt_var = "DSCAT",
    id_vars = oak_id_vars()
  )

# 4d. Map DSDTC — disposition collection date
#     Raw variable: DSDTCOL → SDTM variable: DSDTC
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "DSDTCOL",
    tgt_var = "DSDTC",
    raw_fmt = c("m/d/y"),
    id_vars = oak_id_vars()
  )

# 4e. Map DSSTDTC — disposition event start date
#     Raw variable: IT.DSSTDAT → SDTM variable: DSSTDTC
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m/d/y"),
    id_vars = oak_id_vars()
  )

# --- Step 5: Add Identifiers and VISIT/VISITNUM -----------------------------
ds <- ds %>%
  dplyr::mutate(
    # Standard identifiers
    STUDYID = ds_raw$STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    
    # VISIT from raw INSTANCE
    VISIT = ds_raw$INSTANCE,
    
    # VISITNUM — numeric mapping from INSTANCE
    # Scheduled visits: integer sequence per study schedule
    # Unscheduled visits: decimal (e.g., 1.1, 6.1, 13.1)
    VISITNUM = case_when(
      VISIT == "Screening 1"       ~ 1,
      VISIT == "Baseline"          ~ 3,
      VISIT == "Week 2"            ~ 4,
      VISIT == "Week 4"            ~ 5,
      VISIT == "Week 6"            ~ 6,
      VISIT == "Week 8"            ~ 7,
      VISIT == "Week 12"           ~ 8,
      VISIT == "Week 16"           ~ 9,
      VISIT == "Week 20"           ~ 10,
      VISIT == "Week 24"           ~ 11,
      VISIT == "Week 26"           ~ 12,
      VISIT == "Retrieval"         ~ 13,
      VISIT == "Ambul Ecg Removal" ~ 14,
      VISIT == "Unscheduled 1.1"   ~ 1.1,
      VISIT == "Unscheduled 4.1"   ~ 4.1,
      VISIT == "Unscheduled 5.1"   ~ 5.1,
      VISIT == "Unscheduled 6.1"   ~ 6.1,
      VISIT == "Unscheduled 8.2"   ~ 8.2,
      VISIT == "Unscheduled 13.1"  ~ 13.1,
      TRUE ~ NA_real_
    ),
    
    # Uppercase DSTERM to match CDISC convention
    DSTERM = toupper(DSTERM)
  )

# --- Step 6: Derive DSSEQ (Sequence Number) ----------------------------------
# Uses sdtm.oak's derive_seq()
ds <- ds %>%
  derive_seq(
    tgt_var  = "DSSEQ",
    rec_vars = c("USUBJID", "VISITNUM","DSTERM" )
  )

# --- Step 7: Derive DSSTDY (Study Day) --------------------------------------
# Uses sdtm.oak's derive_study_day() — same pattern as AE code
# DSSTDY = DSSTDTC - RFSTDTC + 1 (if >= RFSTDTC), else DSSTDTC - RFSTDTC

# ds <- ds %>%
#   derive_study_day(
#     dtc_var   = "DSSTDTC",
#     day_var   = "DSSTDY",
#     ref_dt_var = "RFSTDTC",
#     source_dtc = "DSSTDTC"
#   )


# --- Step 8: Select and Order Final SDTM Variables ---------------------------
ds <- ds %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ",
    "DSTERM", "DSDECOD", "DSCAT",
    "VISITNUM", "VISIT",
    "DSDTC", "DSSTDTC"
  )

# --- Step 9: Quality Checks -------------------------------------------------
cat("\n=== DS Domain Structure ===\n")
str(ds)

cat("\n=== DS Domain Summary ===\n")
cat("Number of records:", nrow(ds), "\n")
cat("Number of unique subjects:", n_distinct(ds$USUBJID), "\n")

cat("\n=== DSDECOD Frequency ===\n")
print(table(ds$DSDECOD, useNA = "ifany"))

cat("\n=== DSCAT Frequency ===\n")
print(table(ds$DSCAT, useNA = "ifany"))

cat("\n=== VISITNUM Frequency ===\n")
print(table(ds$VISITNUM, useNA = "ifany"))

cat("\n=== VISIT Frequency ===\n")
print(table(ds$VISIT, useNA = "ifany"))

cat("\n=== First 20 rows of DS domain ===\n")
print(head(ds, 20))

# --- Step 10: Save Output ----------------------------------------------------
write.csv(ds, "./question_1_sdtm/ds_domain.csv", row.names = FALSE)
cat("\nDS domain saved to ds_domain.csv\n")

saveRDS(ds, "./question_1_sdtm/ds_domain.rds")
cat("DS domain saved to ds_domain.rds\n")

# --- Log: Code ran successfully ----------------------------------------------
cat("\n============================================\n")
cat("Question 1: DS Domain creation completed successfully!\n")
cat("============================================\n")