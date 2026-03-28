# Analytical Data Science Programmer - Coding Assessment Solutions

## Overview

This document explains all 4 programs for the Roche PD Data Science coding assessment.

---

## Question 1: SDTM DS Domain Creation using {sdtm.oak}

**File:** `question_1_sdtm/01_create_ds_domain.R`

**What it does:** Creates an SDTM Disposition (DS) domain dataset from raw clinical trial data (`pharmaverseraw::ds_raw`).

**How it works:**

1. **Loads raw data** from `pharmaverseraw::ds_raw` — this contains raw eCRF disposition records
2. **Defines controlled terminology** (`study_ct`) — a data frame mapping collected values (e.g., "Adverse Event") to CDISC-standard coded terms (e.g., C-codes from codelist C66727)
3. **Maps raw variables to SDTM variables:**
   - `STUDYID` — Study identifier (e.g., "CDISCPILOT01")
   - `DOMAIN` — Fixed as "DS" (Disposition)
   - `USUBJID` — Unique subject ID combining study + subject
   - `DSSEQ` — Sequence number per subject (auto-generated with `row_number()`)
   - `DSTERM` — Reported disposition term (directly from raw data)
   - `DSDECOD` — Decoded/standardized term using `case_when()` mapping
   - `DSCAT` — Disposition category (e.g., "PROTOCOL MILESTONE")
   - `VISITNUM` / `VISIT` — Visit number and name
   - `DSDTC` / `DSSTDTC` — Disposition dates in ISO 8601 format
   - `DSSTDY` — Study day (derived from reference start date if available)
4. **Saves output** as both CSV and RDS formats

**Key R packages:** `sdtm.oak`, `dplyr`, `stringr`

---

## Question 2: ADaM ADSL Dataset Creation using {admiral}

**File:** `question_2_adam/question_2_adam_create_adsl.R`

**What it does:** Creates a subject-level analysis dataset (ADSL) from SDTM sources using the {admiral} package.

**How it works:**

1. **Starts with DM domain** (`pharmaversesdtm::dm`) as the ADSL foundation
2. **Derives 4 key variables:**

   **AGEGR9 & AGEGR9N** (Age Grouping):
   - Uses `case_when()` to categorize: "<18" (1), "18 - 50" (2), ">50" (3)
   - Applied directly to the AGE variable from DM

   **TRTSDTM & TRTSTMF** (Treatment Start DateTime):
   - Filters `EX` (Exposure) domain for valid doses: `EXDOSE > 0` OR (dose=0 AND treatment is PLACEBO)
   - Requires complete date part in EXSTDTC (≥10 characters)
   - Uses admiral's `derive_vars_dtm()` to convert character datetime to numeric with imputation
   - Takes the earliest (first) exposure record per subject
   - Time imputation flag (TRTSTMF) populated only when hours/minutes imputed (NOT for seconds-only)

   **ITTFL** (Intent-to-Treat Flag):
   - Set to "Y" if ARM is populated (subject was randomized), "N" otherwise

   **LSTAVLDT** (Last Known Alive Date):
   - Takes the maximum date across 4 sources:
     1. Last vital signs date with valid result (VS: VSSTRESN or VSSTRESC not both missing)
     2. Last AE onset date (datepart of AESTDTC)
     3. Last disposition date (datepart of DSSTDTC)
     4. Last valid treatment administration date (datepart of EXENDTC for valid doses)

3. **Saves output** as CSV and RDS

**Key R packages:** `admiral`, `dplyr`, `lubridate`

---

## Question 3: TLG - Adverse Events Reporting

### Part 1: Summary Table (`01_create_ae_summary_table.R`)

**What it does:** Creates an FDA Table 10-style summary of treatment-emergent AEs using {gtsummary} and {gt}.

**How it works:**
1. Filters `pharmaverseadam::adae` for treatment-emergent AEs (`TRTEMFL == "Y"`)
2. Gets denominator (Big N) per treatment arm from ADSL
3. Computes n (unique subjects) and % at two levels: SOC level and AETERM level within each SOC
4. Sorts SOCs and terms by descending frequency
5. Builds a hierarchical GT table with SOC headers and indented preferred terms
6. Exports as HTML

### Part 2: Visualizations (`02_create_visualizations.R`)

**Plot 1 — AE Severity by Treatment (Stacked Bar):**
- Groups AEs by `ACTARM` (treatment) and `AESEV` (severity: MILD/MODERATE/SEVERE)
- Creates a stacked bar chart showing count distribution
- Color-coded: salmon=MILD, green=MODERATE, blue=SEVERE

**Plot 2 — Top 10 Most Frequent AEs (Forest Plot with CIs):**
- Counts unique subjects per `AETERM` across all treatments
- Takes the top 10 by frequency
- Computes 95% Clopper-Pearson exact binomial confidence intervals using `binom.test()`
- Creates a horizontal dot plot with error bars showing the CI range

**Key R packages:** `ggplot2`, `gtsummary`, `gt`, `dplyr`, `forcats`

---

## Question 4: GenAI Clinical Data Assistant (Python)

**File:** `question_4_python/clinical_data_agent.py`

**What it does:** A natural language → Pandas query translator that lets clinical safety reviewers ask free-text questions about AE data.

**Architecture (Prompt → Parse → Execute):**

```
User Question
     ↓
Schema-Aware LLM Prompt (context about columns)
     ↓
Structured JSON: {"target_column": "...", "filter_value": "..."}
     ↓
Pandas DataFrame Filter
     ↓
Results: Count of USUBJID + list of IDs
```

**Key components:**

1. **AE_SCHEMA** — A dictionary defining each column's meaning, synonyms, and example values. This serves as the LLM's "understanding" of the dataset.

2. **ClinicalTrialDataAgent class:**
   - `__init__()` — Accepts the dataframe; supports both mock LLM and real OpenAI/LangChain
   - `_build_llm_prompt()` — Constructs the schema-aware prompt for the LLM
   - `_mock_llm_response()` — Rule-based mock that simulates LLM reasoning using synonym matching
   - `_call_real_llm()` — Calls OpenAI via LangChain (when API key is provided)
   - `parse_question()` — The LLM step: question → JSON
   - `execute_query()` — The Pandas step: JSON → filtered DataFrame
   - `ask()` — End-to-end pipeline returning subject count and IDs

3. **Column Mapping Logic:**
   - "severity" / "intensity" → `AESEV`
   - Specific conditions (e.g., "Headache") → `AETERM`
   - Body systems (e.g., "Cardiac", "Skin") → `AESOC`
   - "serious" / "SAE" → `AESER`

4. **Test Script** runs 3 example queries demonstrating all mapping types.

**Key Python packages:** `pandas`, `json`; optional: `langchain`, `openai`

---

## Notes

- All R programs use Pharmaverse packages (`pharmaverseraw`, `pharmaversesdtm`, `pharmaverseadam`) as data sources
- The R programs require R 4.2.0+ with packages: `admiral`, `sdtm.oak`, `gtsummary`, `gt`, `ggplot2`, `dplyr`, `tidyr`, `lubridate`
- The Python program works standalone with mock LLM, or with an OpenAI API key for real LLM integration
- Each program includes quality checks and saves output files as evidence of error-free execution
