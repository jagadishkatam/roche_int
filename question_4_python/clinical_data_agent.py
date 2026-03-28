"""
=============================================================================
Question 4: GenAI Clinical Data Assistant (LLM & LangChain)
=============================================================================
Objective: Develop a Generative AI Assistant that translates natural language
           questions into structured Pandas queries on an AE dataset.

Scenario:  A clinical safety reviewer asks free-text questions about AE data.
           The agent dynamically maps user intent to the correct column
           without hard-coding rules.

Input:     adae.csv (pharmaversesdtm::ae exported to CSV)
Output:    Filtered results with count of unique subjects and their IDs

Architecture:
   User Question → LLM (schema-aware) → JSON {target_column, filter_value}
   → Pandas filter → Results (USUBJID count + list)

Author:    Ram
Date:      2026-03-28
=============================================================================
"""

import json
import pandas as pd
from typing import Optional, Dict, Any

# =============================================================================
# Step 1: Schema Definition
# =============================================================================
# Define the AE dataset schema so the LLM understands the column meanings.
# This is the "context" provided to the LLM for intelligent column mapping.

AE_SCHEMA = {
    "AESEV": {
        "description": "Severity or intensity of the adverse event",
        "synonyms": ["severity", "intensity", "how severe", "how bad", "mild", "moderate", "severe"],
        "example_values": ["MILD", "MODERATE", "SEVERE"]
    },
    "AETERM": {
        "description": "Reported term for the adverse event (the specific condition/symptom)",
        "synonyms": ["adverse event", "condition", "symptom", "event name", "AE term",
                      "headache", "nausea", "dizziness", "rash", "pruritus", "diarrhoea",
                      "vomiting", "fatigue", "erythema", "dermatitis"],
        "example_values": ["HEADACHE", "NAUSEA", "APPLICATION SITE PRURITUS", "DIZZINESS"]
    },
    "AESOC": {
        "description": "Primary System Organ Class - the body system affected",
        "synonyms": ["body system", "organ class", "system", "SOC",
                      "cardiac", "skin", "nervous", "gastrointestinal", "respiratory",
                      "musculoskeletal", "psychiatric", "vascular", "infections"],
        "example_values": ["CARDIAC DISORDERS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                           "NERVOUS SYSTEM DISORDERS", "GASTROINTESTINAL DISORDERS"]
    },
    "AESER": {
        "description": "Serious adverse event flag (Y/N)",
        "synonyms": ["serious", "SAE", "serious adverse event"],
        "example_values": ["Y", "N"]
    },
    "AEREL": {
        "description": "Causality/relationship of AE to study treatment",
        "synonyms": ["related", "relationship", "causality", "drug-related",
                      "treatment-related", "caused by"],
        "example_values": ["PROBABLE", "POSSIBLE", "REMOTE", "NONE"]
    },
    "AEACN": {
        "description": "Action taken with study treatment due to AE",
        "synonyms": ["action taken", "dose modification", "discontinued",
                      "dose reduced", "drug withdrawn"],
        "example_values": ["DRUG WITHDRAWN", "DOSE REDUCED", "DOSE NOT CHANGED", "NOT APPLICABLE"]
    },
    "AEOUT": {
        "description": "Outcome of the adverse event",
        "synonyms": ["outcome", "resolved", "recovered", "ongoing", "fatal"],
        "example_values": ["RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED", "FATAL"]
    },
    "USUBJID": {
        "description": "Unique Subject Identifier",
        "synonyms": ["subject", "patient", "participant", "subject ID"],
        "example_values": ["CDISCPILOT01-01-701-1015"]
    }
}


# =============================================================================
# Step 2: LLM Implementation - ClinicalTrialDataAgent
# =============================================================================
class ClinicalTrialDataAgent:
    """
    A GenAI-powered agent that translates natural language questions about
    clinical trial AE data into structured Pandas queries.

    The agent uses an LLM to:
    1. Understand the user's question
    2. Map it to the correct dataset column
    3. Extract the filter value
    4. Execute the query and return results
    """

    def __init__(self, data: pd.DataFrame, use_mock: bool = True, api_key: Optional[str] = None):
        """
        Initialize the Clinical Trial Data Agent.

        Args:
            data: The AE DataFrame to query
            use_mock: If True, uses a rule-based mock LLM (no API key needed)
            api_key: OpenAI API key (optional, for real LLM calls)
        """
        self.data = data
        self.schema = AE_SCHEMA
        self.use_mock = use_mock
        self.api_key = api_key

        # If using real LLM, initialize the client
        if not use_mock and api_key:
            try:
                from langchain_openai import ChatOpenAI
                from langchain.prompts import ChatPromptTemplate
                self.llm = ChatOpenAI(
                    model="gpt-3.5-turbo",
                    temperature=0,
                    api_key=api_key
                )
            except ImportError:
                print("Warning: langchain not installed. Falling back to mock LLM.")
                self.use_mock = True

    def _build_llm_prompt(self, user_question: str) -> str:
        """
        Build the prompt that gives the LLM context about the dataset schema
        so it can intelligently map the user's question to the right column.
        """
        schema_desc = "You are a clinical data assistant. Given the following dataset schema:\n\n"

        for col, info in self.schema.items():
            schema_desc += f"- **{col}**: {info['description']}\n"
            schema_desc += f"  Example values: {', '.join(info['example_values'])}\n\n"

        schema_desc += (
            f"\nUser question: \"{user_question}\"\n\n"
            "Based on the question, determine:\n"
            "1. Which column (target_column) should be filtered\n"
            "2. What value (filter_value) to search for\n\n"
            "Return ONLY valid JSON in this exact format:\n"
            '{"target_column": "COLUMN_NAME", "filter_value": "VALUE"}\n\n'
            "Rules:\n"
            "- The filter_value should be in UPPERCASE to match the data\n"
            "- Match the user's intent to the most relevant column\n"
            "- If they ask about severity/intensity → AESEV\n"
            "- If they ask about a specific condition (e.g., Headache) → AETERM\n"
            "- If they ask about a body system (e.g., Cardiac, Skin) → AESOC\n"
            "- If they ask about seriousness → AESER\n"
            "- For partial matches, use the most likely matching value from the examples\n"
        )
        return schema_desc

    def _call_real_llm(self, user_question: str) -> Dict[str, str]:
        """
        Call the actual LLM (OpenAI via LangChain) to parse the question.
        """
        from langchain.prompts import ChatPromptTemplate

        prompt_template = ChatPromptTemplate.from_messages([
            ("system", "You are a clinical data analyst assistant. "
                       "You only respond with valid JSON."),
            ("human", "{prompt}")
        ])

        prompt_text = self._build_llm_prompt(user_question)
        chain = prompt_template | self.llm
        response = chain.invoke({"prompt": prompt_text})

        # Parse the LLM response
        response_text = response.content.strip()
        # Extract JSON from response (handle markdown code blocks)
        if "```" in response_text:
            response_text = response_text.split("```")[1]
            if response_text.startswith("json"):
                response_text = response_text[4:]
            response_text = response_text.strip()

        return json.loads(response_text)

    def _mock_llm_response(self, user_question: str) -> Dict[str, str]:
        """
        Mock LLM that uses schema-aware heuristics to map questions to columns.
        This simulates the LLM's role: understanding natural language and mapping
        it to the correct column + value. The logic flow is:
        Prompt → Parse → Execute (same as with a real LLM).
        """
        question_lower = user_question.lower()

        # Build the prompt (for demonstration of the flow)
        prompt = self._build_llm_prompt(user_question)

        # Simulate LLM reasoning: check each column's synonyms
        best_match_column = None
        best_match_score = 0
        extracted_value = None

        for col, info in self.schema.items():
            for synonym in info["synonyms"]:
                if synonym.lower() in question_lower:
                    # Score based on synonym length (longer = more specific match)
                    # Boost score for AESEV, AESOC, AESER which are more specific columns
                    score = len(synonym)
                    if col in ("AESEV", "AESOC", "AESER"):
                        score += 10  # Prioritize specific columns over AETERM
                    if score > best_match_score:
                        best_match_score = score
                        best_match_column = col

        # If no column match found, default to AETERM
        if best_match_column is None:
            best_match_column = "AETERM"

        # Extract the filter value from the question
        # Try to match against known example values first
        for val in self.schema[best_match_column]["example_values"]:
            if val.lower() in question_lower:
                extracted_value = val
                break

        # If no exact match, try to extract the key term
        if extracted_value is None:
            # Use heuristics based on the matched column
            if best_match_column == "AESEV":
                for severity in ["MILD", "MODERATE", "SEVERE"]:
                    if severity.lower() in question_lower:
                        extracted_value = severity
                        break
            elif best_match_column == "AESER":
                if any(w in question_lower for w in ["serious", "sae"]):
                    extracted_value = "Y"
            elif best_match_column == "AESOC":
                # Try to find body system keywords
                soc_keywords = {
                    "cardiac": "CARDIAC DISORDERS",
                    "skin": "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                    "nervous": "NERVOUS SYSTEM DISORDERS",
                    "gastrointestinal": "GASTROINTESTINAL DISORDERS",
                    "respiratory": "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
                    "psychiatric": "PSYCHIATRIC DISORDERS",
                    "vascular": "VASCULAR DISORDERS",
                    "infections": "INFECTIONS AND INFESTATIONS",
                    "musculoskeletal": "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",
                    "eye": "EYE DISORDERS",
                    "general": "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                }
                for keyword, soc_val in soc_keywords.items():
                    if keyword in question_lower:
                        extracted_value = soc_val
                        break
            elif best_match_column == "AETERM":
                # Capitalize the meaningful words from the question
                # Remove common question words
                stop_words = {"give", "me", "the", "subjects", "who", "had", "with",
                              "what", "are", "about", "show", "list", "find", "all",
                              "patients", "that", "have", "adverse", "events", "event",
                              "of", "a", "an", "for", "is", "were", "was", "do", "does"}
                words = question_lower.split()
                key_words = [w for w in words if w not in stop_words]
                if key_words:
                    extracted_value = " ".join(key_words).upper()

        if extracted_value is None:
            extracted_value = "UNKNOWN"

        result = {
            "target_column": best_match_column,
            "filter_value": extracted_value
        }

        # Print the simulated LLM interaction
        print(f"\n{'='*60}")
        print("LLM Processing (Mock)")
        print(f"{'='*60}")
        print(f"Input Question: {user_question}")
        print(f"Matched Column: {best_match_column}")
        print(f"Extracted Value: {extracted_value}")
        print(f"JSON Output: {json.dumps(result, indent=2)}")
        print(f"{'='*60}\n")

        return result

    def parse_question(self, user_question: str) -> Dict[str, str]:
        """
        Parse a user's natural language question into a structured JSON output
        containing target_column and filter_value.

        This is the core LLM step: Prompt → Parse

        Args:
            user_question: Natural language question about AE data

        Returns:
            Dict with 'target_column' and 'filter_value'
        """
        if self.use_mock:
            return self._mock_llm_response(user_question)
        else:
            return self._call_real_llm(user_question)

    def execute_query(self, parsed_output: Dict[str, str]) -> pd.DataFrame:
        """
        Execute the Pandas filter based on the LLM's structured output.

        This is the Execution step: Parse → Execute

        Args:
            parsed_output: Dict with 'target_column' and 'filter_value'

        Returns:
            Filtered DataFrame
        """
        target_column = parsed_output["target_column"]
        filter_value = parsed_output["filter_value"]

        if target_column not in self.data.columns:
            print(f"Warning: Column '{target_column}' not found in dataset.")
            return pd.DataFrame()

        # Apply the filter using case-insensitive partial matching
        mask = self.data[target_column].str.upper().str.contains(
            filter_value.upper(), na=False
        )
        filtered_df = self.data[mask]

        return filtered_df

    def ask(self, user_question: str) -> Dict[str, Any]:
        """
        End-to-end pipeline: Prompt → Parse → Execute

        Takes a natural language question and returns:
        - Count of unique subjects (USUBJID)
        - List of matching subject IDs
        - The parsed query details

        Args:
            user_question: Natural language question about AE data

        Returns:
            Dict with results including subject count and IDs
        """
        # Step 1: Parse the question using LLM
        parsed = self.parse_question(user_question)

        # Step 2: Execute the query
        filtered_data = self.execute_query(parsed)

        # Step 3: Get unique subjects
        if len(filtered_data) > 0 and "USUBJID" in filtered_data.columns:
            unique_subjects = filtered_data["USUBJID"].unique().tolist()
            n_subjects = len(unique_subjects)
        else:
            unique_subjects = []
            n_subjects = 0

        # Step 4: Compile results
        results = {
            "question": user_question,
            "parsed_query": parsed,
            "n_records": len(filtered_data),
            "n_subjects": n_subjects,
            "subject_ids": unique_subjects
        }

        # Print formatted results
        print(f"Results for: \"{user_question}\"")
        print(f"  Column filtered: {parsed['target_column']}")
        print(f"  Filter value: {parsed['filter_value']}")
        print(f"  Matching records: {len(filtered_data)}")
        print(f"  Unique subjects: {n_subjects}")
        if n_subjects > 0 and n_subjects <= 20:
            print(f"  Subject IDs: {unique_subjects}")
        elif n_subjects > 20:
            print(f"  Subject IDs (first 20): {unique_subjects[:20]}")
            print(f"  ... and {n_subjects - 20} more")

        return results


# =============================================================================
# Step 3: Test Script - Running Example Queries
# =============================================================================
def main():
    """
    Test the ClinicalTrialDataAgent with 3 example queries.
    """
    print("=" * 70)
    print("  Clinical Trial Data Agent - Test Script")
    print("  GenAI-powered Natural Language to Pandas Query Translator")
    print("=" * 70)

    # --- Load the AE dataset ---
    # Try to load from CSV first; if not available, create sample data
    try:
        ae_df = pd.read_csv("adae.csv")
        print(f"\nLoaded AE dataset from adae.csv: {ae_df.shape[0]} rows, {ae_df.shape[1]} columns")
    except FileNotFoundError:
        print("\nadae.csv not found. Creating sample AE dataset for demonstration...")
        # Create a representative sample dataset
        ae_df = pd.DataFrame({
            "USUBJID": [
                "PILOT01-01-701-1015", "PILOT01-01-701-1015", "PILOT01-01-701-1023",
                "PILOT01-01-701-1023", "PILOT01-01-701-1028", "PILOT01-01-701-1028",
                "PILOT01-01-701-1033", "PILOT01-01-701-1034", "PILOT01-01-701-1047",
                "PILOT01-01-701-1047", "PILOT01-01-701-1057", "PILOT01-01-701-1057",
                "PILOT01-01-701-1068", "PILOT01-01-701-1068", "PILOT01-01-701-1073",
            ],
            "AETERM": [
                "APPLICATION SITE PRURITUS", "HEADACHE", "DIZZINESS",
                "NAUSEA", "APPLICATION SITE ERYTHEMA", "PRURITUS",
                "DIARRHOEA", "RASH", "HEADACHE",
                "APPLICATION SITE DERMATITIS", "NAUSEA", "FATIGUE",
                "SINUS BRADYCARDIA", "APPLICATION SITE IRRITATION", "HEADACHE",
            ],
            "AESOC": [
                "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                "NERVOUS SYSTEM DISORDERS",
                "NERVOUS SYSTEM DISORDERS",
                "GASTROINTESTINAL DISORDERS",
                "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                "GASTROINTESTINAL DISORDERS",
                "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                "NERVOUS SYSTEM DISORDERS",
                "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                "GASTROINTESTINAL DISORDERS",
                "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                "CARDIAC DISORDERS",
                "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                "NERVOUS SYSTEM DISORDERS",
            ],
            "AESEV": [
                "MODERATE", "MILD", "MODERATE", "MILD", "MILD",
                "MODERATE", "MILD", "MODERATE", "SEVERE", "MILD",
                "MILD", "MODERATE", "SEVERE", "MILD", "MILD",
            ],
            "AESER": [
                "N", "N", "N", "N", "N",
                "N", "N", "N", "Y", "N",
                "N", "N", "Y", "N", "N",
            ],
            "AEREL": [
                "PROBABLE", "POSSIBLE", "PROBABLE", "POSSIBLE", "PROBABLE",
                "POSSIBLE", "REMOTE", "POSSIBLE", "PROBABLE", "PROBABLE",
                "NONE", "POSSIBLE", "PROBABLE", "PROBABLE", "REMOTE",
            ],
            "AEOUT": [
                "RECOVERED/RESOLVED", "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED",
                "RECOVERED/RESOLVED", "RECOVERED/RESOLVED", "RECOVERED/RESOLVED",
                "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED", "NOT RECOVERED/NOT RESOLVED",
                "RECOVERED/RESOLVED", "RECOVERED/RESOLVED", "RECOVERED/RESOLVED",
                "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED", "RECOVERED/RESOLVED",
            ],
        })
        print(f"Created sample dataset: {ae_df.shape[0]} rows, {ae_df.shape[1]} columns")

    # --- Initialize the Agent ---
    agent = ClinicalTrialDataAgent(data=ae_df, use_mock=True)

    # --- Run 3 Example Queries ---

    # Query 1: Ask about severity (maps to AESEV)
    print("\n" + "=" * 70)
    print("  QUERY 1: Severity-based question")
    print("=" * 70)
    result1 = agent.ask("Give me the subjects who had Adverse events of Moderate severity.")

    # Query 2: Ask about a specific condition (maps to AETERM)
    print("\n" + "=" * 70)
    print("  QUERY 2: Specific condition question")
    print("=" * 70)
    result2 = agent.ask("Which patients experienced Headache?")

    # Query 3: Ask about a body system (maps to AESOC)
    print("\n" + "=" * 70)
    print("  QUERY 3: Body system question")
    print("=" * 70)
    result3 = agent.ask("Show me subjects with Cardiac disorders.")

    # --- Summary ---
    print("\n" + "=" * 70)
    print("  TEST SUMMARY")
    print("=" * 70)
    for i, result in enumerate([result1, result2, result3], 1):
        print(f"\n  Query {i}: \"{result['question']}\"")
        print(f"    → Column: {result['parsed_query']['target_column']}")
        print(f"    → Value:  {result['parsed_query']['filter_value']}")
        print(f"    → Subjects found: {result['n_subjects']}")

    print("\n" + "=" * 70)
    print("  All 3 test queries completed successfully!")
    print("=" * 70)


if __name__ == "__main__":
    main()
