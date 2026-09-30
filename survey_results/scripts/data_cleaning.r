library(dplyr)
library(stringr)

response_path <- "survey_results/data/PMP_survey_responses_raw.tsv"
cleaned_path <- "survey_results/data/PMP_survey_responses_clean.parquet"

freq_fcts <- c("Always", "Frequently", "Sometimes", "Rarely", "Never")
cons_fcts <- c("Critical", "Major", "Moderate", "Minor", "Negligible")
appr_fcts <- c("Structured", "Ad-hoc", "Not Managed")
prev_fcts <- c("Yes", "No")
adop_fcts <- c("Yes", "No", "Not Sure")


responses_clean <- readr::read_tsv(response_path) |>
  # Strip additional text from consequences and approach questions
  mutate(across(ends_with("consequences"), ~str_remove(., pattern="[:blank:]-.*"))) |> 
  mutate(across(ends_with("approach"), ~str_remove(., pattern="[:blank:]-.*"))) |>
  # Convert character responses to factors
  mutate(across(ends_with("frequency"), ~factor(., levels = freq_fcts))) |>
  mutate(across(ends_with("consequences"), ~factor(., levels = cons_fcts))) |>
  mutate(across(ends_with("approach"), ~factor(., levels = appr_fcts))) |>
  mutate(across(ends_with("prev_use"), ~factor(., levels = prev_fcts))) |>
  mutate(across(ends_with("adopting"), ~factor(., levels = adop_fcts)))

arrow::write_parquet(responses_clean, cleaned_path)
  