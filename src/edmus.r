library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

source("src/common.r")

edmus_load <- function(path) {
    read_tsv(path,
        locale = locale(encoding = "UTF-16"),
        guess_max = 9999, na = c("", "?")
    ) |> normalize_names()
}

edmus_personal <- edmus_load("data/edmus-personal-220813_133309-DEN.txt") |>
    mutate(
        wait_and_see = as.logical(wait_and_see),
        across(c(
            date_of_birth, first_exam, date_consent_form, created,
            last_modified, last_info, last_clinical_assessment,
            starts_with("irreversible_") & !ends_with("_unknown_date"),
            ends_with("_date") & !ends_with("_unknown_date") & -unknown_decease_date
        ), dmy)
    ) |>
    drop_na(patient_id) |>
    select(-ms_onset)

edmus_diagnosis <- edmus_load("data/edmus-diagnosis-220811_121631-DEP.txt") |>
    mutate(
        across(ms_onset, dmy),
        disease_course = factor(case_match(
            disease_course,
            1:2 ~ "RR",
            3 ~ "SP-NR",
            4 ~ "SP-R",
            5 ~ "PP-NR",
            6:7 ~ "PP-R"
        ))
    )

edmus_clinical <- edmus_load("data/edmus-clinical-220811_122451-DEP.txt") |>
    mutate(across(date, dmy))

edmus_episodes <- edmus_load("data/edmus-episodes-220811_121723-DEP.txt") |>
    mutate(across(date, dmy))

edmus_studies <- edmus_load("data/edmus-study-230405_130411-DEN.txt")
edmus_protocols <- edmus_load("data/edmus-protocol-230405_130423-DEN.txt")

edmus_trt_dm <- edmus_load("data/edmus-trt_dm-230405_130357-DEN.txt") |>
    mutate(across(ends_with("_date"), dmy))
