library(readr)
library(dplyr)
library(stringr)

edmus_normalize_names <- function(data) {
    data |>
        rename_with(\(x) x |>
            str_to_lower() |>
            str_replace("^([^a-z])", "_\\1") |>
            str_replace_all("(?:[^a-z0-9]|\\s)+", "_") |>
            str_replace("_*$", ""))
}

edmus_load <- function(path) {
    read_tsv(path,
        locale = locale(encoding = "UTF-16"),
        guess_max = 9999, na = c("", "?")
    ) |>
        edmus_normalize_names()
}

edmus_personal <- edmus_load("data/edmus-personal-220813_133309-DEN.txt") |>
    mutate(
        wait_and_see = as.logical(wait_and_see)
    )

edmus_diagnosis <- edmus_load("data/edmus-diagnosis-220811_121631-DEP.txt") |>
    mutate(
        disease_course = factor(case_match(
            disease_course,
            1:2 ~ "RR",
            3 ~ "SP-NR",
            4 ~ "SP-R",
            5 ~ "PP-NR",
            6:7 ~ "PP-R"
        ))
    )

edmus_clinical <- edmus_load("data/edmus-clinical-220811_122451-DEP.txt")
edmus_episodes <- edmus_load("data/edmus-episodes-220811_121723-DEP.txt")
