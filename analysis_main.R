# analysis_main.R
# Reproducibility script for:
# "Skeletal Structure and Makeup Effects in Facial Attractiveness Judgments"
#
# This script uses only the two public CSV files:
#   1. rating_data_anonymized.csv
#   2. derived_face_measures.csv
#
# It reproduces the descriptive summaries and cumulative link mixed-effects
# models reported in the article.
#
# Note:
# - The original CFD photographs, rendered stimulus images, 3D meshes,
#   raw landmark coordinates, and mapping to original CFD model IDs are not used.
# - The variable face_id is a study-specific anonymized identifier.

# -------------------------------------------------------------------------
# 0. Setup
# -------------------------------------------------------------------------

INSTALL_MISSING_PACKAGES <- TRUE

required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "ordinal"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  if (INSTALL_MISSING_PACKAGES) {
    install.packages(missing_packages, repos = "https://cloud.r-project.org")
  } else {
    stop(
      "The following packages are required but not installed: ",
      paste(missing_packages, collapse = ", ")
    )
  }
}

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ordinal)

# -------------------------------------------------------------------------
# 1. Input/output paths
# -------------------------------------------------------------------------

# The script first looks for the files in the current working directory.
# If not found, it also looks in a data/ subdirectory.
find_input_file <- function(filename) {
  candidates <- c(
    file.path(".", filename),
    file.path("data", filename)
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop("Cannot find input file: ", filename,
         "\nPlace it in the working directory or in a data/ subdirectory.")
  }
  found[[1]]
}

rating_file <- find_input_file("rating_data_anonymized.csv")
derived_file <- find_input_file("derived_face_measures.csv")

output_dir <- "analysis_outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

message("Using rating file: ", rating_file)
message("Using derived face measures file: ", derived_file)
message("Writing outputs to: ", output_dir)

# -------------------------------------------------------------------------
# 2. Read data
# -------------------------------------------------------------------------

rating_raw <- read_csv(rating_file, show_col_types = FALSE)
derived_raw <- read_csv(derived_file, show_col_types = FALSE)

# Required columns
required_rating_cols <- c(
  "rater_id",
  "face_id",
  "rating",
  "makeup",
  "skeletal_group",
  "ethnicity",
  "rater_gender",
  "rater_age_group",
  "makeup_frequency",
  "makeup_interest"
)

required_derived_cols <- c(
  "face_id",
  "ethnicity",
  "r_mid",
  "r_jaw_adj",
  "z_r_mid",
  "z_r_jaw_adj",
  "skeletal_group",
  "type_code"
)

missing_rating_cols <- setdiff(required_rating_cols, names(rating_raw))
missing_derived_cols <- setdiff(required_derived_cols, names(derived_raw))

if (length(missing_rating_cols) > 0) {
  stop("rating_data_anonymized.csv is missing columns: ",
       paste(missing_rating_cols, collapse = ", "))
}
if (length(missing_derived_cols) > 0) {
  stop("derived_face_measures.csv is missing columns: ",
       paste(missing_derived_cols, collapse = ", "))
}

# -------------------------------------------------------------------------
# 3. Basic consistency checks
# -------------------------------------------------------------------------

rating_face_ids <- sort(unique(rating_raw$face_id))
derived_face_ids <- sort(unique(derived_raw$face_id))

missing_in_derived <- setdiff(rating_face_ids, derived_face_ids)
unused_in_rating <- setdiff(derived_face_ids, rating_face_ids)

if (length(missing_in_derived) > 0) {
  warning("Some face_id values in rating_data_anonymized.csv are missing from derived_face_measures.csv: ",
          paste(missing_in_derived, collapse = ", "))
}
if (length(unused_in_rating) > 0) {
  warning("Some face_id values in derived_face_measures.csv are not used in rating_data_anonymized.csv: ",
          paste(unused_in_rating, collapse = ", "))
}

# Check whether ethnicity and skeletal_group are consistent by face_id.
rating_face_info <- rating_raw %>%
  distinct(face_id, ethnicity, skeletal_group)

derived_face_info <- derived_raw %>%
  distinct(face_id, ethnicity, skeletal_group) %>%
  rename(
    ethnicity_derived = ethnicity,
    skeletal_group_derived = skeletal_group
  )

face_info_check <- rating_face_info %>%
  left_join(derived_face_info, by = "face_id") %>%
  mutate(
    ethnicity_match = ethnicity == ethnicity_derived,
    skeletal_group_match = skeletal_group == skeletal_group_derived
  )

write_csv(face_info_check, file.path(output_dir, "check_face_id_consistency.csv"))

# Check repeated presentation of the same face_id to the same rater.
repeated_face_by_rater <- rating_raw %>%
  count(rater_id, face_id, name = "n_presentations") %>%
  filter(n_presentations > 1) %>%
  arrange(desc(n_presentations), rater_id, face_id)

write_csv(repeated_face_by_rater, file.path(output_dir, "check_repeated_face_by_rater.csv"))

# General data checks
data_checks <- tibble(
  item = c(
    "n_rating_rows",
    "n_raters",
    "n_faces_in_rating_data",
    "n_faces_in_derived_data",
    "n_missing_values_rating",
    "n_missing_values_derived",
    "n_repeated_rater_face_pairs"
  ),
  value = c(
    nrow(rating_raw),
    n_distinct(rating_raw$rater_id),
    n_distinct(rating_raw$face_id),
    n_distinct(derived_raw$face_id),
    sum(is.na(rating_raw)),
    sum(is.na(derived_raw)),
    nrow(repeated_face_by_rater)
  )
)

write_csv(data_checks, file.path(output_dir, "data_checks.csv"))

# -------------------------------------------------------------------------
# 4. Prepare analysis data
# -------------------------------------------------------------------------

analysis_data <- rating_raw %>%
  mutate(
    rating_ord = ordered(rating, levels = 1:6),
    makeup_f = factor(makeup, levels = c(0, 1),
                      labels = c("no_makeup", "makeup")),
    skeletal_group = factor(skeletal_group, levels = c("low", "high")),
    ethnicity = factor(ethnicity,
                       levels = c("Asian American", "Black", "Latina", "White")),
    rater_gender = factor(rater_gender, levels = c("male", "female")),
    makeup_frequency = factor(makeup_frequency),
    makeup_interest = factor(makeup_interest),
    rater_id = factor(rater_id),
    face_id = factor(face_id)
  )

# Preserve the intended age ordering when these labels are present.
age_levels <- c("under 20", "20‐29", "30‐39", "40‐49", "50‐59", "60‐69", "70 and above")
if (all(unique(analysis_data$rater_age_group) %in% age_levels)) {
  analysis_data <- analysis_data %>%
    mutate(rater_age_group = factor(rater_age_group, levels = age_levels))
} else {
  analysis_data <- analysis_data %>%
    mutate(rater_age_group = factor(rater_age_group))
}

# -------------------------------------------------------------------------
# 5. Descriptive statistics
# -------------------------------------------------------------------------

# Rater attributes
rater_summary <- analysis_data %>%
  distinct(rater_id, rater_gender, rater_age_group, makeup_frequency, makeup_interest) %>%
  summarise(
    n_raters = n(),
    .by = c(rater_gender, rater_age_group, makeup_frequency, makeup_interest)
  ) %>%
  arrange(rater_age_group, rater_gender, makeup_frequency, makeup_interest)

write_csv(rater_summary, file.path(output_dir, "rater_attribute_summary.csv"))

# Counts by condition
condition_counts <- analysis_data %>%
  count(skeletal_group, makeup_f, ethnicity, name = "n")

write_csv(condition_counts, file.path(output_dir, "condition_counts.csv"))

# Descriptive statistics corresponding to the skeletal_group x makeup condition table.
descriptive_by_condition <- analysis_data %>%
  group_by(skeletal_group, makeup_f) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating),
    sd_rating = sd(rating),
    median_rating = median(rating),
    iqr_rating = IQR(rating),
    .groups = "drop"
  )

write_csv(descriptive_by_condition, file.path(output_dir, "descriptive_by_condition.csv"))

# Rating distribution by skeletal group and makeup condition.
rating_distribution <- analysis_data %>%
  count(skeletal_group, makeup_f, rating, name = "n") %>%
  group_by(skeletal_group, makeup_f) %>%
  mutate(
    total = sum(n),
    percentage = 100 * n / total
  ) %>%
  ungroup()

write_csv(rating_distribution, file.path(output_dir, "rating_distribution.csv"))

# Derived face measure summary
derived_summary <- derived_raw %>%
  summarise(
    n_faces = n(),
    n_face_ids = n_distinct(face_id),
    mean_r_mid = mean(r_mid),
    sd_r_mid = sd(r_mid),
    mean_r_jaw_adj = mean(r_jaw_adj),
    sd_r_jaw_adj = sd(r_jaw_adj),
    mean_z_r_mid = mean(z_r_mid),
    sd_z_r_mid = sd(z_r_mid),
    mean_z_r_jaw_adj = mean(z_r_jaw_adj),
    sd_z_r_jaw_adj = sd(z_r_jaw_adj)
  )

write_csv(derived_summary, file.path(output_dir, "derived_face_measure_summary.csv"))

derived_counts <- derived_raw %>%
  count(ethnicity, skeletal_group, type_code, name = "n") %>%
  arrange(ethnicity, skeletal_group, type_code)

write_csv(derived_counts, file.path(output_dir, "derived_face_measure_counts.csv"))

# -------------------------------------------------------------------------
# 6. Cumulative link mixed-effects models
# -------------------------------------------------------------------------

# The main model corresponds to the reported primary analysis.
# Fixed effects: makeup, skeletal group, their interaction, rater gender, and age group.
# Random intercepts: rater and face model.
main_model <- clmm(
  rating_ord ~ makeup_f * skeletal_group + rater_gender + rater_age_group +
    (1 | rater_id) + (1 | face_id),
  data = analysis_data,
  link = "logit",
  Hess = TRUE,
  nAGQ = 1
)

saveRDS(main_model, file.path(output_dir, "main_clmm_model.rds"))

main_model_summary <- summary(main_model)

# Fixed-effect coefficient table
main_coef <- as.data.frame(coef(main_model_summary)) %>%
  rownames_to_column("term") %>%
  mutate(
    odds_ratio = exp(Estimate)
  )

write_csv(main_coef, file.path(output_dir, "main_clmm_coefficients.csv"))

# Thresholds
main_thresholds <- as.data.frame(main_model_summary$alpha) %>%
  rownames_to_column("threshold")
names(main_thresholds)[2] <- "estimate"

write_csv(main_thresholds, file.path(output_dir, "main_clmm_thresholds.csv"))

# Random effects / variance components
main_varcorr <- capture.output(print(VarCorr(main_model)))
writeLines(main_varcorr, file.path(output_dir, "main_clmm_varcorr.txt"))

# Model fit
main_model_fit <- tibble(
  model = "main_model",
  logLik = as.numeric(logLik(main_model)),
  AIC = AIC(main_model),
  BIC = BIC(main_model),
  nobs = nobs(main_model)
)

write_csv(main_model_fit, file.path(output_dir, "main_clmm_fit.csv"))

# -------------------------------------------------------------------------
# 7. Extended model including ethnicity
# -------------------------------------------------------------------------

# The extended model checks whether the main results are robust when ethnicity
# and its interactions with makeup and skeletal group are included.
extended_model <- clmm(
  rating_ord ~ makeup_f * skeletal_group * ethnicity +
    rater_gender + rater_age_group +
    (1 | rater_id) + (1 | face_id),
  data = analysis_data,
  link = "logit",
  Hess = TRUE,
  nAGQ = 1
)

saveRDS(extended_model, file.path(output_dir, "extended_ethnicity_clmm_model.rds"))

extended_model_summary <- summary(extended_model)

extended_coef <- as.data.frame(coef(extended_model_summary)) %>%
  rownames_to_column("term") %>%
  mutate(
    odds_ratio = exp(Estimate)
  )

write_csv(extended_coef, file.path(output_dir, "extended_ethnicity_clmm_coefficients.csv"))

extended_thresholds <- as.data.frame(extended_model_summary$alpha) %>%
  rownames_to_column("threshold")
names(extended_thresholds)[2] <- "estimate"

write_csv(extended_thresholds, file.path(output_dir, "extended_ethnicity_clmm_thresholds.csv"))

extended_varcorr <- capture.output(print(VarCorr(extended_model)))
writeLines(extended_varcorr, file.path(output_dir, "extended_ethnicity_clmm_varcorr.txt"))

extended_model_fit <- tibble(
  model = "extended_ethnicity_model",
  logLik = as.numeric(logLik(extended_model)),
  AIC = AIC(extended_model),
  BIC = BIC(extended_model),
  nobs = nobs(extended_model)
)

write_csv(extended_model_fit, file.path(output_dir, "extended_ethnicity_clmm_fit.csv"))

# -------------------------------------------------------------------------
# 8. Optional model comparison
# -------------------------------------------------------------------------

model_comparison <- bind_rows(main_model_fit, extended_model_fit)
write_csv(model_comparison, file.path(output_dir, "model_comparison.csv"))

# -------------------------------------------------------------------------
# 9. Session information
# -------------------------------------------------------------------------

session_info <- capture.output(sessionInfo())
writeLines(session_info, file.path(output_dir, "sessionInfo.txt"))

message("Analysis complete. Output files were written to: ", output_dir)
