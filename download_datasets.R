
# download_datasets.R
# Purpose: Download and prepare new datasets for the expanded experiment.
# Required format: data.frame with features + target column (factor, levels "0" and "1").
# "1" = minority class, "0" = majority class.
# Usage: source("download_datasets.R")  (run from the RF-GCM project root)

if (!exists("DATASET_DIR")) source("config.R")
output_dir = DATASET_DIR
if (!dir.exists(output_dir)) dir.create(output_dir)

# Helper: save dataset with standard checks
save_dataset = function(df, target_col, dta_name) {
  # Ensure target is factor with levels "0" and "1"
  stopifnot(is.factor(df[[target_col]]))
  stopifnot(all(levels(df[[target_col]]) == c("0", "1")))

  # Remove any row names
  rownames(df) = NULL

  # Report
  n = nrow(df)
  p = ncol(df) - 1
  prop1 = mean(df[[target_col]] == "1")
  cat(sprintf("  Saved %s: %d instances, %d features, minority prop = %.5f\n", dta_name, n, p, prop1))

  saveRDS(df, file.path(output_dir, paste0(dta_name, ".rds")))
}


# ---------- 1. abalone19 ----------
cat("1. Downloading abalone19...\n")
abalone_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
abalone = read.csv(abalone_url, header = FALSE, stringsAsFactors = TRUE)
colnames(abalone) = c("Sex", "Length", "Diameter", "Height", "Whole_weight",
                       "Shucked_weight", "Viscera_weight", "Shell_weight", "Rings")
# Target: Rings == 19 (extreme minority)
abalone$target = factor(ifelse(abalone$Rings == 19, "1", "0"), levels = c("0", "1"))
abalone$Rings = NULL
save_dataset(abalone, "target", "abalone19")


# ---------- 2. page_blocks ----------
cat("2. Downloading page_blocks...\n")
pb_url = "https://archive.ics.uci.edu/static/public/78/page+blocks+classification.zip"
pb_zip = tempfile(fileext = ".zip")
download.file(pb_url, pb_zip, method = "curl", quiet = TRUE)
pb_dir = tempfile(pattern = "pb_")
dir.create(pb_dir)
unzip(pb_zip, exdir = pb_dir)
pb_files = list.files(pb_dir, full.names = TRUE, recursive = TRUE)
cat(sprintf("  Unzipped files: %s\n", paste(basename(pb_files), collapse = ", ")))
# The .data.Z file needs to be decompressed with uncompress/gunzip
pb_z_file = pb_files[grepl("\\.data\\.Z$", pb_files)]
if (length(pb_z_file) > 0) {
  # Decompress .Z file
  system2("uncompress", args = shQuote(pb_z_file), stdout = TRUE, stderr = TRUE)
  pb_data_file = sub("\\.Z$", "", pb_z_file)
} else {
  pb_data_file = pb_files[grepl("\\.data$", pb_files)]
}
cat(sprintf("  Reading: %s\n", pb_data_file))
page_blocks = read.table(pb_data_file, header = FALSE, stringsAsFactors = FALSE)
colnames(page_blocks) = c("height", "lenght", "area", "eccen", "p_black", "p_and",
                           "mean_tr", "blackpix", "blackand", "wb_trans", "class")
# Target: class 5 (very rare block type) vs rest
page_blocks$target = factor(ifelse(page_blocks$class == 5, "1", "0"), levels = c("0", "1"))
page_blocks$class = NULL
save_dataset(page_blocks, "target", "page_blocks")


# ---------- 3. mammography ----------
cat("3. Downloading mammography...\n")
if (!requireNamespace("foreign", quietly = TRUE)) install.packages("foreign")
mammo_url = "https://www.openml.org/data/download/52214/phpn1jVwe"
mammo_tmp = tempfile(fileext = ".arff")
download.file(mammo_url, mammo_tmp, method = "curl", quiet = TRUE)
mammo = foreign::read.arff(mammo_tmp)
# Identify target column (last column)
target_col_mammo = colnames(mammo)[ncol(mammo)]
mammo_target = mammo[[target_col_mammo]]
cat(sprintf("  Target col: %s, levels: %s, table: %s\n",
            target_col_mammo, paste(levels(mammo_target), collapse=","),
            paste(table(mammo_target), collapse=",")))
# Minority class -> "1"
minority_label = names(which.min(table(mammo_target)))
mammo$target = factor(ifelse(mammo_target == minority_label, "1", "0"), levels = c("0", "1"))
mammo[[target_col_mammo]] = NULL
# Ensure all features are numeric
for (col in setdiff(colnames(mammo), "target")) {
  mammo[[col]] = as.numeric(mammo[[col]])
}
save_dataset(mammo, "target", "mammography")


# ---------- 4. sick (thyroid disease) ----------
cat("4. Downloading sick...\n")
sick_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/sick.data"
sick_tmp = tempfile(fileext = ".data")
download.file(sick_url, sick_tmp, method = "curl", quiet = TRUE)
sick_raw = readLines(sick_tmp, warn = FALSE)

# Parse: comma-separated, last field is "class.|ID"
sick_list = lapply(sick_raw, function(line) {
  vals = strsplit(line, ",")[[1]]
  # Last element: "sick.|1234" or "negative.|5678"
  last = vals[length(vals)]
  class_val = sub("\\.[^.]*$", "", last)
  vals[length(vals)] = class_val
  return(vals)
})

# Filter to rows with exactly 30 fields
n_fields = sapply(sick_list, length)
sick_list = sick_list[n_fields == 30]
sick_df = as.data.frame(do.call(rbind, sick_list), stringsAsFactors = FALSE)

sick_colnames = c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication",
                  "sick_flag", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid",
                  "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary",
                  "psych", "TSH_measured", "TSH", "T3_measured", "T3",
                  "TT4_measured", "TT4", "T4U_measured", "T4U", "FTI_measured",
                  "FTI", "TBG_measured", "TBG", "referral_source", "class")
colnames(sick_df) = sick_colnames

# Replace "?" with NA
sick_df[sick_df == "?"] = NA

# Target: "sick" -> 1, everything else -> 0
sick_df$target = factor(ifelse(trimws(sick_df$class) == "sick", "1", "0"), levels = c("0", "1"))
sick_df$class = NULL

# Convert numeric columns
num_cols = c("age", "TSH", "T3", "TT4", "T4U", "FTI", "TBG")
for (col in num_cols) {
  sick_df[[col]] = as.numeric(sick_df[[col]])
}

# Convert binary t/f columns to factor
binary_cols = c("sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication",
                "sick_flag", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid",
                "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary",
                "psych", "TSH_measured", "T3_measured", "TT4_measured", "T4U_measured",
                "FTI_measured", "TBG_measured")
for (col in binary_cols) {
  sick_df[[col]] = as.factor(sick_df[[col]])
}
sick_df$referral_source = as.factor(sick_df$referral_source)

# Remove TBG column (almost all NA)
sick_df$TBG = NULL
sick_df$TBG_measured = NULL

# Remove remaining rows with NA
sick_df = na.omit(sick_df)
save_dataset(sick_df, "target", "sick")


# ---------- 5. satimage ----------
cat("5. Downloading satimage...\n")
sat_train_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/satimage/sat.trn"
sat_test_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/satimage/sat.tst"
sat_train = read.table(sat_train_url, header = FALSE)
sat_test = read.table(sat_test_url, header = FALSE)
satimage = rbind(sat_train, sat_test)
colnames(satimage) = c(paste0("f_", 1:36), "class")
# Target: class 4 (damp grey soil) is minority (~9.7%)
satimage$target = factor(ifelse(satimage$class == 4, "1", "0"), levels = c("0", "1"))
satimage$class = NULL
save_dataset(satimage, "target", "satimage")


# ---------- 6. letter_A ----------
cat("6. Downloading letter_A...\n")
letter_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data"
letter = read.csv(letter_url, header = FALSE, stringsAsFactors = FALSE)
colnames(letter) = c("letter", paste0("f_", 1:16))
# Target: letter "A" vs rest (~3.9%)
letter$target = factor(ifelse(letter$letter == "A", "1", "0"), levels = c("0", "1"))
letter$letter = NULL
save_dataset(letter, "target", "letter_A")


# ---------- 7. ozone_level ----------
cat("7. Downloading ozone_level...\n")
ozone_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/ozone/eighthr.data"
ozone_tmp = tempfile()
download.file(ozone_url, ozone_tmp, method = "curl", quiet = TRUE)
ozone = read.csv(ozone_tmp, header = FALSE, stringsAsFactors = FALSE, na.strings = "?")
# First column is date, last column is target
colnames(ozone) = c("date", paste0("f_", 1:(ncol(ozone)-2)), "target_raw")
ozone$date = NULL

# Convert all features to numeric
for (col in setdiff(colnames(ozone), "target_raw")) {
  ozone[[col]] = as.numeric(ozone[[col]])
}

# Remove columns with >50% NA
na_prop = colMeans(is.na(ozone[, setdiff(colnames(ozone), "target_raw")]))
drop_cols = names(na_prop[na_prop >= 0.5])
if (length(drop_cols) > 0) ozone[drop_cols] = NULL

# Remove rows with NA
ozone = na.omit(ozone)

# Target: 1 = ozone day (minority)
ozone$target = factor(ifelse(ozone$target_raw == 1, "1", "0"), levels = c("0", "1"))
ozone$target_raw = NULL
save_dataset(ozone, "target", "ozone_level")


# ---------- 8. arrhythmia ----------
cat("8. Downloading arrhythmia...\n")
arr_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data"
arr = read.csv(arr_url, header = FALSE, stringsAsFactors = FALSE, na.strings = "?")
colnames(arr) = c(paste0("f_", 1:(ncol(arr)-1)), "class")

# Use class 6 as minority target (rare arrhythmia subtype, ~25 instances)
# This gives meaningful imbalance in a high-dimensional setting
cat(sprintf("  Class 6 count: %d / %d\n", sum(arr$class == 6, na.rm=TRUE), nrow(arr)))
arr$target = factor(ifelse(arr$class == 6, "1", "0"), levels = c("0", "1"))
arr$class = NULL

# Convert all to numeric
for (col in setdiff(colnames(arr), "target")) {
  arr[[col]] = as.numeric(arr[[col]])
}

# Remove columns with >50% NA
na_prop = colMeans(is.na(arr[, setdiff(colnames(arr), "target")]))
drop_cols = names(na_prop[na_prop >= 0.5])
if (length(drop_cols) > 0) {
  cat(sprintf("  Dropping %d cols with >50%% NA\n", length(drop_cols)))
  arr[drop_cols] = NULL
}

# Remove rows with NA
arr = na.omit(arr)
save_dataset(arr, "target", "arrhythmia")


# ---------- 9. wine_quality ----------
cat("9. Downloading wine_quality...\n")
wine_red_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
wine_white_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine_red = read.csv(wine_red_url, sep = ";", header = TRUE)
wine_white = read.csv(wine_white_url, sep = ";", header = TRUE)
wine = rbind(wine_red, wine_white)
# Target: quality >= 8 is "excellent" (minority, ~3%)
wine$target = factor(ifelse(wine$quality >= 8, "1", "0"), levels = c("0", "1"))
wine$quality = NULL
save_dataset(wine, "target", "wine_quality")


cat("\n=== Download complete ===\n")
cat("Files in dataset/:\n")
rds_files = list.files(output_dir, pattern = "\\.rds$")
print(rds_files)
