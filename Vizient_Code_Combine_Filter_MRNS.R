library(readxl)
library(dplyr)
library(stringr)

# Define CCC Patient IDs
flag_cases_raw <- c("1772396", "1591322", "880348", "795719", "1346547", "850935", 
                    "699837", "4030408","4062987", "4080921", "1691845", "973522", 
                    "3987654", "4084923", 
                    "1772997", "3981461","713184", "1733015", "4077671", "4014680", 
                    "4017462", "1646931", 
                    "697193", "4094651","4081472", "4064821", "4065250", "675039", 
                    "4045148", "4080952", 
                    "1331021", "470406","3886299", "1535850", "4093721", "4101634", 
                    "4068018", "3837642", 
                    "4127191", "3870471", "1374241", "4138871", "4143218", "4137518", 
                    "4045947", "3122411", "888523", "1267251", 
                    "004296383", "00282489",
                    "001014542", "003905334","003831307", "001529462", "003989308", 
                    "003094083", "001542364", 
                    "001723512", "003933450", "004336254")

flag_cases <- str_pad(flag_cases_raw, width = 9, side = "left", pad = "0")

cat("Searching for", length(flag_cases), "CCC Patient IDs\n\n")

base_dir <- "R:/IQVIA PharMetrics Plus (2024) Members/WatsonWilliam/Bailey_Data/"

# Get all files
all_files <- list.files(base_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
filenames <- basename(all_files)

# Classify files
cleaned_patterns <- "10-21|11-21|12-21|1-22|2-22|3-22|4-22|5-22|6-22"
is_cleaned <- grepl(cleaned_patterns, filenames)

cat("Files:", length(all_files), "(", sum(is_cleaned), "cleaned,", sum(!is_cleaned), "uncleaned)\n\n")

# COMPREHENSIVE SEARCH
cat("=== SEARCHING ALL FILES FOR CCC PATIENTS ===\n\n")

all_ccc_data <- list()
found_patients <- character()
files_with_data <- 0

start_time <- Sys.time()

for(i in seq_along(all_files)) {
  skip_rows <- if(is_cleaned[i]) 0 else 31
  
  tryCatch({
    cat(basename(all_files[i]), "...")
    
    # Read file
    data <- read_excel(all_files[i], skip = skip_rows, .name_repair = "minimal")
    
    # Find Patient ID column (should be exact match)
    patient_id_col <- "Patient ID"
    
    if(!patient_id_col %in% names(data)) {
      cat("NO 'Patient ID' column found\n")
      next
    }
    
    # Pad Patient IDs to 9 digits
    data[[patient_id_col]] <- str_pad(as.character(data[[patient_id_col]]), 
                                      width = 9, side = "left", pad = "0")
    
    # Filter to CCC patients
    ccc_records <- data %>%
      filter(!!sym(patient_id_col) %in% flag_cases)
    
    if(nrow(ccc_records) > 0) {
      # Track unique patients found in this file
      patients_in_file <- unique(ccc_records[[patient_id_col]])
      found_patients <- unique(c(found_patients, patients_in_file))
      
      # Add source tracking
      ccc_records$source_file <- basename(all_files[i])
      
      # Store
      all_ccc_data[[length(all_ccc_data) + 1]] <- ccc_records
      files_with_data <- files_with_data + 1
      
      cat(nrow(ccc_records), "records,", length(patients_in_file), "unique patients ✓\n")
    } else {
      cat("0 CCC patients\n")
    }
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
  
  # Progress update
  if(i %% 10 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    cat(sprintf("\n[%d/%d files checked | %.1f min | %d patients found so far]\n\n", 
                i, length(all_files), elapsed, length(found_patients)))
  }
}

elapsed <- difftime(Sys.time(), start_time, units = "mins")

cat("\n=== SEARCH COMPLETE ===\n")
cat("Time:", round(elapsed, 1), "minutes\n")
cat("Files with CCC data:", files_with_data, "\n")
cat("Unique CCC patients found:", length(found_patients), "out of", length(flag_cases), "\n\n")

# Combine all data
if(length(all_ccc_data) > 0) {
  combined_ccc <- bind_rows(all_ccc_data)
  
  cat("Total CCC records:", nrow(combined_ccc), "\n\n")
  
  # Show which patients were found
  cat("CCC Patients FOUND (", length(found_patients), "):\n")
  print(sort(found_patients))
  
  # Show which patients were NOT found
  missing_patients <- setdiff(flag_cases, found_patients)
  cat("\n\nCCC Patients NOT FOUND (", length(missing_patients), "):\n")
  print(sort(missing_patients))
  
  # Save combined data
  write.csv(combined_ccc, "ccc_patients_ALL_COMPREHENSIVE.csv", row.names = FALSE)
  cat("\n✓ Saved to: ccc_patients_ALL_COMPREHENSIVE.csv\n")
  
  # Summary by patient
  patient_summary <- combined_ccc %>%
    count(`Patient ID`) %>%
    arrange(desc(n))
  
  cat("\nRecords per patient (top 20):\n")
  print(head(patient_summary, 20))
  
} else {
  cat("ERROR: No CCC patient data found!\n")
}