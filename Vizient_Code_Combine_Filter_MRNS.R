# ============================================================================
# VIZIENT DATA FILTERING FOR SPECIFIC MRNs
# ============================================================================

# Load required packages
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(stringr)

# Define the CCC MRNs list
flag_cases <- c("1772396", "1591322", "880348", "795719", "1346547", "850935", "699837", "4030408",
                "4062987", "4080921", "1691845", "973522", "3987654", "4084923", "1772997", "3981461",
                "713184", "1733015", "4077671", "4014680", "4017462", "1646931", "697193", "4094651",
                "4081472", "4064821", "4065250", "675039", "4045148", "4080952", "1331021", "470406",
                "3886299", "1535850", "4093721", "4101634", "4068018", "3837642", 
                "4127191", "3870471", "1374241", "4138871", "4143218", "4137518", 
                "4045947", "3122411", "888523", "1267251")

# Set the base directory for Vizient data files
base_dir <- "C:/Users/watso/Box/Cost of Housing Unhoused in Hospital/Will_Melissa/Vizient_Data/"

# ============================================================================
# OPTIMIZED READ AND FILTER FUNCTION
# ============================================================================

# Function to read and filter Vizient Excel files
# - Only keeps rows with MRNs from flag_cases
# - Returns NULL if file can't be read or no matching records
# - UPDATED: Keeps all encounter types (inpatient, observation, ED)
read_and_filter_vizient <- function(file_path, ccc_mrns = flag_cases, skip_rows = 0) {
  tryCatch({
    cat("Processing", basename(file_path), "...")
    
    # Read only essential columns to save memory
    col_types <- "text" # Default to text for all columns
    
    # Read the file
    data <- read_excel(
      file_path, 
      skip = skip_rows,
      .name_repair = "minimal",
      col_types = col_types
    )
    
    # Find the MRN column name (might vary between files)
    mrn_col <- names(data)[grep("MRN|Medicare ID|Patient ID", names(data), ignore.case = TRUE)][1]
    
    if(is.na(mrn_col) || is.null(mrn_col)) {
      cat("ERROR: Could not find MRN column\n")
      return(NULL)
    }
    
    # Filter IMMEDIATELY to only keep rows with MRNs in flag_cases
    # No additional filtering by encounter type
    data <- data %>% 
      filter(!!sym(mrn_col) %in% ccc_mrns)
    
    # Report results
    n_records <- nrow(data)
    if(n_records > 0) {
      cat(n_records, "matching records found ✓\n")
      return(data)
    } else {
      cat("No matching records found\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# PROCESS ALL VIZIENT FILES
# ============================================================================

# Get all Excel files in the directory (including subdirectories)
all_files <- list.files(base_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

# Determine which files are "cleaned" (different row skipping needed)
cleaned_patterns <- "10-21|11-21|12-21|1-22|2-22|3-22|4-22|5-22|6-22"
is_cleaned <- grepl(cleaned_patterns, basename(all_files))

# Report the files to be processed
cat("Files to process:", length(all_files), "\n")
cat("Cleaned:", sum(is_cleaned), "| Uncleaned:", sum(!is_cleaned), "\n\n")

# Process files incrementally
output_file <- "ccc_vizient_data_all_encounters.csv"
first_file <- TRUE
total_rows <- 0

cat("=== Processing and filtering files ===\n")
start_time <- Sys.time()

for(i in seq_along(all_files)) {
  # Determine how many rows to skip based on file type
  skip_rows <- if(is_cleaned[i]) 0 else 31
  
  # Read and filter the file
  data <- read_and_filter_vizient(all_files[i], flag_cases, skip_rows)
  
  if(!is.null(data) && nrow(data) > 0) {
    # Add file source info
    data <- data %>%
      mutate(source_file = basename(all_files[i]))
    
    # Mark rows as from CCC list
    data <- data %>%
      mutate(Is_CCC = TRUE)
    
    # Add encounter type if available (for analysis, not filtering)
    if(any(grepl("Encounter Type|Visit Type", names(data), ignore.case = TRUE))) {
      encounter_col <- names(data)[grep("Encounter Type|Visit Type", names(data), ignore.case = TRUE)][1]
      data <- data %>%
        rename_with(~ "Encounter_Type", all_of(encounter_col))
    }
    
    # Write filtered data
    write.table(
      data, 
      output_file,
      sep = ",",
      row.names = FALSE,
      col.names = first_file,
      append = !first_file
    )
    
    first_file <- FALSE
    total_rows <- total_rows + nrow(data)
    
    # Progress report and garbage collection
    if(i %% 5 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      rate <- i / elapsed
      remaining <- (length(all_files) - i) / rate
      cat(sprintf("  [%d/%d | %.1f min left | %s total rows so far]\n", 
                  i, length(all_files), remaining, 
                  format(total_rows, big.mark=",")))
      gc(verbose = FALSE)
    }
  }
}

# Report final stats
elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat("\n=== Processing Complete ===\n")
cat("Time:", round(elapsed, 1), "minutes\n")
cat("Total CCC patient rows:", format(total_rows, big.mark=","), "\n")
cat("Saved to:", output_file, "\n")

# ============================================================================
# DATA CLEANING AND STANDARDIZATION
# ============================================================================

# This section loads the combined data and performs standardization
# If the file is too large to load at once, you can use data.table::fread with nrows

cat("\n=== Cleaning and Standardizing Combined Data ===\n")
start_time <- Sys.time()

# Load the combined data
cat("Loading combined data...\n")
combined_data <- tryCatch({
  read.csv(output_file, stringsAsFactors = FALSE)
}, error = function(e) {
  cat("ERROR loading combined data:", e$message, "\n")
  return(NULL)
})

if(!is.null(combined_data) && nrow(combined_data) > 0) {
  # Identify key columns - adjust these based on your actual column names
  cat("Standardizing column names...\n")
  
  # Standardize column names (find common columns regardless of capitalization)
  combined_data <- combined_data %>%
    rename_with(~ "MRN", matches("MRN|Medicare ID|Patient ID", ignore.case = TRUE)) %>%
    rename_with(~ "MS_DRG", matches("MS[ -]?DRG|Diagnosis Related Group", ignore.case = TRUE)) %>%
    rename_with(~ "LOS", matches("Length of Stay|LOS", ignore.case = TRUE)) %>%
    rename_with(~ "Expected_LOS", matches("Expected[ _]LOS|Expected Length of Stay", ignore.case = TRUE)) %>%
    rename_with(~ "Admission_Date", matches("Admission Date|Admit Date", ignore.case = TRUE)) %>%
    rename_with(~ "Discharge_Date", matches("Discharge Date|Disch Date", ignore.case = TRUE)) %>%
    rename_with(~ "Direct_Cost", matches("Direct Cost|Dir Cost", ignore.case = TRUE)) %>%
    rename_with(~ "Expected_Direct_Cost", matches("Expected Direct Cost|Exp Dir Cost", ignore.case = TRUE)) %>%
    rename_with(~ "Total_Cost", matches("Total Cost", ignore.case = TRUE))
  
  # Ensure all MRNs are character strings
  combined_data$MRN <- as.character(combined_data$MRN)
  
  # Clean up date columns if present
  if("Admission_Date" %in% names(combined_data)) {
    cat("Converting date columns...\n")
    
    # Handle multiple date formats - try several common formats
    combined_data <- combined_data %>%
      mutate(
        Admission_Date = case_when(
          # Try to parse the date using several formats
          !is.na(as.Date(Admission_Date, format = "%Y-%m-%d")) ~ 
            as.Date(Admission_Date, format = "%Y-%m-%d"),
          !is.na(as.Date(Admission_Date, format = "%m/%d/%Y")) ~ 
            as.Date(Admission_Date, format = "%m/%d/%Y"),
          !is.na(as.Date(Admission_Date, format = "%d-%b-%Y")) ~ 
            as.Date(Admission_Date, format = "%d-%b-%Y"),
          !is.na(mdy(Admission_Date)) ~ mdy(Admission_Date),
          TRUE ~ NA_Date_
        ),
        Discharge_Date = case_when(
          !is.na(as.Date(Discharge_Date, format = "%Y-%m-%d")) ~ 
            as.Date(Discharge_Date, format = "%Y-%m-%d"),
          !is.na(as.Date(Discharge_Date, format = "%m/%d/%Y")) ~ 
            as.Date(Discharge_Date, format = "%m/%d/%Y"),
          !is.na(as.Date(Discharge_Date, format = "%d-%b-%Y")) ~ 
            as.Date(Discharge_Date, format = "%d-%b-%Y"),
          !is.na(mdy(Discharge_Date)) ~ mdy(Discharge_Date),
          TRUE ~ NA_Date_
        )
      )
  }
  
  # Ensure numeric columns are actually numeric
  numeric_cols <- c("LOS", "Expected_LOS", "Direct_Cost", "Expected_Direct_Cost", "Total_Cost")
  for(col in numeric_cols) {
    if(col %in% names(combined_data)) {
      cat("Converting", col, "to numeric...\n")
      
      # Handle currency symbols and commas in numeric values
      combined_data[[col]] <- combined_data[[col]] %>%
        as.character() %>%
        str_replace_all("[$,]", "") %>%
        as.numeric()
    }
  }
  
  # Calculate key metrics if possible
  cat("Calculating derived metrics...\n")
  if(all(c("LOS", "Expected_LOS") %in% names(combined_data))) {
    combined_data <- combined_data %>%
      mutate(LOS_Ratio = LOS / Expected_LOS,
             LOS_Excess = LOS - Expected_LOS)
  }
  
  if(all(c("Direct_Cost", "Expected_Direct_Cost") %in% names(combined_data))) {
    combined_data <- combined_data %>%
      mutate(Cost_Ratio = Direct_Cost / Expected_Direct_Cost,
             Cost_Excess = Direct_Cost - Expected_Direct_Cost)
  }
  
  # Analyze encounter types if available
  if("Encounter_Type" %in% names(combined_data)) {
    cat("\nEncounter Type Summary:\n")
    encounter_summary <- combined_data %>%
      group_by(Encounter_Type) %>%
      summarize(
        Count = n(),
        Percentage = n() / nrow(combined_data) * 100,
        Avg_LOS = mean(LOS, na.rm = TRUE),
        Total_LOS = sum(LOS, na.rm = TRUE),
        Avg_Direct_Cost = mean(Direct_Cost, na.rm = TRUE),
        Total_Direct_Cost = sum(Direct_Cost, na.rm = TRUE)
      )
    
    print(encounter_summary)
  }
  
  # Save cleaned data
  cleaned_output <- "ccc_vizient_data_all_encounters_cleaned.csv"
  cat("Saving cleaned data to", cleaned_output, "...\n")
  write.csv(combined_data, cleaned_output, row.names = FALSE)
  
  # Report final stats for cleaned data
  cat("Cleaned data rows:", nrow(combined_data), "\n")
  cat("Cleaned data columns:", ncol(combined_data), "\n")
  
  # Report some basic statistics
  if("LOS" %in% names(combined_data)) {
    cat("\nLength of Stay Summary:\n")
    cat("  Mean: ", round(mean(combined_data$LOS, na.rm = TRUE), 1), "\n")
    cat("  Median: ", median(combined_data$LOS, na.rm = TRUE), "\n")
    cat("  Max: ", max(combined_data$LOS, na.rm = TRUE), "\n")
    cat("  Total: ", sum(combined_data$LOS, na.rm = TRUE), "\n")
  }
  
  if("Direct_Cost" %in% names(combined_data)) {
    cat("\nDirect Cost Summary:\n")
    cat("  Mean: $", format(round(mean(combined_data$Direct_Cost, na.rm = TRUE), 2), big.mark = ","), "\n")
    cat("  Median: $", format(round(median(combined_data$Direct_Cost, na.rm = TRUE), 2), big.mark = ","), "\n")
    cat("  Max: $", format(round(max(combined_data$Direct_Cost, na.rm = TRUE), 2), big.mark = ","), "\n")
    cat("  Total: $", format(round(sum(combined_data$Direct_Cost, na.rm = TRUE), 2), big.mark = ","), "\n")
  }
} else {
  cat("No data to clean or standardize.\n")
}

elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat("\n=== Cleaning Complete ===\n")
cat("Time:", round(elapsed, 1), "minutes\n")

# ============================================================================
# DATA VISUALIZATION SETUP
# ============================================================================

# Create a few simple plots to help understand the data
if(!is.null(combined_data) && nrow(combined_data) > 0) {
  library(ggplot2)
  
  cat("\n=== Creating Data Visualizations ===\n")
  
  # Setup directory for plots
  plots_dir <- "ccc_vizient_plots"
  if(!dir.exists(plots_dir)) dir.create(plots_dir)
  
  # 1. LOS distribution
  if("LOS" %in% names(combined_data)) {
    cat("Creating LOS distribution plot...\n")
    
    p1 <- ggplot(combined_data, aes(x = LOS)) +
      geom_histogram(fill = "#005A87", binwidth = 5) +
      theme_minimal() +
      labs(
        title = "Distribution of Length of Stay for CCC Patients",
        subtitle = "All encounter types included",
        x = "Length of Stay (days)",
        y = "Count"
      )
    
    ggsave(file.path(plots_dir, "los_distribution.png"), p1, width = 10, height = 6)
  }
  
  # 2. LOS vs Expected LOS by MS-DRG
  if(all(c("LOS", "Expected_LOS", "MS_DRG") %in% names(combined_data))) {
    cat("Creating LOS vs Expected LOS by MS-DRG plot...\n")
    
    # Get top MS-DRGs by frequency
    top_drgs <- combined_data %>%
      count(MS_DRG) %>%
      arrange(desc(n)) %>%
      head(15) %>%
      pull(MS_DRG)
    
    # Create plot for top MS-DRGs
    drg_summary <- combined_data %>%
      filter(MS_DRG %in% top_drgs) %>%
      group_by(MS_DRG) %>%
      summarize(
        Avg_LOS = mean(LOS, na.rm = TRUE),
        Avg_Expected_LOS = mean(Expected_LOS, na.rm = TRUE),
        Count = n()
      ) %>%
      arrange(desc(Count))
    
    p2 <- ggplot(drg_summary, aes(x = reorder(MS_DRG, Count))) +
      geom_bar(aes(y = Avg_LOS, fill = "Observed"), stat = "identity", position = "dodge") +
      geom_bar(aes(y = Avg_Expected_LOS, fill = "Expected"), stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Observed" = "#E84A27", "Expected" = "#005A87")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Observed vs Expected Length of Stay by MS-DRG",
        subtitle = "For top 15 most frequent MS-DRGs - All encounter types included",
        x = "MS-DRG",
        y = "Average Length of Stay (days)",
        fill = "Type"
      )
    
    ggsave(file.path(plots_dir, "los_by_msdrg.png"), p2, width = 12, height = 7)
  }
  
  # 3. Cost distribution
  if("Direct_Cost" %in% names(combined_data)) {
    cat("Creating cost distribution plot...\n")
    
    p3 <- ggplot(combined_data, aes(x = Direct_Cost)) +
      geom_histogram(fill = "#007FB1", bins = 30) +
      scale_x_continuous(labels = scales::dollar_format()) +
      theme_minimal() +
      labs(
        title = "Distribution of Direct Costs for CCC Patients",
        subtitle = "All encounter types included",
        x = "Direct Cost",
        y = "Count"
      )
    
    ggsave(file.path(plots_dir, "cost_distribution.png"), p3, width = 10, height = 6)
  }
  
  # 4. Encounter type breakdown (if available)
  if("Encounter_Type" %in% names(combined_data)) {
    cat("Creating encounter type plots...\n")
    
    # Count by encounter type
    p4 <- combined_data %>%
      count(Encounter_Type) %>%
      ggplot(aes(x = reorder(Encounter_Type, n), y = n, fill = Encounter_Type)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      labs(
        title = "CCC Patient Encounters by Type",
        x = "Encounter Type",
        y = "Count"
      )
    
    ggsave(file.path(plots_dir, "encounter_types.png"), p4, width = 10, height = 6)
    
    # Cost by encounter type
    if("Direct_Cost" %in% names(combined_data)) {
      p5 <- combined_data %>%
        group_by(Encounter_Type) %>%
        summarize(
          Total_Cost = sum(Direct_Cost, na.rm = TRUE),
          Avg_Cost = mean(Direct_Cost, na.rm = TRUE)
        ) %>%
        ggplot(aes(x = reorder(Encounter_Type, Total_Cost), y = Total_Cost, fill = Encounter_Type)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Blues") +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        labs(
          title = "Total Direct Cost by Encounter Type for CCC Patients",
          x = "Encounter Type",
          y = "Total Direct Cost"
        )
      
      ggsave(file.path(plots_dir, "cost_by_encounter.png"), p5, width = 10, height = 6)
    }
  }
  
  cat("Visualizations saved to", plots_dir, "directory\n")
}

cat("\n=== All Processing Complete ===\n")