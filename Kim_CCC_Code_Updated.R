# Import libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(scales)
library(patchwork)
library(gt)
library(stringr)  # Added for str_pad

# Import data
readmissions <- read_excel("R:/IQVIA PharMetrics Plus (2024) Members/WatsonWilliam/Bailey_Data/Selected Readmissions 07.04.23 - 10.14.25.xlsx")
admissions <- read_excel("R:/IQVIA PharMetrics Plus (2024) Members/WatsonWilliam/Bailey_Data/Selected Admissions - 11.28.22 - 10.14.25.xlsx")

# Remove extra rows
readmissions <- readmissions[1:3006,]
admissions <- admissions[1:42978,]

# Full join the two data tables
data <- full_join(admissions, readmissions, by=NULL)

# Clean names
data <- clean_names(data)

# Ensure correct data types for variables of interest
data$los <- as.numeric(data$los)
data$expected_vizient_los <- as.numeric(data$expected_vizient_los)
data$observed_direct_cost <- as.numeric(gsub(",", "", data$observed_direct_cost))
data$expected_direct_cost <- as.numeric(gsub(",", "", data$expected_direct_cost))
data$observed_total_cost <- as.numeric(gsub(",", "", data$observed_total_cost))

# Pad MRN column to 9 digits with leading zeros
data$mrn <- str_pad(data$mrn, width = 9, side = "left", pad = "0")

# Flag cases with MRNs from our ccc MRNs list - pad these too
flag_cases <- c("1772396", "1591322", "880348", "795719", "1346547", "850935", 
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

# Pad flag_cases to 9 digits with leading zeros
flag_cases <- str_pad(flag_cases, width = 9, side = "left", pad = "0")

# Create the flag variable
data$ccc_flag <- ifelse(data$mrn %in% flag_cases, 1, 0)
table(data$ccc_flag)
length(unique(data$mrn[data$ccc_flag==1]))

# Split the data based on the flag
df_ccc <- data %>% filter(ccc_flag==1)
df_other <- data %>% filter(ccc_flag==0)

# Get list of MS-DRGs in the ccc dataset
ccc_drg_list <- unique(df_ccc$msdrg)
length(ccc_drg_list)  # Check total number of unique DRGs

# Make list of MS-DRGs into a dataframe and add label variable
# FIXED: Removed duplicate DRG codes (025, 270)
ccc_drg_lookup <- data.frame(
  msdrg = c("004", "007", "023", "025", "026", "035", "037", "040", "051", "065", "066", "069", 
            "094", "098", "099", "122", "123", "124", "125", "126", "127", "128", "130", "135", 
            "192", "198", "206", "207", "208", "209", "228", "229", "270", "273", "274", "276", 
            "277", "280", "570", "574", "617", "637", "640", "682", "853", "854", "855", "856", 
            "915", "919", "920", "957", "959", "976"),
  description = c(
    "Tracheostomy with MV >96 hours or PDX except face, mouth and neck without major O.R. procedure",
    "Other circulatory system diagnoses with MCC",
    "Craniotomy and endovascular intracranial procedures with MCC",
    "Craniotomy and endovascular intracranial procedures with CC",
    "Craniotomy and endovascular intracranial procedures without CC/MCC",
    "ECMO or tracheostomy with MV >96 hours or PDX except face, mouth and neck with major O.R. procedure",
    "Extracranial procedures with MCC",
    "Septicemia or severe sepsis with MV >96 hours",
    "Intracranial hemorrhage or cerebral infarction with MCC",
    "Intracranial vascular procedures with PDX hemorrhage with MCC",
    "Intracranial vascular procedures without PDX hemorrhage with MCC",
    "Transient ischemia with MCC",
    "Bacterial and tuberculous infections of nervous system with MCC",
    "Non-bacterial infection of nervous system except viral meningitis with MCC",
    "Major hematological and immunological diagnoses except sickle cell crisis and coagulation disorders with CC",
    "Poisoning and toxic effects of drugs with MCC",
    "Hip and femur procedures except major joint with CC",
    "Hip and femur procedures except major joint with MCC",
    "Osteomyelitis with MCC",
    "Osteomyelitis with CC",
    "Chemotherapy without acute leukemia as secondary diagnosis with CC",
    "Other bone disease and specific arthropathies with MCC",
    "Other bone disease and specific arthropathies with CC",
    "Skin ulcers with CC",
    "Other skin, subcutaneous tissue and breast procedures with CC",
    "Miscellaneous disorders of nutrition, metabolism, fluids and electrolytes with MCC",
    "Other skin, subcutaneous tissue and breast procedures with MCC",
    "Endocrine disorders with CC",
    "Endocrine disorders with MCC",
    "Miscellaneous disorders of nutrition, metabolism, fluids and electrolytes without MCC",
    "Non-extensive O.R. procedure unrelated to principal diagnosis with MCC",
    "Non-extensive O.R. procedure unrelated to principal diagnosis with CC",
    "Cardiac valve and other major cardiothoracic procedures with cardiac catheterization with MCC",
    "G.I. hemorrhage with MCC",
    "Septicemia or severe sepsis without MV >96 hours with MCC",
    "Septicemia or severe sepsis without MV >96 hours without MCC",
    "Signs and symptoms of musculoskeletal system and connective tissue with MCC",
    "Inborn and other disorders of metabolism",
    "Skin graft for skin ulcer or cellulitis with CC",
    "Other kidney and urinary tract diagnoses with MCC",
    "Chemotherapy without acute leukemia as secondary diagnosis with MCC",
    "Diabetes with MCC",
    "Kidney and urinary tract infections without MCC",
    "Acute leukemia without major O.R. procedure with MCC",
    "Infectious and parasitic diseases with O.R. procedure with MCC",
    "Infectious and parasitic diseases without O.R. procedure with MCC",
    "Infectious and parasitic diseases without O.R. procedure with CC",
    "Infectious and parasitic diseases without O.R. procedure without CC/MCC",
    "Other circulatory system diagnoses with CC",
    "Diabetes with CC",
    "Lymphoma and non-acute leukemia with MCC",
    "Tracheostomy with MV >96 hours or PDX except face, mouth and neck with major O.R. procedure",
    "Other respiratory system diagnosis with ventilator support >96 hours",
    "Wound debridement and skin graft except hand for musculoskeletal system and connective tissue disorders with CC"
  ),
  stringsAsFactors = FALSE
)

# Filter data to only include DRGs of interest
df_ccc_drg <- df_ccc %>% 
  filter(msdrg %in% ccc_drg_list)

df_other_drg <- df_other %>%
  filter(msdrg %in% ccc_drg_list)

# Add descriptions - use left_join to handle all DRGs
df_ccc_drg <- df_ccc_drg %>% left_join(ccc_drg_lookup, by="msdrg")
df_other_drg <- df_other_drg %>% left_join(ccc_drg_lookup, by="msdrg")

# Create combined variable
df_ccc_drg <- df_ccc_drg %>% mutate(msdrg_des = paste(msdrg, description, sep=": "))
df_other_drg <- df_other_drg %>% mutate(msdrg_des = paste(msdrg, description, sep=": "))

# Check for missing descriptions
print("CCC records with missing descriptions:")
print(sum(is.na(df_ccc_drg$description)))

print("Other records with missing descriptions:")
print(sum(is.na(df_other_drg$description)))

# How many patients in each dataset?
print(paste("Unique CCC patients:", length(unique(df_ccc_drg$mrn))))
print(paste("Unique non-CCC patients:", length(unique(df_other_drg$mrn))))

# Get median summary stats for ccc cases for each msdrg
ccc_medians <- df_ccc_drg %>% 
  filter(!is.na(description)) %>%  # Filter out records with missing descriptions
  group_by(msdrg_des) %>%
  summarize(sample_size=n(),
            med_LOS = median(los, na.rm=TRUE),
            med_GMLOS = median(expected_vizient_los, na.rm=TRUE),
            med_ObsCost = median(observed_direct_cost, na.rm=TRUE),
            med_ExpectedCost = median(expected_direct_cost, na.rm=TRUE),
            .groups = 'drop')

# Get summary stats for non-ccc cases for each msdrg
other_medians <- df_other_drg %>% 
  filter(!is.na(description)) %>%  # Filter out records with missing descriptions
  group_by(msdrg_des) %>%
  summarize(med_LOS = median(los, na.rm=TRUE),
            med_GMLOS = median(expected_vizient_los, na.rm=TRUE),
            med_ObsCost = median(observed_direct_cost, na.rm=TRUE),
            med_ExpectedCost = median(expected_direct_cost, na.rm=TRUE),
            .groups = 'drop')

# Get top 15 MS-DRGs by CCC observed median LOS
top15_msdrgs <- ccc_medians %>%
  arrange(desc(med_LOS)) %>%
  slice_head(n = 15) %>%
  pull(msdrg_des)

# Filter both datasets to only include those top 15 MS-DRGs
ccc_medians_filtered <- ccc_medians %>%
  filter(msdrg_des %in% top15_msdrgs)

other_medians_filtered <- other_medians %>%
  filter(msdrg_des %in% top15_msdrgs)

# Pivot for plotting - LOS
ccc_medians_long <- ccc_medians_filtered %>%
  select(msdrg_des, med_LOS, med_GMLOS) %>%
  pivot_longer(
    cols = c(med_LOS, med_GMLOS),
    names_to = "Type",
    values_to = "MedianValue"
  )

other_medians_long <- other_medians_filtered %>%
  select(msdrg_des, med_LOS, med_GMLOS) %>%
  pivot_longer(
    cols = c(med_LOS, med_GMLOS),
    names_to = "Type",
    values_to = "MedianValue"
  )

# Set factor levels for consistent x-axis order
ccc_medians_long$msdrg_des <- factor(ccc_medians_long$msdrg_des, levels = rev(top15_msdrgs))
other_medians_long$msdrg_des <- factor(other_medians_long$msdrg_des, levels = rev(top15_msdrgs))

# Theme with light grey background
light_bg_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey95", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.subtitle = element_text(size=7)
  )

# CCC Plot
cccmedianlos <- ggplot(ccc_medians_long, aes(x = msdrg_des, y = MedianValue, color = Type)) +
  geom_point(size = 2) +
  geom_line(aes(group = msdrg_des), color = "lightblue3", linetype = "dotted", linewidth = 1) +
  labs(title = "CCC Population", x = NULL, y = "Median LOS (Days)",
       subtitle="Note: Sample sizes per MS-DRG are small for CCC population; in some cases, sample size =1") +
  scale_color_discrete(labels = c("Expected", "Observed")) +
  light_bg_theme +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x= element_text(hjust=1.2),
    legend.position = "top",
    legend.justification = "left"
  ) +
  coord_flip()

# Non-CCC Plot
othermedianlos <- ggplot(other_medians_long, aes(x = msdrg_des, y = MedianValue, color = Type)) +
  geom_point(size = 2) +
  geom_line(aes(group = msdrg_des), color = "lightblue3", linetype = "dotted", linewidth = 1) +
  labs(title = "Non-CCC Population", x = NULL, y = NULL, 
       subtitle="NOTE: axis change") +
  scale_color_discrete(labels = c("Expected", "Observed")) +
  light_bg_theme +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip()+
  scale_y_continuous(
    breaks = function(x) unique(c(0, pretty(x))),
    limits = function(x) c(0, max(x)))

# Combine plots
final_plot <- cccmedianlos + othermedianlos +
  plot_layout(ncol = 2, widths = c(1.1, 1)) &
  plot_annotation(
    title = "Observed vs Expected Median Length of Stay by MS-DRG",
    subtitle = "Top 15 MS-DRGs (based on CCC population)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) 
  )

print(final_plot)

##For Observed Direct Costs
#Get top 15 MS-DRGs by CCC observed median direct costs
top15_msdrgs2 <- ccc_medians %>%
  arrange(desc(med_ObsCost)) %>%
  slice_head(n = 15) %>%
  pull(msdrg_des)

#Filter both datasets to only include those top 15 MS-DRGs
ccc_medians_filtered2 <- ccc_medians %>%
  filter(msdrg_des %in% top15_msdrgs2)

other_medians_filtered2 <- other_medians %>%
  filter(msdrg_des %in% top15_msdrgs2)

#Pivot for plotting
ccc_medians_long2 <- ccc_medians_filtered2 %>%
  select(msdrg_des, med_ObsCost, med_ExpectedCost) %>%
  pivot_longer(
    cols = c(med_ObsCost, med_ExpectedCost),
    names_to = "Type",
    values_to = "MedianValue"
  )

other_medians_long2 <- other_medians_filtered2 %>%
  select(msdrg_des, med_ObsCost, med_ExpectedCost) %>%
  pivot_longer(
    cols = c(med_ObsCost, med_ExpectedCost),
    names_to = "Type",
    values_to = "MedianValue"
  )

#Set factor levels for consistent x-axis order
ccc_medians_long2$msdrg_des <- factor(ccc_medians_long2$msdrg_des, levels = rev(top15_msdrgs2))
other_medians_long2$msdrg_des <- factor(other_medians_long2$msdrg_des, levels = rev(top15_msdrgs2))

# CCC Plot
cccmedianDC <- ggplot(ccc_medians_long2, aes(x = msdrg_des, y = MedianValue, color = Type)) +
  geom_point(size = 2) +
  geom_line(aes(group = msdrg_des), color = "lightblue3", linetype = "dotted", linewidth = 1) +
  labs(title = "CCC Population", x = NULL, y = "Median Direct Cost",
       subtitle="Note: Sample sizes per MS-DRG are small for CCC population; in some cases, sample size =1") +
  scale_color_discrete(labels = c("Expected", "Observed")) +
  light_bg_theme +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x=element_text(hjust=1.2),
    legend.position = "top",
    legend.justification = "left"
  ) +
  coord_flip() +
  scale_y_continuous(labels=label_number(scale=1/1000, suffix=" K"))

# Non-CCC Plot
othermedianDC <- ggplot(other_medians_long2, aes(x = msdrg_des, y = MedianValue, color = Type)) +
  geom_point(size = 2) +
  geom_line(aes(group = msdrg_des), color = "lightblue3", linetype = "dotted", linewidth = 1) +
  labs(title = "Non-CCC Population", x = NULL, y = NULL, 
       subtitle="NOTE: axis change") +
  scale_color_discrete(labels = c("Expected", "Observed")) +
  light_bg_theme +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip()+
  scale_y_continuous(labels=label_number(scale=1/1000, suffix=" K"),
                     breaks = function(x) unique(c(0, pretty(x))),
                     limits = function(x) c(0, max(x)))

# Combine plots
final_plot2 <- cccmedianDC + othermedianDC +
  plot_layout(ncol = 2, widths = c(1.1, 1)) &
  plot_annotation(
    title = "Observed vs Expected Median Direct Cost by MS-DRG",
    subtitle = "Top 15 MS-DRGs (based on CCC population)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

print(final_plot2)

#################
# Summary statistics for CCC cohort

# Sum Observed Total Cost for CCC patients
sum_totalcost <- sum(df_ccc$observed_total_cost, na.rm=TRUE)

# Count admission cases (readmissions have NA cost)
adm_cases <- nrow(df_ccc[which(!is.na(df_ccc$observed_total_cost)), ])

# Avg per admission case
avg_totalcost_per_case <- sum_totalcost/adm_cases

# Sum Observed Direct Cost for CCC patients
sum_directcost <- sum(df_ccc$observed_direct_cost, na.rm=TRUE)
avg_directcost_per_case <- sum_directcost/adm_cases

# Total LOS
sum_los <- sum(df_ccc$los, na.rm=TRUE)
avg_los_per_case <- sum_los/nrow(df_ccc)

# Arithmetic Means of cost by CCC patient
df_avg_tc <- df_ccc %>%
  group_by(mrn) %>%
  summarize(avg_totalcost = mean(observed_total_cost, na.rm=TRUE),
            .groups = 'drop')

df_avg_dc <- df_ccc %>%
  group_by(mrn) %>%
  summarize(avg_directcost = mean(observed_direct_cost, na.rm=TRUE),
            .groups = 'drop')

df_avg_los <- df_ccc %>%
  group_by(mrn) %>%
  summarize(avg_los = mean(los, na.rm=TRUE),
            .groups = 'drop')

# Print summary statistics
print("===== CCC COHORT SUMMARY STATISTICS =====")
print(paste("Total unique CCC patients:", length(unique(df_ccc$mrn))))
print(paste("Total CCC encounters:", nrow(df_ccc)))
print(paste("Total LOS:", sum_los))
print(paste("Average LOS per encounter:", avg_los_per_case))
print(paste("Total Observed Direct Cost:", dollar(sum_directcost)))
print(paste("Average Direct Cost per encounter:", dollar(avg_directcost_per_case)))
print(paste("Total Observed Total Cost:", dollar(sum_totalcost)))
print(paste("Average Total Cost per encounter:", dollar(avg_totalcost_per_case)))
print(paste("Mean Direct Cost per patient:", dollar(mean(df_avg_dc$avg_directcost, na.rm=TRUE))))
print(paste("Mean Total Cost per patient:", dollar(mean(df_avg_tc$avg_totalcost, na.rm=TRUE))))
print(paste("Mean LOS per patient:", mean(df_avg_los$avg_los, na.rm=TRUE)))

library(gtsummary)
library(dplyr)

# ============================================================================
# TABLE 1: Patient-Level Characteristics by Primary Payer Type
# ============================================================================

# Calculate patient-level summaries
patient_summary <- df_ccc %>%
  group_by(mrn) %>%
  summarise(
    # Demographics (take first value per patient)
    Age = first(age),
    Sex = first(sex),
    Race = first(race),
    Ethnicity = first(ethnicity),
    Primary_Payor = first(vizient_primary_payor_type),
    
    # Utilization totals
    N_Encounters = n(),
    Total_LOS = sum(los, na.rm = TRUE),
    Total_Cost = sum(observed_total_cost, na.rm = TRUE),
    Total_Direct_Cost = sum(observed_direct_cost, na.rm = TRUE),
    
    .groups = 'drop'
  )

# Create Table 1 using gtsummary
table1 <- patient_summary %>%
  select(Primary_Payor, Age, Sex, Race, Ethnicity, 
         N_Encounters, Total_LOS, Total_Cost, Total_Direct_Cost) %>%
  tbl_summary(
    by = Primary_Payor,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      Age ~ "Age (years)",
      Sex ~ "Sex",
      Race ~ "Race",
      Ethnicity ~ "Ethnicity",
      N_Encounters ~ "Number of Encounters per Patient",
      Total_LOS ~ "Total Length of Stay per Patient (days)",
      Total_Cost ~ "Total Cost Observed per Patient ($)",
      Total_Direct_Cost ~ "Total Direct Cost Observed per Patient ($)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "fisher.test"
    )
  ) %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Primary Payer Type**") %>%
  modify_footnote(
    all_stat_cols() ~ "Mean ± SD; n (%)"
  ) %>%
  bold_labels()

# Print the table
print(table1)

# Optional: Save as Word document
table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "Table1_CCC_by_Payer.docx")

# Optional: Save as HTML
table1 %>%
  as_gt() %>%
  gt::gtsave(filename = "Table1_CCC_by_Payer.html")

# ============================================================================
# Alternative: More detailed version with custom formatting
# ============================================================================

table1_detailed <- patient_summary %>%
  select(Primary_Payor, Age, Sex, Race, Ethnicity, 
         N_Encounters, Total_LOS, Total_Cost, Total_Direct_Cost) %>%
  tbl_summary(
    by = Primary_Payor,
    statistic = list(
      Age ~ "{mean} ± {sd}",
      N_Encounters ~ "{mean} ± {sd}",
      Total_LOS ~ "{mean} ± {sd}",
      Total_Cost ~ "{mean} ± {sd}",
      Total_Direct_Cost ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      Age ~ c(1, 1),
      N_Encounters ~ c(1, 1),
      Total_LOS ~ c(1, 1),
      Total_Cost ~ c(0, 0),
      Total_Direct_Cost ~ c(0, 0),
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      Age ~ "Age (years)",
      Sex ~ "Sex",
      Race ~ "Race",
      Ethnicity ~ "Ethnicity",
      N_Encounters ~ "Number of Encounters per Patient",
      Total_LOS ~ "Total Length of Stay per Patient (days)",
      Total_Cost ~ "Total Cost Observed per Patient ($)",
      Total_Direct_Cost ~ "Total Direct Cost Observed per Patient ($)"
    ),
    missing = "no"
  ) %>%
  add_overall(last = FALSE) %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "fisher.test"
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  modify_header(
    label ~ "**Characteristic**",
    stat_0 ~ "**Overall**, N = {N}"
  ) %>%
  modify_spanning_header(all_stat_cols() ~ "**Primary Payer Type**") %>%
  modify_footnote(
    update = all_stat_cols() ~ "Mean ± SD; n (%)",
    abbreviation = TRUE
  ) %>%
  modify_footnote(
    p.value ~ "Kruskal-Wallis rank sum test; Fisher's exact test"
  ) %>%
  bold_labels() %>%
  italicize_levels()

print(table1_detailed)

# ============================================================================
# Summary statistics to check
# ============================================================================

cat("\n=== PATIENT-LEVEL SUMMARY ===\n")
cat("Total unique patients:", nrow(patient_summary), "\n")
cat("\nPayer type distribution:\n")
print(table(patient_summary$Primary_Payor))

cat("\n\nSummary by payer:\n")
patient_summary %>%
  group_by(Primary_Payor) %>%
  summarise(
    N_Patients = n(),
    Mean_Age = mean(Age, na.rm = TRUE),
    Mean_Encounters = mean(N_Encounters, na.rm = TRUE),
    Mean_LOS = mean(Total_LOS, na.rm = TRUE),
    Mean_Total_Cost = mean(Total_Cost, na.rm = TRUE),
    Mean_Direct_Cost = mean(Total_Direct_Cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  print()