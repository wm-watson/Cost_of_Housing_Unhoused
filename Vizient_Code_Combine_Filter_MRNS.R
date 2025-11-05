## This file analyzes data from the UAMS Vizient Clinical Database PowerBI Report.
## Data is from November 28, 2022 to February 28, 2025. 
## Readmission cases are classified as less than 30 days between previous admission to current.
## Goal: Determine if individuals in the ccc MRNs list are a costly population to UAMS.

# Import libraries
library(ggplot2)
library(readxl) # to read in excel data
library(dplyr) # for clean syntax
library(janitor)
library(tidyr)
library(scales)
library(patchwork)
library(gt) #for table printing
# Import data

readmissions <- read_excel("C:/Users/watso/Box/Cost of Housing Unhoused in Hospital/Will_Melissa/Vizient_Data/Selected Readmissions 07.04.23 - 10.14.25.xlsx", col_names = TRUE)
admissions <- read_excel("C:/Users/watso/Box/Cost of Housing Unhoused in Hospital/Will_Melissa/Vizient_Data/Selected Admissions - 11.28.22 - 10.14.25.xlsx", col_names = TRUE)

# # Remove extra rows
# readmissions <- readmissions[1:3006,]
# admissions <- admissions[1:42978,]
# Full join the two data tables
data <- full_join(admissions, readmissions, by=NULL)
tail(data)

# Clean names
data <- clean_names(data)

# ensure correct data types for variables of interest
data$los <- as.numeric(data$los)
data$expected_vizient_los <- as.numeric(data$expected_vizient_los)
data$observed_direct_cost <- as.numeric(gsub(",", "", data$observed_direct_cost))
data$expected_direct_cost <- as.numeric(gsub(",", "", data$expected_direct_cost))
data$observed_total_cost <- as.numeric(gsub(",", "", data$observed_total_cost))

# Flag cases with MRNs from our ccc MRNs list
# Define ccc MRNs list with leading zeros
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
                    "4045947", "3122411", "888523", "1267251", "004296383", "00282489",
                    "001014542", "003905334","003831307", "001529462", "003989308", 
                    "003094083", "001542364", 
                    "001723512", "003933450", "004336254")

# Remove leading zeros by converting to numeric then back to character
flag_cases <- as.character(as.numeric(flag_cases_raw))

cat("Total MRNs in list:", length(flag_cases), "\n")

# Create the flag variable
data$ccc_flag <- ifelse(data$mrn %in% flag_cases, 1, 0)
table(data$ccc_flag)
length(unique(data$mrn[data$ccc_flag==1]))
# Split the data based on the flag, so ccc data is separate from other cases
# Create dataset for ccc cases
df_ccc <- data %>% filter(ccc_flag==1)
# Create dataset for non-ccc cases      
df_other <- data %>% filter(ccc_flag==0)

# For each of the two datasets (ccc vs non-ccc), get summary statistics for each MS-DRG in the ccc dataset 
# Get list of MS-DRGs in the ccc dataset
ccc_drg_list <- unique(df_ccc$msdrg)
# Make list of MS-DRGs into a dataframe and add label variable
# source: https://www.cms.gov/icd10m/version37-fullcode-cms/fullcode_cms/P0002.html
ccc_drg_lookup <- data.frame(msdrg=ccc_drg_list,
                             description= c(
                               "Diabetes with CC",                                    # 919
                               "Other circulatory system diagnoses with MCC",          # 955
                               "Degenerative nervous system disorders with MCC",       # 3
                               "Allogeneic bone marrow transplant",                    # 14 - NEW
                               "Miscellaneous disorders of nutrition, metabolism, fluids and electrolytes with MCC", # 11
                               "Heart failure and shock with MCC",                     # 637
                               "Amputation of lower limb for endocrine, nutritional and metabolic disorders with CC", # 840
                               "Craniotomy and endovascular intracranial procedures with MCC", # 20
                               "Tracheostomy with MV >96 hours or PDX except face, mouth and neck without major O.R. procedure", # 957
                               "Non-extensive O.R. procedure unrelated to principal diagnosis with MCC", # 25
                               "Septicemia or severe sepsis without MV >96 hours with MCC", # 40
                               "Septicemia or severe sepsis without MV >96 hours without MCC", # 872
                               "Other circulatory system diagnoses with CC",           # 574
                               "Diabetes with MCC",                                    # 4
                               "Infectious and parasitic diseases with O.R. procedure with MCC", # 853 - NEW
                               "Cardiac valve and other major cardiothoracic procedures with cardiac catheterization with MCC", # 270 - NEW
                               "Cardiac valve and other major cardiothoracic procedures without cardiac catheterization with CC", # 216 - NEW
                               "Signs and symptoms of musculoskeletal system and connective tissue with MCC", # 690
                               "Poisoning and toxic effects of drugs with MCC",        # 253
                               "Chemotherapy without acute leukemia as secondary diagnosis with CC", # 617
                               "Miscellaneous disorders of nutrition, metabolism, fluids and electrolytes without MCC", # 886
                               "Kidney and urinary tract infections without MCC",      # 642
                               "Respiratory system diagnosis with ventilator support >96 hours", # 871
                               "Skin graft for skin ulcer or cellulitis with CC",     # 481
                               "Inborn and other disorders of metabolism",             # 94
                               "Lymphoma and non-acute leukemia with MCC",             # 870
                               "Diabetes with CC or without CC/MCC",                   # 920 - NEW
                               "Other skin, subcutaneous tissue and breast procedures with CC", # 314
                               "Other kidney and urinary tract diagnoses with MCC",    # 644
                               "Septicemia or severe sepsis with MV >96 hours",        # 97
                               "G.I. hemorrhage with MCC",                             # 64
                               "Chemotherapy without acute leukemia as secondary diagnosis with MCC", # 580
                               "Major hematological and immunological diagnoses except sickle cell crisis and coagulation disorders with CC", # 987
                               "Kidney and ureter procedures for non-neoplasm with MCC", # 654 - NEW
                               "Other O.R. procedures for multiple significant trauma with MCC", # 555
                               "Kidney and urinary tract infections with MCC",         # 464
                               "Craniotomy with major device implant or acute complex CNS PDX without MCC", # 876
                               "ECMO or tracheostomy with MV >96 hours or PDX except face, mouth and neck with major O.R. procedure", # 207
                               "Other vascular procedures with CC",                    # 291
                               "Intracranial hemorrhage or cerebral infarction with MCC", # 847
                               "Skin ulcers with CC",                                  # 24
                               "Craniotomy with major device implant or acute complex CNS PDX with MCC or chemotherapy implant or epilepsy with neurostimulator", # 809
                               "Wound debridement and skin graft except hand for musculoskeletal system and connective tissue disorders with CC", # 641
                               "Tracheostomy for face, mouth & neck diagnoses or laryngectomy with MCC", # 846
                               "Hip and femur procedures except major joint with CC",  # 638
                               "Craniotomy for multiple significant trauma",           # 115
                               "Behavioral and developmental disorders",               # 593
                               "O.R. procedure with principal diagnoses of mental illness", # 917
                               "Osteomyelitis with MCC",                               # 689
                               "Renal failure with CC",                                # 683 - NEW
                               "Endocrine disorders with CC",                          # 640
                               "Bacterial and tuberculous infections of nervous system with MCC", # 698
                               "Acute leukemia without major O.R. procedure with MCC", # 441
                               "Non-bacterial infection of nervous system except viral meningitis with MCC", # 205
                               "Disorders of liver except malignancy, cirrhosis or alcoholic hepatitis with MCC", # 304
                               "Peripheral, cranial nerve and other nervous system procedures with MCC", # 56
                               "Intracranial vascular procedures with PDX hemorrhage with MCC", # 776
                               "Complications of treatment with MCC",                  # 539
                               "Postpartum and post abortion diagnoses without O.R. procedure", # 315
                               "Aftercare with CC/MCC",                                # 377
                               "Acute adjustment reaction and psychosocial dysfunction", # 280 - NEW
                               "Acute leukemia without major O.R. procedure with CC",  # 834
                               "Transient ischemia",                                   # 669 - NEW
                               "Spinal procedures with MCC",                           # 393 - NEW
                               "Disorders of pancreas except malignancy with CC",      # 442 - NEW
                               "Non-extensive burns",                                  # 70
                               "Major esophageal disorders with MCC",                  # 368
                               "Major small and large bowel procedures with MCC",      # 330 - NEW
                               "Extensive O.R. procedure unrelated to principal diagnosis with CC", # 981
                               "Craniotomy and endovascular intracranial procedures with CC" # 23
                             ),
                             stringsAsFactors=FALSE
)



# Filter data to only include DRGs of interest
df_ccc_drg <- df_ccc %>% 
  filter(msdrg %in% ccc_drg_list) #NOTE: should be no change
df_other_drg <- df_other %>%
  filter(msdrg %in% ccc_drg_list)
# Add descriptions
df_ccc_drg <- df_ccc_drg %>% left_join(ccc_drg_lookup, by="msdrg")
df_other_drg <- df_other_drg %>% left_join(ccc_drg_lookup, by="msdrg")
#Create combined variable
df_ccc_drg <- df_ccc_drg %>% mutate(msdrg_des = paste(msdrg, description, sep=": "))
df_other_drg <- df_other_drg %>% mutate(msdrg_des = paste(msdrg, description, sep=": "))

#How many patients in each dataset?
length(unique(df_ccc_drg$mrn)) #40
length(unique(df_other_drg$mrn)) #6643
# Get median summary stats for ccc cases for each msdrg
ccc_medians <- df_ccc_drg %>% group_by(msdrg_des) %>%
  summarize(sample_size=n(),
            med_LOS = median(los, na.rm=TRUE),
            med_GMLOS =median(expected_vizient_los, na.rm=TRUE),
            med_ObsCost = median(observed_direct_cost, na.rm=TRUE),
            med_ExpectedCost = median(expected_direct_cost, na.rm=TRUE)
  )
# Get summary stats for non-ccc cases for each msdrg
other_medians <- df_other_drg %>% group_by(msdrg_des) %>%
  summarize(
    med_LOS = median(los, na.rm=TRUE),
    med_GMLOS = median(expected_vizient_los, na.rm=TRUE),
    med_ObsCost = median(observed_direct_cost, na.rm=TRUE),
    med_ExpectedCost = median(expected_direct_cost, na.rm=TRUE)
  )

#######################
# Plot for median los for ccc and non-ccc
#Reshape data
ccc_medians_long <- ccc_medians %>%
  select(msdrg_des, med_LOS, med_GMLOS) %>%
  pivot_longer(
    cols = c(med_LOS, med_GMLOS),
    names_to = "Type",
    values_to = "MedianValue"
  )
other_medians_long <- other_medians %>%
  select(msdrg_des, med_LOS, med_GMLOS) %>%
  pivot_longer(
    cols = c(med_LOS, med_GMLOS),
    names_to = "Type",
    values_to = "MedianValue"
  )
# Get common x-axis range from both datasets
# shared_x_limits <- range(c(ccc_medians$med_LOS, other_medians$med_LOS), na.rm=TRUE)

#Plot ccc los
cccmedianlos <- ggplot(ccc_medians_long, aes(x=reorder(msdrg_des, -MedianValue), y=MedianValue, color=Type, group=msdrg_des))+
  geom_point(size=2)+
  geom_line(aes(group=msdrg_des), color="lightblue3", linetype="dotted", linewidth=1)+
  labs(title="Observed vs Expected Length of Stay by MS-DRG (CCC population)",
       subtitle="Note: Sample sizes per MS-DRG are small for CCC population; in some cases, sample size =1",
       x="MS-DRG", y="Median LOS (Days)")+
  scale_color_discrete(labels=c("Expected", "Observed"))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.text.y=element_text(size=7),
        legend.position="top",
        legend.justification="right")+
  coord_flip() +
  scale_x_discrete(limits=rev, expand=expansion(mult=c(0.01, 0.01)))

#Plot non-ccc los
othermedianlos <- ggplot(other_medians_long, aes(x=reorder(msdrg_des, -MedianValue), y=MedianValue, color=Type, group=msdrg_des))+
  geom_point(size=2)+
  geom_line(aes(group=msdrg_des), color="lightblue3", linetype="dotted", linewidth=1)+
  labs(title="Observed vs Expected Length of Stay by MS-DRG (Non-CCC Population)",
       subtitle="Note: Change in x-axis scale",
       x="MS-DRG", y="Median LOS (Days)")+
  scale_color_discrete(labels=c("Expected", "Observed"))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.text.y=element_text(size=7),
        legend.position="none")+
  coord_flip()+
  scale_x_discrete(limits=rev, expand=expansion(mult=c(0.01, 0.01)))

# Compare
(cccmedianlos/othermedianlos ) + plot_layout(heights=c(1,1))

######################
# Plot for median direct costs for ccc and non-ccc
#Reshape data
ccc_medians_long2 <- ccc_medians %>%
  select(msdrg_des, med_ObsCost, med_ExpectedCost) %>%
  pivot_longer(
    cols = c(med_ObsCost, med_ExpectedCost),
    names_to = "Type",
    values_to = "MedianValue"
  )
other_medians_long2 <- other_medians %>%
  select(msdrg_des, med_ObsCost, med_ExpectedCost) %>%
  pivot_longer(
    cols = c(med_ObsCost, med_ExpectedCost),
    names_to = "Type",
    values_to = "MedianValue"
  )
# Get common x-axis range from both datasets
# shared_x_limits <- range(c(ccc_medians$med_LOS, other_medians$med_LOS), na.rm=TRUE)

#Plot ccc los
cccmedian_dcost <- ggplot(ccc_medians_long2, aes(x=reorder(msdrg_des, -MedianValue), y=MedianValue, color=Type, group=msdrg_des))+
  geom_point(size=2)+
  geom_line(aes(group=msdrg_des), color="lightblue3", linetype="dotted", linewidth=1)+
  labs(title="Observed vs Expected Direct Cost by MS-DRG (CCC population)",
       subtitle="Note: Sample sizes per MS-DRG are small for CCC population; in some cases, sample size =1",
       x="MS-DRG", y=" Median Direct Cost (USD)")+
  scale_color_discrete(labels=c("Expected", "Observed"))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.text.y=element_text(size=7),
        legend.position="top",
        legend.justification="right")+
  coord_flip() +
  scale_x_discrete(limits=rev, expand=expansion(mult=c(0.01, 0.01)))+
  scale_y_continuous(labels=label_number(scale=1/1000, suffix=" K"))

#Plot non-ccc los
othermedian_dcost <- ggplot(other_medians_long2, aes(x=reorder(msdrg_des, -MedianValue), y=MedianValue, color=Type, group=msdrg_des))+
  geom_point(size=2)+
  geom_line(aes(group=msdrg_des), color="lightblue3", linetype="dotted", size=1)+
  labs(title="Observed vs Expected Direct Cost by MS-DRG (Non-CCC Population)",
       subtitle="Note: Change in x-axis scale",
       x="MS-DRG", y=" Median Direct Cost (USD)")+
  scale_color_discrete(labels=c("Expected", "Observed"))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.text.y=element_text(size=7),
        legend.position="none")+
  coord_flip()+
  scale_x_discrete(limits=rev, expand=expansion(mult=c(0.01, 0.01)))+
  scale_y_continuous(labels=label_number(scale=1/1000, suffix=" K"))

# Compare
(cccmedian_dcost/othermedian_dcost ) + plot_layout(heights=c(1,1))

###########################################################
#Plots are dense, let's go back and select only 15 MSDRGs
#Get top 15 MS-DRGs by CCC observed median LOS
top15_msdrgs <- ccc_medians %>%
  arrange(desc(med_LOS)) %>%
  slice_head(n = 15) %>%
  pull(msdrg_des)
#Filter both datasets to only include those top 15 MS-DRGs
ccc_medians_filtered <- ccc_medians %>%
  filter(msdrg_des %in% top15_msdrgs)

other_medians_filtered <- other_medians %>%
  filter(msdrg_des %in% top15_msdrgs)
#Pivot for plotting
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
#Set factor levels for consistent x-axis order
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

final_plot


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

final_plot2
#################
#Sum Observed Total Cost for CCC patients (admissions)
sum_totalcost <- sum(df_ccc$observed_total_cost, na.rm=TRUE)
#count admission cases (readmissions have NA cost)
adm_mrns<-nrow(df_ccc[which(!is.na(df_ccc$observed_total_cost)), ])
#avg per admission case
sum_totalcost/adm_mrns
#Sum Observed Direct Cost for CCC patients (admissions)
sum_directcost <- sum(df_ccc$observed_direct_cost, na.rm=TRUE)
sum_directcost/adm_mrns #

#Arithmetic Means of cost by CCC patient
#Total Cost
df_avg_tc <- df_ccc %>%
  group_by(mrn) %>%
  summarize(avg_totalcost = mean(observed_total_cost, na.rm=TRUE))
#Print table
df_avg_tc <- df_avg_tc %>%
  mutate(Patient = paste("Patient", row_number()))

# Create the table
df_avg_tc %>%
  gt() %>%
  cols_label(
    Patient = "Patient",
    avg_totalcost = "Avg Total Cost"
  ) %>%
  tab_header(title = "Average Total Cost Per Patient") %>%
  fmt_number(
    columns = c(avg_totalcost),
    decimals = 2
  ) %>%
  cols_hide(columns = c(mrn)) 

#Direct Cost
df_avg_dc <- df_ccc %>%
  group_by(mrn) %>%
  summarize(avg_directcost = mean(observed_direct_cost, na.rm=TRUE))
df_avg_dc
#Print table
df_avg_dc <- df_avg_dc %>%
  mutate(Patient = paste("Patient", row_number()))

# Create the table
df_avg_dc %>%
  gt() %>%
  cols_label(
    Patient = "Patient",
    avg_directcost = "Avg Direct Cost"
  ) %>%
  tab_header(title = "Average Direct Cost Per Patient") %>%
  fmt_number(
    columns = c(avg_directcost),
    decimals = 2
  ) %>%
  cols_hide(columns = c(mrn)) 

#Sum LOS for CCC patients
sum_los <- sum(df_ccc$los, na.rm=TRUE)
#Avg per case
sum_los/nrow(df_ccc)
#Arithmetic Means of LOS by CCC patient
df_avg_los <- df_ccc %>%
  group_by(mrn) %>%
  summarize(avg_los = mean(los, na.rm=TRUE))
df_avg_los  #Print table

# Add a patient label column
df_avg_los <- df_avg_los %>%
  mutate(Patient = paste("Patient", row_number()))

# Create the table
df_avg_los %>%
  gt() %>%
  cols_label(
    Patient = "Patient",
    avg_los = "Avg LOS"
  ) %>%
  tab_header(title = "Average LOS Per Patient") %>%
  fmt_number(
    columns = c(avg_los),
    decimals = 2
  ) %>%
  cols_hide(columns = c(mrn)) 

