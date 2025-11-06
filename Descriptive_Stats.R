# ------------------------------------------------------------------------------
# TABLE 1: STRATIFIED BY PAYER TYPE
# ------------------------------------------------------------------------------

# First, let's create a categorical LOS variable to better represent the data
ccc_data <- ccc_data %>%
  mutate(LOS_Category = case_when(
    LOS.Observed == 1 ~ "1 day",
    LOS.Observed == 2 ~ "2 days",
    LOS.Observed >= 3 & LOS.Observed <= 5 ~ "3-5 days",
    LOS.Observed > 5 ~ ">5 days",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("1 day", "2 days", "3-5 days", ">5 days")))

# Create the stratified table
table1 <- ccc_data %>%
  select(
    Vizient.Primary.Payer,
    Age, Sex, Race, Ethnicity,
    Encounter.Type, 
    Total_Cost_Numeric, 
    LOS.Observed,
    LOS_Category
  ) %>%
  tbl_summary(
    by = Vizient.Primary.Payer,
    type = list(
      Total_Cost_Numeric ~ "continuous",
      Age ~ "continuous"
      # Let LOS.Observed be removed since we'll use LOS_Category
    ),
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)",
      Total_Cost_Numeric ~ "${median} [{p25}, {p75}]"
    ),
    digits = list(
      Age ~ 1,
      Total_Cost_Numeric ~ 0
    ),
    label = list(
      Age ~ "Age (years)",
      Sex ~ "Sex",
      Race ~ "Race",
      Ethnicity ~ "Ethnicity",
      Encounter.Type ~ "Encounter Type",
      Total_Cost_Numeric ~ "Total Cost",
      LOS_Category ~ "Length of Stay"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "chisq.test" # Use chi-square for categorical variables
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_overall() %>%
  modify_header(
    update = list(
      label ~ "**Characteristic**",
      stat_0 ~ "**Overall**\n(N = {N})",
      all_stat_cols() ~ "**{level}**\n(N = {n})"
    )
  ) %>%
  modify_caption("**Table 1. Patient Characteristics Stratified by Primary Payer Type**") %>%
  bold_labels() %>%
  # Add a footnote about the statistical tests
  add_stat_label(
    label = list(
      all_categorical() ~ "Chi-square test",
      all_continuous() ~ "Kruskal-Wallis rank sum test"
    )
  )

# Save to Word
table1 %>% 
  as_gt() %>% 
  gt::gtsave("Table1_ByPayer.docx")