Statistical Analysis Worksheet: Diabetes Prevalence by Race and Income Group (Revised)
This worksheet focuses on the R code for analyzing diabetes prevalence by race and income group, assuming your data (df) is already loaded in your R environment.

Data Preparation: Create Diabetes_Status_Group Variable
This step categorizes the DIABETE4 variable into meaningful groups for analysis.

# --- Create Diabetes_Status_Group Variable ---
# This step is crucial to categorize DIABETE4 into meaningful groups for analysis.
# We will categorize '1' and '2' as 'Yes (Diagnosed)' for simplicity in this analysis.
# Adjust categories as needed for your specific research question.
df <- df %>%
  mutate(
    Diabetes_Status_Group = case_when(
      DIABETE4 == 1 ~ "Yes (Diagnosed)",
      DIABETE4 == 2 ~ "Yes (Pregnancy-related)", # You might combine this with 'Yes (Diagnosed)' for simplicity or keep separate
      DIABETE4 == 3 ~ "No Diabetes",
      DIABETE4 == 4 ~ "Pre-diabetes/Borderline",
      DIABETE4 %in% c(7, 9) ~ NA_character_, # Treat 'Don't know/Not sure' and 'Refused' as missing
      TRUE ~ NA_character_ # Catch any other unexpected values as missing
    )
  )

Analysis: Proportions of Diabetes Status by Race and Income
This code calculates the percentage of "Yes (Diagnosed)" diabetes statuses within each racial group, separately for each income group, filtering out missing values.

# Calculate proportions (percentages) of Diabetes Status within each Race Group and Income Group
diabetes_by_race_income_proportions <- df %>%
  filter(!is.na(Race_Group) & !is.na(Diabetes_Status_Group) & !is.na(Income_Group)) %>% # Filter missing values
  group_by(Income_Group, Race_Group) %>%
  count(Diabetes_Status_Group) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(Diabetes_Status_Group == "Yes (Diagnosed)") %>% # Focusing on "Yes (Diagnosed)" for plotting
  ungroup()

# Display the results
print(diabetes_by_race_income_proportions)

Creating Bar Charts with ggplot2
This section provides the R code for generating the bar charts, including the faceted chart with a fixed y-axis for better comparison.

# Load the ggplot2 Library
library(ggplot2)

# Bar Chart for Lower Income Group:
lower_income_diabetes_data <- diabetes_by_race_income_proportions %>%
  filter(Income_Group == "Lower Income (< $50,000)")

ggplot(lower_income_diabetes_data, aes(x = Race_Group, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentage of Diagnosed Diabetes by Race Group (Lower Income)",
    x = "Race Group",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Adjust y-axis to prevent labels from being cut off

# Bar Chart for Higher Income Group:
higher_income_diabetes_data <- diabetes_by_race_income_proportions %>%
  filter(Income_Group == "Higher Income (>= $50,000)")

ggplot(higher_income_diabetes_data, aes(x = Race_Group, y = percentage)) +
  geom_bar(stat = "identity", fill = "darkred") + # Using a different color for distinction
  labs(
    title = "Percentage of Diagnosed Diabetes by Race Group (Higher Income)",
    x = "Race Group",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Adjust y-axis to prevent labels from being cut off

# Optional: Faceted Bar Chart (Combined View) with fixed y-axis
ggplot(diabetes_by_race_income_proportions, aes(x = Race_Group, y = percentage, fill = Income_Group)) +
  geom_bar(stat = "identity", position = "dodge") + # 'dodge' for side-by-side bars if not faceting
  facet_wrap(~ Income_Group, scales = "fixed") + # 'fixed' ensures consistent y-axis range across facets
  labs(
    title = "Percentage of Diagnosed Diabetes by Race Group and Income",
    x = "Race Group",
    y = "Percentage (%)",
    fill = "Income Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3.5,
            position = position_dodge(width = 0.9)) + # Adjust text position for dodged bars
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Adjust y-axis to prevent labels from being cut off

This revised worksheet provides the R code directly, allowing you to run the analysis and generate the plots in your R environment.
