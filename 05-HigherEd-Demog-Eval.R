library(tidyverse)
library(dplyr)
library(readxl)
library(chisq.posthoc.test)
library(ggplot2)
library(ggeffects)
library(broom)
library(gt)
library(here)

here()

# Load data
students <- read_excel(here("100-Projects", "05-HigherEd", "data", "complete_DH minor students_2011-2024.xlsx"), sheet = "student list")

apps <- read_excel(here("100-Projects", "05-HigherEd", "data", "2017-2024_DH-Minor-Applications.xlsx"))

# Clean column names (snake_case)
students <- students %>% clean_names()
apps <- apps %>% clean_names()

# Remove duplicate rows
students <- students %>% distinct()
apps <- apps %>% distinct()

apps <- apps %>% rename("pronouns" = "what_are_your_pronouns_e_g_he_him_her_hers_they_theirs_xe_xem_etc")

# Trim whitespace from all character columns
apps <- apps %>% mutate(across(where(is.character), str_trim))

glimpse(students)
glimpse(apps)

# Create some useful derived fields:
students <- students %>%
  mutate(
    URM = raceethnic %in% c("Hispanic","Black Non-Hispanic","American Indian or Alaskan Native"))

# Join pronouns column from apps to students dataset
students <- students %>%
  left_join(apps %>% select(ucla_id, pronouns), by = "ucla_id")

# View the result
head(students)

# Use pronouns column to update the gender column with additional information.
students <- students %>%
  mutate(gender = case_when(
    is.na(gender) & pronouns == "he/him" ~ "Male",
    is.na(gender) & pronouns == "she/her" ~ "Female",
    is.na(gender) & pronouns == "they" ~ "Non-binary",
    TRUE ~ gender  # Retain the original value if no condition is met
  ))

# Update the variable regarding transfer status and the classes to improve clarity
students <- students %>%
  mutate(entry_freshman_transfer = case_when(
    entry_freshman_transfer == "Freshman" ~ "Non-Transfer",
    entry_freshman_transfer == "Transfer" ~ "Transfer"))

students <- students %>% rename("transfer_status" = "entry_freshman_transfer")

## Exploratory analysis potential correlations between demographic variables.
## Are students in one under-represented category likely to be in others?
## What does this indicate about the program's diversity and the needs of its students?

# Frequency of transfer status by race/ethnicity
students %>%
  filter(!is.na(raceethnic)) %>%
  filter(!is.na(transfer_status)) %>%
  count(raceethnic, transfer_status) %>%
  group_by(raceethnic) %>%
  arrange(raceethnic, desc(n)) %>%
  slice_head(n = 5)

raceeth_transfer <- table(students$raceethnic, students$transfer_status)
raceeth_transfer

chisq.test(raceeth_transfer)
fisher.test(raceeth_transfer, simulate.p.value=TRUE) # significant

chisq.posthoc.test(raceeth_transfer, method = "bonferroni") # There appears to be a strong correlation between transfer status and Hispanic ethnicity.

#  Residuals: -4.9740731 (this indicates a significant difference)
# p-value: 0.0000090 (highly significant; there is a significant difference between Freshman and Transfer students in the Hispanic category)

# NOTE: visualizations have already been created in Tableau. See the dashboards available at https://public.tableau.com/app/profile/ashley.sanders2190/vizzes.


###############################
## Student Academic Outcomes ##
###############################

minor_awarded <- read_excel(here("100-Projects", "05-HigherEd", "data", "complete_DH minor students_2011-2024.xlsx"), sheet = "minor awarded")

grads <- students %>% filter(cal_year < 2023)

# Ensure one record per student in minor_awarded
minor_awarded_clean <- minor_awarded %>%
  distinct(ucla_id, .keep_all = TRUE) %>%  # Keep only the first record per ucla_id
  mutate(completed = 1)

# Merge datasets to identify which students completed the minor
student_outcomes <- grads %>%
  left_join(minor_awarded_clean %>% select(ucla_id, completed), by = "ucla_id") %>%
  mutate(completed = ifelse(is.na(completed), 0, completed))

# Analyze completion rates by demographic variables
completion_summary <- student_outcomes %>%
  group_by(raceethnic, gender, first_gen_bachelors, transfer_status) %>%
  summarise(
    total_students = n(),
    completed = sum(completed),
    completion_rate = mean(completed)
  ) %>%
  arrange(desc(completion_rate))

rate <- completion_summary$completed/completion_summary$total_students
completion_summary$rate <- rate

# Display summary
print(completion_summary)

# Group by race/ethnicity and analyzing completion rates
race_completion_summary <- completion_summary %>%
  group_by(raceethnic) %>%
  summarise(
    total_students = sum(total_students),
    completed = sum(completed),
    completion_rate = completed / total_students
  ) %>%
  arrange(desc(completion_rate))

# Display the results
print(race_completion_summary)

ggplot(race_completion_summary, aes(x = reorder(raceethnic, -completion_rate), y = completion_rate)) +
  geom_bar(stat = "identity", fill = "#003B5C") +
  coord_flip() +
  labs(title = "DH Minor Completion Rate by Race/Ethnicity", x = "Race/Ethnicity", y = "Completion Rate") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Group by gender and analyzing completion rates
gender_completion_summary <- completion_summary %>%
  group_by(gender) %>%
  summarise(
    total_students = sum(total_students),
    completed = sum(completed),
    completion_rate = completed / total_students
  ) %>%
  arrange(desc(completion_rate))

# Display the results
print(gender_completion_summary)

# Group by first generation status and analyzing completion rates
first_gen_completion_summary <- completion_summary %>%
  group_by(first_gen_bachelors) %>%
  summarise(
    total_students = sum(total_students),
    completed = sum(completed),
    completion_rate = completed / total_students
  ) %>%
  arrange(desc(completion_rate))

# Display the results
print(first_gen_completion_summary)

# Group by transfer status and analyzing completion rates
transfer_completion_summary <- completion_summary %>%
  group_by(transfer_status) %>%
  summarise(
    total_students = sum(total_students),
    completed = sum(completed),
    completion_rate = completed / total_students
  ) %>%
  arrange(desc(completion_rate))

# Display the results
print(transfer_completion_summary)

##############################################
## Deeper Analysis of Completion Likelihood ##
##############################################

# Logistic regression to statistically test differences in completion likelihood

# Convert categorical variables to factors
student_outcomes$URM <- factor(student_outcomes$URM, levels = c(FALSE, TRUE),
                               labels = c("Non-URM", "URM"))
student_outcomes$gender <- factor(student_outcomes$gender, levels = c("Female", "Male"), labels = c("Female", "Male"))

student_outcomes$first_gen_bachelors <- factor(student_outcomes$first_gen_bachelors, levels = c("First Gen", "Not First Gen", "Unknown/Other"), labels = c("First Gen", "Not First Gen", "Unknown/Other"))

student_outcomes$transfer_status <- factor(student_outcomes$transfer_status, levels = c("Non-Transfer", "Transfer"), labels = c("Non-Transfer", "Transfer"))

completion_model <- glm(completed ~ raceethnic + gender + first_gen_bachelors + transfer_status,
                        data = student_outcomes, family = binomial)

summary(completion_model)

# Extract Odds Ratios
exp(coef(completion_model))

# Extract confidence intervals for the odds ratios
exp(confint(completion_model))

#######################################################
## Exlploring outcomes for Intersectional Identities ##
#######################################################

# Remove null and uncommon values
student_outcomes_filtered <- student_outcomes %>%
  filter(
    !is.na(URM),
    !is.na(gender),
    !is.na(first_gen_bachelors),
    !is.na(transfer_status),
    first_gen_bachelors != "Unknown/Other"
  )

# Drop unused factor levels from all variables
student_outcomes_filtered <- droplevels(student_outcomes_filtered)

# Create the logistic regression model with pair-wise interactions
full_interaction_model_filtered <- glm(
  completed ~ (URM + gender + first_gen_bachelors + transfer_status)^2,
  data = student_outcomes_filtered,
  family = binomial
)

# Examine interaction terms
summary(full_interaction_model_filtered)

# Extract Odds Ratios & format as strings with fixed digits
format(exp(coef(full_interaction_model_filtered)), digits = 3, scientific = FALSE)

# Create valid combinations
newdata <- expand.grid(
  URM = levels(student_outcomes_filtered$URM),
  gender = levels(student_outcomes_filtered$gender),
  first_gen_bachelors = levels(student_outcomes_filtered$first_gen_bachelors),
  transfer_status = levels(student_outcomes_filtered$transfer_status)
)

# Predict probabilities for each combination
newdata$predicted <- predict(full_interaction_model_filtered, newdata = newdata, type = "response")

# Clean and format
pred_table <- newdata %>%
  mutate(`Predicted Probability` = round(predicted, 3)) %>%
  select(URM, gender, first_gen_bachelors, transfer_status, `Predicted Probability`)

pred_table # View

# Create a polished table for export to HMTL
# Tidy the logistic model with ORs and significance stars
or_table <- tidy(full_interaction_model_filtered) %>%
  mutate(
    `Odds Ratio` = round(exp(estimate), 3),
    `CI Lower` = round(exp(estimate - 1.96 * std.error), 3),
    `CI Upper` = round(exp(estimate + 1.96 * std.error), 3),
    `p-value` = round(p.value, 4),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  select(term, `Odds Ratio`, `CI Lower`, `CI Upper`, `p-value`, Significance) %>%
  rename(`Term` = term)

# Create the table
or_gt <- or_table %>%
  gt() %>%
  tab_header(
    title = "Logistic Regression Results: Odds Ratios and Significance"
  ) %>%
  fmt_number(columns = 2:4, decimals = 3) %>%
  tab_style(
    style = list(cell_fill(color = "#E6F2FF")),
    locations = cells_body(rows = Significance != "")
  ) %>%
  cols_label(
    Term = "Variable",
    `Odds Ratio` = "OR",
    `CI Lower` = "95% CI (Lower)",
    `CI Upper` = "95% CI (Upper)",
    `p-value` = "p",
    Significance = ""
  )

# Export to HTML
gtsave(or_gt, "logistic_regression_odds_ratios.html")

# Create an HTML export of the probability table:

# Sort and flag the top 2 and bottom 2 rows
pred_table_flagged <- pred_table %>%
  arrange(`Predicted Probability`) %>%
  mutate(
    row_highlight = case_when(
      row_number() <= 2 ~ "low",
      row_number() > n() - 2 ~ "high",
      TRUE ~ "none"
    )
  )

# Create the styled table with accessible colors
pred_gt <- pred_table_flagged %>%
  gt() %>%
  tab_header(
    title = "Predicted Probabilities of DH Minor Completion"
  ) %>%
  fmt_number(columns = "Predicted Probability", decimals = 3) %>%
  tab_style(
    style = cell_fill(color = "#f4a582"),  # soft orange for lowest
    locations = cells_body(rows = row_highlight == "low")
  ) %>%
  tab_style(
    style = cell_fill(color = "#92c5de"),  # soft blue for highest
    locations = cells_body(rows = row_highlight == "high")
  ) %>%
  cols_hide(columns = "row_highlight")

# Save to HTML
gtsave(pred_gt, "highlighted_predicted_probabilities_accessible.html")


# Because intersectional identities matter in real ways, we'll do one more exploration of interactions to ensure we're not missing an important effect, this time between 3 demographic variables.
# Base model with 2-way interactions only
base_model <- glm(
  completed ~ (URM + gender + first_gen_bachelors + transfer_status)^2,
  data = student_outcomes_filtered,
  family = binomial
)

# Fit models with 3-way interactions
# Models with individual 3-way interactions added
model_1 <- glm(completed ~ (URM + gender + first_gen_bachelors + transfer_status)^2 +
                 URM:gender:first_gen_bachelors,
               data = student_outcomes_filtered, family = binomial)

model_2 <- glm(completed ~ (URM + gender + first_gen_bachelors + transfer_status)^2 +
                 URM:gender:transfer_status,
               data = student_outcomes_filtered, family = binomial)

model_3 <- glm(completed ~ (URM + gender + first_gen_bachelors + transfer_status)^2 +
                 URM:first_gen_bachelors:transfer_status,
               data = student_outcomes_filtered, family = binomial)

model_4 <- glm(completed ~ (URM + gender + first_gen_bachelors + transfer_status)^2 +
                 gender:first_gen_bachelors:transfer_status,
               data = student_outcomes_filtered, family = binomial)

# AIC comparison
AIC(base_model, model_1, model_2, model_3, model_4)

# Likelihood ratio tests to compare each model to the base
anova(base_model, model_1, test = "Chisq")
anova(base_model, model_2, test = "Chisq")
anova(base_model, model_3, test = "Chisq")
anova(base_model, model_4, test = "Chisq")

# None of the 3-way interaction models significantly improved fit over the base model (according to the likelihood ratio tests / chi-square p-values), so there’s no strong statistical justification to extract the odds ratios and predictive probabilities.