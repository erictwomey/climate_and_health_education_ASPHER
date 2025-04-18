# Title: Climate and Health Education Data Analysis
# Author: Fadilah F Arsy, BPH
# Date: 2025-03-25
# Objective: Analyse data on LCDE indicators for climate & health education

# 1. Install and load required packages
packages <- c(
  "tidyverse",     # includes dplyr, ggplot2, readr, stringr, etc.
  "sf",            # for spatial data
  "rnaturalearth", # country shapefiles
  "rnaturalearthdata",
  "summarytools",  # for clean tables
  "janitor"        # for cleaning column names
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))



# 2. Load Data 
data_path <- "data/Europe_data_2024_CH_ed_indicator_data_use.csv"
data <- read_csv(data_path)

datapop_path <- "data/Total population EU 2024 (Only).csv"
eupop <- read_csv(datapop_path)



# 3. Clean data
data <- data %>%
  clean_names()

eupop <- eupop %>% 
  clean_names()



# 4. Define Yes/No columns to convert to binary (1 = Yes, 0 = No)
yes_no_cols <- c(
  "x2_1_has_ch_ed",
  "x2_2_1_has_doctoral_ch_ed",
  "x2_2_2_has_masters_ch_ed",
  "x2_2_3_has_undergrad_ch_ed",
  "x2_2_4_has_vocational_ch_ed",
  "x2_7_2_1_a_undergrad_competency_a",
  "x2_7_2_1_b_undergrad_competency_b",
  "x2_7_2_1_c_undergrad_competency_c",
  "x2_7_2_1_d_undergrad_competency_d",
  "x2_7_2_1_e_undergrad_competency_e",
  "x2_7_2_1_f_undergrad_competency_f",
  "x2_7_2_1_g_undergrad_competency_g",
  "x2_7_2_1_h_undergrad_competency_h",
  "x2_7_2_2_a_masters_competency_a",
  "x2_7_2_2_b_masters_competency_b",
  "x2_7_2_2_c_masters_competency_c",
  "x2_7_2_2_d_masters_competency_d",
  "x2_7_2_2_e_masters_competency_e",
  "x2_7_2_2_f_masters_competency_f",
  "x2_7_2_2_g_masters_competency_g",
  "x2_7_2_2_h_masters_competency_h",
  "x2_7_2_3_a_vocational_competency_a",
  "x2_7_2_3_b_vocational_competency_b",
  "x2_7_2_3_c_vocational_competency_c",
  "x2_7_2_3_d_vocational_competency_d",
  "x2_7_2_3_e_vocational_competency_e",
  "x2_7_2_3_f_vocational_competency_f",
  "x2_7_2_3_g_vocational_competency_g",
  "x2_7_2_3_h_vocational_competency_h",
  "x2_7_2_4_a_doctoral_competency_a",
  "x2_7_2_4_b_doctoral_competency_b",
  "x2_7_2_4_c_doctoral_competency_c",
  "x2_7_2_4_d_doctoral_competency_d",
  "x2_7_2_4_e_doctoral_competency_e",
  "x2_7_2_4_f_doctoral_competency_f",
  "x2_7_2_4_g_doctoral_competency_g",
  "x2_7_2_4_h_doctoral_competency_h"
)

# Apply Yes/No conversion only to selected columns
data <- data %>%
  mutate(x1_5_country = str_squish(x1_5_country)) %>%
  mutate(across(all_of(yes_no_cols), ~ if_else(str_squish(.) == "Yes", 1, 0)))



# *****************************************************************************

# 4. Analyse institutions offering C&H Education per Country 
# 4.1 Country institutions per country
country_counts <- data %>%
  group_by(x1_5_country) %>%
  summarise(
    total_institutions = n(),
    institutions_with_ch_ed = sum(x2_1_has_ch_ed == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_with_ch_ed = round(100 * institutions_with_ch_ed / sum(total_institutions), 1)
  )

View(country_counts)


# 4.2 Create double y axis figure
country_counts_long <- country_counts %>%
  pivot_longer(cols = c(total_institutions, institutions_with_ch_ed),
               names_to = "category", values_to = "value") %>%
  mutate(category = factor(category, levels = c("total_institutions", "institutions_with_ch_ed")))

# Scaling factor for percent to secondary axis
scale_factor <- max(country_counts$total_institutions, na.rm = TRUE) / 100

# Plot with custom legend
plot_institution_number <- ggplot() +
  # Bars for total and CH institutions
  geom_col(data = country_counts_long, aes(x = reorder(x1_5_country, -value), y = value, fill = category),
           position = "identity", width = ifelse(country_counts_long$category == "total_institutions", 0.6, 0.4)) +
  
  # Line and points for percent (invisible geom to trigger legend)
  geom_line(data = country_counts, aes(x = reorder(x1_5_country, -total_institutions), 
                                       y = percent_with_ch_ed * scale_factor, color = "Percent with CH Ed"),
            size = 1, show.legend = TRUE) +
  geom_point(data = country_counts, aes(x = reorder(x1_5_country, -total_institutions), 
                                        y = percent_with_ch_ed * scale_factor, color = "Percent with CH Ed"),
             size = 2, show.legend = TRUE) +
  
  # Text labels for percent
  geom_text(data = country_counts, aes(x = reorder(x1_5_country, -total_institutions), 
                                       y = institutions_with_ch_ed + 0.5, 
                                       label = paste0(percent_with_ch_ed, "%")),
            color = "orange", size = 3.5, vjust = 0) +
  
  
  # Custom fill and color legends
  scale_fill_manual(
    name = "Institution Type",
    values = c(
      "total_institutions" = "lightblue",
      "institutions_with_ch_ed" = "darkblue"
    ),
    labels = c("Total Institutions", "with Climate and Health Education")
  ) +
  scale_color_manual(
    name = "Line",
    values = c("Percent with CH Ed" = "orange")
  ) +
  
  # Axes and theme
  scale_y_continuous(
    name = "Number of Institutions",
    sec.axis = sec_axis(~ . / scale_factor, name = "% with CH Education from Total Institutions")
  ) +
  labs(
    title = "",
    x = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.title.y.right = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.text.y.right = element_text(size = 10),
    legend.position = "top",
    legend.box = "horizontal"
  )

print(plot_institution_number)



# 4.3 Plot map of proportion of institutions offering C&H Education
# Get map data
europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe")

# Add proportion column 
institutions_map_ch <- europe_map %>%
  left_join(country_counts, by = c("name" = "x1_5_country")) %>%
  mutate(
    proportion_ch_ed = if_else(!is.na(institutions_with_ch_ed),
                               institutions_with_ch_ed / total_institutions,
                               0)
  )

# Plot the map
map_institutions_proportion <- ggplot(institutions_map_ch) +
  geom_sf(aes(fill = proportion_ch_ed)) +
  scale_fill_gradient(
    low = "#c6dbef", high = "#08306b", na.value = "grey90",
    labels = scales::percent, name = "Proportion"
  ) +
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
  labs(
    title = "",
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(map_institutions_proportion)



# ******************************************************************************

# 5. Analyse mandatory vs elective courses
# Analyse and plot of mandatory vs elective programs
mandatory_elective <- tibble(
  education_level = c("vocational", "vocational",
                      "bachelor", "bachelor",
                      "master", "master",
                      "doctoral", "doctoral"),
  category = c("Mandatory", "Elective",
               "Mandatory", "Elective",
               "Mandatory", "Elective",
               "Mandatory", "Elective"),
  count = c(
    sum(data$x2_4_4_vocational_standalone_required_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_4_vocational_part_of_required_core_curriculum == 1, na.rm = TRUE),
    sum(data$x2_4_4_vocational_standalone_elective_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_4_vocational_part_of_non_required_curriculum == 1, na.rm = TRUE),
    
    sum(data$x2_4_3_undergrad_standalone_required_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_3_undergrad_part_of_required_core_curriculum == 1, na.rm = TRUE),
    sum(data$x2_4_3_undergrad_standalone_elective_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_3_undergrad_part_of_non_required_curriculum == 1, na.rm = TRUE),
    
    sum(data$x2_4_2_masters_standalone_required_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_2_masters_part_of_required_core_curriculum == 1, na.rm = TRUE),
    sum(data$x2_4_2_masters_standalone_elective_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_2_masters_part_of_non_required_curriculum == 1, na.rm = TRUE),
    
    sum(data$x2_4_1_doctoral_standalone_required_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_1_doctoral_part_of_required_core_curriculum == 1, na.rm = TRUE),
    sum(data$x2_4_1_doctoral_standalone_elective_course == 1, na.rm = TRUE) + 
      sum(data$x2_4_1_doctoral_part_of_non_required_curriculum == 1, na.rm = TRUE)
  )
)


# Compute percentages within each education level
total_by_level <- mandatory_elective %>%
  group_by(education_level) %>%
  summarise(total = sum(count), .groups = "drop")

mandatory_elective <- mandatory_elective %>%
  left_join(total_by_level, by = "education_level") %>%
  mutate(percent = round(100 * count / total, 1))


# Summary total Mandatory vs Elective
summary_mandatory_elective <- mandatory_elective %>%
  group_by(category) %>%
  summarise(
    total_count_me = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_percent_me = round(100 * total_count_me / sum(total_count_me), 1)
  )

print(summary_mandatory_elective)


# Plot Mandatory vs Elective Aggregated by Education
mandatory_elective$education_level <- factor(
  mandatory_elective$education_level,
  levels = c("vocational", "bachelor", "master", "doctoral")
)

mandatory_elective_plot <- ggplot(mandatory_elective, aes(x = education_level, y = percent, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)
  ) +
  labs(title = "",
       x = "",
       y = "Percentage of Institutions",
       fill = "Education Type") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(size = 10))

print(mandatory_elective_plot)



# ******************************************************************************

# 6.Student engagement
# 6.1 Analysis and plot of student engagement to the program
student_data <- data %>%
  select(x1_5_country,
         x1_7_students_in_entire_institution,
         x2_3_4_vocational_st_in_c_hed,
         x2_3_3_undergrad_st_in_c_hed,
         x2_3_2_masters_st_in_c_hed,
         x2_3_1_doctoral_st_in_c_hed) %>%
  group_by(x1_5_country) %>%
  summarise(
    vocational = sum(x2_3_4_vocational_st_in_c_hed, na.rm = TRUE),
    bachelor = sum(x2_3_3_undergrad_st_in_c_hed, na.rm = TRUE),
    master = sum(x2_3_2_masters_st_in_c_hed, na.rm = TRUE),
    doctoral = sum(x2_3_1_doctoral_st_in_c_hed, na.rm = TRUE),
    total_public_health_students = sum(x1_7_students_in_entire_institution, na.rm = TRUE),
    .groups = "drop"
  )

student_long <- student_data %>%
  pivot_longer(cols = c(vocational, bachelor, master, doctoral),
               names_to = "education_level", values_to = "students")

total_students_by_level <- student_long %>%
  group_by(education_level) %>%
  summarise(total_students = sum(students, na.rm = TRUE), .groups = "drop") %>%
  mutate(education_level = factor(education_level, levels = c("vocational", "bachelor", "master", "doctoral")))


# summary table with percentages
total_ph_students <- sum(student_data$total_public_health_students, na.rm = TRUE)


# Add percentage column to existing total_students_by_level table
student_percentage_summary <- total_students_by_level %>%
  mutate(
    percent_of_all_ph_students = round(100 * total_students / sum(total_students), 1)
  ) %>%
  rename(
    `Education Level` = education_level,
    `CH Students` = total_students,
    `% of Total PH Students` = percent_of_all_ph_students
  )


# View the result
print(student_percentage_summary)


# 6.2 Plot total students aggregated by education level
student_level_bar <- ggplot(total_students_by_level, aes(x = education_level, y = total_students, fill = education_level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_students), vjust = -0.3, size = 4.5) +
  scale_fill_manual(
    values = c(
      "vocational" = "#a6cee3",
      "bachelor" = "#1f78b4",
      "master" = "#fdbf6f",
      "doctoral" = "#ff7f00"
    )
  ) +
  labs(
    title = "",
    x = "",
    y = NULL,
    fill = "Education Level"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(student_level_bar)


# 6.3 Create map of proportion of students enrolled based on population data
student_per_population <- student_data %>%
  left_join(eupop, by = c("x1_5_country" = "country")) %>%
  mutate(student_proportion = total_public_health_students / population_2024)

europe_student_proportion <- europe_map %>%
  left_join(student_per_population, by = c("name" = "x1_5_country"))

# Plot the map showing student per population
student_proportion_map <- ggplot(europe_student_proportion) +
  geom_sf(aes(fill = student_proportion)) +
  scale_fill_gradient(
    low = "#c6dbef", high = "#08306b", na.value = "grey90",
    labels = scales::percent_format(accuracy = 0.01),
    name = "% of Population"
  ) +
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
  labs(
    title = "",
    subtitle = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(student_proportion_map)


