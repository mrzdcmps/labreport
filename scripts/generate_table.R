library(tidyverse)
library(knitr)
library(kableExtra)

links = c(
  "studies/lovekind.html",
  "studies/prayer.html",
  "studies/monks.html",
  "studies/monks.html",
  "studies/monks.html",
  "studies/monks.html",
  "studies/meditation.html",
  "studies/meditation.html",
  "studies/sound.html",
  "studies/allgood.html",
  "studies/bandit.html",
  "studies/cointoss.html",
  "studies/incongruence.html",
  "studies/incongruence.html",
  "studies/smokers1.html",
  "studies/smokers1.html",
  "studies/smokers2.html",
  "studies/smokers2.html",
  "studies/smokers3.html",
  "studies/psysc.html",
  "studies/psysc.html",
  "studies/psysc.html",
  "studies/psysc.html",
  "studies/psysc.html",
  "studies/psysc.html",
  "studies/relax.html",
  "studies/priming1.html",
  "studies/priming1.html",
  "studies/priming2.html",
  "studies/priming2.html",
  "studies/priming3.html",
  "studies/priming3.html",
  "studies/priming4.html",
  "studies/priming4.html",
  "studies/erotic1.html",
  "studies/erotic2.html",
  "studies/smokers-priming.html",
  "studies/smokers-priming.html",
  "studies/sobjectivity.html",
  "studies/sobjectivity.html",
  "studies/sobjectivity.html",
  "studies/epsi.html",
  "studies/epsi.html",
  "studies/robots.html",
  "studies/games.html",
  "studies/schroedinger.html",
  "studies/schroedinger.html",
  "studies/desire.html",
  "studies/stories.html",
  "studies/stories.html",
  "studies/willpower.html",
  "studies/willpower.html",
  "studies/baseline1.html",
  "studies/baseline1.html",
  "studies/baseline2.html"
)

generate_table <- function(experiment_name) {
  
  if(experiment_name == ""){
    #load("data/study_objects.RData", envir = .GlobalEnv) # not needed for overall table
    st <- readRDS("data/overview2025-07-12.rds")
    links <- setNames(links, st$Study)
  } else {
    load("../data/study_objects.RData", envir = .GlobalEnv) # load individual study data
    st <- readRDS("../data/overview2025-07-12.rds")
  }
  
  ft <- st %>%
    filter(grepl(experiment_name, Study)) %>%
    mutate(
      across(where(is.numeric), ~ round(., 2)),
      `Hits (%)` = formattable::color_tile("coral2", "cornflowerblue")(`Hits (%)`),
      p = cell_spec(p, bold = ifelse(p < 0.05, TRUE, FALSE)),
      Experimental = ifelse(Experimental, "✔", "✖"),
      Experimental = cell_spec(Experimental, color = ifelse(Experimental == "✔", "darkgreen", "red")),
      Labstudy = ifelse(Labstudy, "🏠", "🌍")
    )
  
  if(experiment_name == ""){
    ft <- ft %>%
      mutate(Study = text_spec(Study, link = links[Study]))
  }
  
  kbl(ft, escape = F,
      col.names = c("Study",
                    "Effect expected",
                    "N",
                    "Trials",
                    "M",
                    "SD",
                    "Hits (%)",
                    "t",
                    "p",
                    "ES",
                    "Var",
                    "BF",
                    "Direction",
                    "Year",
                    "Lab/Online"),
      align = "lcccccccccccccc") %>%
    kable_classic("hover") %>%
    kable_styling(
      fixed_thead = T,
      html_font = "Arial",
      bootstrap_options = c("striped", "hover", "condensed")
    )
}

# generate_table_all <- function() {
#   st <- readRDS("data/overview2025-07-12.rds")
#   
#   # Make sure `links` is a named vector like: c("Study A" = "http://...", ...)
#   # You can load or define `links` here or assume it’s in the global environment
#   
#   links <- setNames(links, st$Study)
#   
#   ft <- st %>%
#     mutate(
#       across(where(is.numeric), ~ round(., 2)),
#       `Hits (%)` = formattable::color_tile("coral2", "cornflowerblue")(`Hits (%)`),
#       p = cell_spec(p, bold = ifelse(p < 0.05, TRUE, FALSE)),
#       Experimental = ifelse(Experimental, "✔", "✖"),
#       Experimental = cell_spec(Experimental, color = ifelse(Experimental == "✔", "darkgreen", "red")),
#       Labstudy = ifelse(Labstudy, "🏠", "🌍"),
#       Study = text_spec(Study, link = links[Study])  # Match by study name
#       #Study = text_spec(Study, link = links)
#     )
#   
#   kbl(ft, escape = FALSE,
#       col.names = c("Study",
#                     "Effect expected",
#                     "N",
#                     "Trials",
#                     "M",
#                     "SD",
#                     "Hits (%)",
#                     "t",
#                     "p",
#                     "ES",
#                     "Var",
#                     "BF",
#                     "Direction",
#                     "Year",
#                     "Lab/Online"),
#       align = "lcccccccccccccc") %>%
#     kable_classic("hover") %>%
#     kable_styling(
#       fixed_thead = TRUE,
#       html_font = "Arial",
#       bootstrap_options = c("striped", "hover", "condensed")
#     )
# }
# 
# 
# generate_table <- function(experiment_name) {
#   load("../data/study_objects.RData", envir = .GlobalEnv)
#   st <- readRDS("../data/overview2025-07-12.rds")
#   
#   ft <- st %>%
#     filter(grepl(experiment_name, Study)) %>%
#     mutate(
#       across(where(is.numeric), ~ round(., 2)),
#       `Hits (%)` = formattable::color_tile("coral2", "cornflowerblue")(`Hits (%)`),
#       p = cell_spec(p, bold = ifelse(p < 0.05, TRUE, FALSE)),
#       Experimental = ifelse(Experimental, "✔", "✖"),
#       Experimental = cell_spec(Experimental, color = ifelse(Experimental == "✔", "darkgreen", "red")),
#       Labstudy = ifelse(Labstudy, "🏠", "🌍")
#       # No Study = text_spec() here
#     )
#   
#   kbl(ft, escape = FALSE,
#       col.names = c("Study",
#                     "Effect expected",
#                     "N",
#                     "Trials",
#                     "M",
#                     "SD",
#                     "Hits (%)",
#                     "t",
#                     "p",
#                     "ES",
#                     "Var",
#                     "BF",
#                     "Direction",
#                     "Year",
#                     "Lab/Online"),
#       align = "lcccccccccccccc") %>%
#     kable_classic("hover") %>%
#     kable_styling(
#       fixed_thead = TRUE,
#       html_font = "Arial",
#       bootstrap_options = c("striped", "hover", "condensed")
#     )
# }



generate_demographics_table <- function(data, age_col, gender_col, male_value, female_value, other_value = NULL) {
  # Data
  total_participants <- nrow(data)
  mean_age <- mean(data[[age_col]], na.rm = TRUE)
  sd_age <- sd(data[[age_col]], na.rm = TRUE)
  
  # Gender counts
  female_count <- sum(data[[gender_col]] == female_value, na.rm = TRUE) / total_participants * 100
  male_count <- sum(data[[gender_col]] == male_value, na.rm = TRUE) / total_participants * 100
  
  # Initialize table data
  table_data <- data.frame(
    Characteristic = c("N", "Female", "Male", "Mean Age", "SD Age"),
    Count_Statistics = c(
      total_participants,
      paste0(round(female_count, 2), "%"),
      paste0(round(male_count, 2), "%"),
      paste0(round(mean_age, 2), " years"),
      paste0(round(sd_age, 2), " years")
    )
  )
  
  # Add "Other" if provided
  if (!is.null(other_value)) {
    other_count <- sum(data[[gender_col]] == other_value, na.rm = TRUE) / total_participants * 100
    table_data <- table_data %>%
      add_row(
        Characteristic = "Other",
        Count_Statistics = paste0(round(other_count, 2), "%")
      )
    # Reorder to maintain logical order (N, Female, Male, Other, Mean Age, SD Age)
    table_data <- table_data[c(1:3, 6, 4:5),]
  }
  
  # Print the table using kable
  kable(table_data, format = "html", col.names = c("Characteristic", "Count/Statistic")) %>%
    kable_styling(
      full_width = FALSE
      )
}
