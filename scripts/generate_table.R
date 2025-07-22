library(tidyverse)
library(knitr)
library(kableExtra)
library(changeofevidence)
library(patchwork)

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

generate_coe_table <- function() {
  
  coe <- readRDS("data/coe_2025-07-14.rds") %>%
    select(
      Study,
      Experimental,
      N,
      MaxBF,
      `MaxBF p`,
      Energy,
      `Energy p`,
      `FFT Amplitude sum`,
      `FFT p`,
      Direction,
      Labstudy
    )
  
  # exclude games from links
  links <- links[-which(links == "studies/games.html")]
  links <- setNames(links, coe$Study)
  
  ft <- coe %>%
    mutate(
      across(where(is.numeric), ~ round(., 2)),
      `MaxBF p` = cell_spec(`MaxBF p`, bold = ifelse(`MaxBF p` < 0.05, TRUE, FALSE)),
      `Energy p` = cell_spec(`Energy p`, bold = ifelse(`Energy p` < 0.05, TRUE, FALSE)),
      `FFT p` = cell_spec(`FFT p`, bold = ifelse(`FFT p` < 0.05, TRUE, FALSE)),
      Experimental = ifelse(Experimental, "✔", "✖"),
      Experimental = cell_spec(Experimental, color = ifelse(Experimental == "✔", "darkgreen", "red")),
      `Labstudy` = ifelse(Labstudy, "🏠", "🌍"),
      Study = text_spec(Study, link = links[Study])
    )
  
  kbl(ft, escape = F,
      col.names = c("Study",
                    "Effect expected",
                    "N",
                    "MaxBF",
                    "p",
                    "Energy",
                    "p",
                    "Asum",
                    "p",
                    "Direction",
                    "Lab/Online"),
      align = "lcccccccccc") %>%
    kable_classic("hover", fixed_thead = T) %>%
    add_header_above(c(" " = 3, "Max BF" = 2, "BF Energy" = 2, "FFT Amplitude sum" = 2, " " = 2))
}



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


####

#Generate plots
generate_bfplot <- function(data){
  plotbf(data)
}

generate_rwplot <- function(data, n_bits) {
  offset <- n_bits / 2
  
  # Determine whether data is a list or single vector
  if (is.list(data)) {
    rw <- lapply(data, function(x) cumsum(x - offset))
  } else if (is.numeric(data)) {
    rw <- cumsum(data - offset)
  } else {
    stop("`data` must be a numeric vector or a list of numeric vectors.")
  }
  
  # Call plot function
  changeofevidence::plotrw(rw, n_bits = n_bits)
}

generate_coeplot <- function(data) {
  plots_row1 <- list()
  plot_row2 <- NULL
  
  common_theme <- ggplot2::theme(text = ggplot2::element_text(size = 10))
  
  p1 <- plotbf(data)+
    ggplot2::geom_point(aes(x = which.max(data), y = max(data)), color = "red", size = 3) +
    ggplot2::geom_text(aes(x = which.max(data), y = max(data),
                           label = paste("Max BF:", round(max(data), 2))), vjust = -1) +
    ggplot2::ggtitle("Maximum BF") +
    common_theme
  plots_row1 <- c(plots_row1, list(p1))
  
  p2 <- plotbf(data) +
    ggplot2::geom_ribbon(
      aes(x = 1:length(data), ymin = pmin(data, 1), ymax = 1),
      fill = "red", alpha = 0.3
    ) +
    ggplot2::geom_ribbon(
      aes(x = 1:length(data), ymin = 1, ymax = pmax(data, 1)),
      fill = "green", alpha = 0.3
    ) +
    ggplot2::ggtitle("Energy of BF") +
    ggplot2::theme(axis.title.y = element_blank()) +  # remove y-axis label
    common_theme
  plots_row1 <- c(plots_row1, list(p2))
  
  fft <- changeofevidence::fftcreate(data)
  p3 <- plotfft(fft) +
    ggplot2::ggtitle("FFT Test") +
    common_theme
  plot_row2 <- p3
  
  combined <- (patchwork::wrap_plots(plots_row1) / plot_row2)
  
  print(combined)
}

