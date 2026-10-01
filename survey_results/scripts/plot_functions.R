library(ggplot2)
library(ggtext)
library(dplyr)
library(stringr)
library(tidyr)

plot_bars <- function(question_id, x_label = NULL){
  
  plot_title <- codebook$question_text[codebook$question_id==question_id]
  plot_subtitle <- codebook$question_additional_text[codebook$question_id==question_id]
  
  responses |>
    select(matches(question_id)) |>
    drop_na() |>
    ggplot(aes( x = as.factor(.data[[question_id]]) )) +
    geom_bar(fill = "deepskyblue3", width = 0.75) +
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = -1, colour = "black") + 
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      y = NULL,
      x = x_label
    ) +
    scale_y_continuous(expand = expansion(mult = c(0,0.2))) +
    scale_x_discrete(drop=FALSE) +
    theme_light(base_size = 12) +
    theme(
      plot.title = element_textbox_simple(face = "bold", margin = unit(c(0, 0, 10, 0), "pt")),
      plot.subtitle = element_textbox_simple(face = "italic", margin = unit(c(0, 0, 15, 0), "pt"), size = 12),
      plot.title.position = "plot",
      plot.margin = margin(15, 10, 10, 15),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12, margin = margin(t = 5, "pt")),
      axis.title.x = element_text(margin = margin(t = 15, "pt")),
    )
}

plot_props <- function(q_suffix, legend_label){
  freq_levels <- c("reqs", "scope", "planning" , "timeline", "reporting", "collab", "risk", "knowledge", "review")
  freq_labels <- str_wrap(
    c("Requirements Gathering", "Scope Management", "Task Planning", "Timeline Management", "Progress Reporting", "Collaboration Management", "Risk Management", "Project Knowledge Management", "Project Review and Wrap-up"),
    20
  )
  
  legend_label <- str_wrap(legend_label, 20)

  transformed_resps <- responses |>
    select(ends_with(q_suffix)) |>
    pivot_longer(everything()) |>
    drop_na() |>
    mutate(name = str_remove(name, "_.*")) |>
    mutate(name = factor(name, levels = freq_levels, labels = freq_labels, ordered = TRUE)) |>
    count(name, value)
  
  
  ggplot(transformed_resps, aes(x = n, y = name, fill = value)) +
    geom_col(colour = "black", linewidth = 0.2) +
    geom_text(aes(label = n), colour = "gray30", position = position_stack(vjust = 0.5), size = 3) + 
    scale_y_discrete(limits=rev) +
    scale_x_continuous(limits = c(0, 15)) +
    theme_minimal(base_size = 12) +
    labs(
      fill = legend_label,
      y = "Aspect of Project Management",
      x = NULL
    ) + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    theme(
      plot.margin = margin(15, 10, 10, 15),
      panel.grid = element_blank(),
      axis.text.y = element_textbox(),
      axis.title.y = element_text(margin = margin(r = 10, unit = "pt")),
      legend.position = "bottom",
      legend.location = "plot"
    )
  
}

plot_strats <- function(question_id){
  # Split the character string into individual responses
  # Based on commas in the string, but not ones in parentheses
  res <- stringr::str_split(responses[[question_id]],  ",(?![^()]*\\)) ") |>
    unlist() |>
    as.data.frame(nm="strats") |>
    tidyr::drop_na() |>
    count(strats) |>
    mutate(strats = str_wrap(strats, 25)) # War the text for plot labels
  
  plot_title <- stringr::str_remove(
    codebook$question_text[codebook$question_id==question_id],
    ".*: ")
  
  ggplot(res, aes( x = n, y = reorder(strats, n))) +
    geom_col(fill = "deepskyblue3", width = 0.75) +
    geom_text(aes(label = n), hjust = -1, colour = "black") + 
    scale_x_continuous(expand = expansion(mult = c(0,0.2))) +
    labs(
      title = plot_title,
      y = NULL,
      x = NULL
    ) +
    theme_light(base_size = 12) +
    theme(
      plot.title = element_textbox_simple(face = "bold", margin = unit(c(7, 0, 10, 0), "pt")),
      plot.title.position = "plot",
      plot.margin = margin(15, 10, 10, 15),
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
    )
}
