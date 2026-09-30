library(ggplot2)
library(ggtext)
library(dplyr)
library(stringr)
library(tidyr)

plot_bars <- function(question_id, x_label = NULL){
  
  plot_title <- codebook$question_text[codebook$col_id==question_id]
  plot_subtitle <- codebook$question_additional_text[codebook$col_id==question_id]
  
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
    codebook$question_text[codebook$col_id==question_id],
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
    theme_light(base_size = 16) +
    theme(
      plot.title = element_textbox_simple(face = "bold", margin = unit(c(7, 0, 10, 0), "pt")),
      plot.title.position = "plot",
      plot.margin = margin(15, 10, 10, 15),
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
    )
}
