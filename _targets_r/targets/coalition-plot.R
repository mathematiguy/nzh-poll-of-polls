coalition_data <- function(seats) {
  seats |>
    select(National,
           Labour,
           ACT,
           Green,
           `NZ First`,
           `Te Pāti Māori`,
           NatAct,
           NatActNZF,
           LabGreen,
           LabGreenNZF,
           LabGreenMaori) |>
    gather(Coalition, Seats) |>
    mutate(lab_in = ifelse(grepl("^Lab", Coalition), "Labour-based", "National-based")) |>
    mutate(
      Coalition = gsub("^LabGreen", "Labour, Greens", Coalition),
      Coalition = gsub("^NatAct", "National, ACT", Coalition),
      Coalition = gsub("NZF$", ", NZ First", Coalition),
      Coalition = gsub("Maori", ",\nTe Pāti Māori", Coalition)
    )
}

coalition_plot <- function(data) {
  current_date <- format(Sys.Date(), "%B %d")
  data |>
    ggplot(aes(x = Seats, fill = lab_in)) +
    geom_histogram(
      alpha = 0.5,
      binwidth = 1,
      position = "identity",
      colour = NA
    )  +
    scale_y_continuous() +
    scale_fill_manual(values = c('#d82c20', '#065BAA')) +
    theme_clean() +
    theme(
      legend.position = 'none',
      strip.background = element_rect(fill = '#121617'),
      strip.text = element_text(colour = 'white'),
      plot.background = element_blank()
    ) +
    labs(y = NULL, title = paste("Coalition seat estimates as of\n", current_date)) +
    annotate(
      'segment',
      x = 61,
      xend = 61,
      y = 0,
      yend = Inf
    )
}

party_seats_data <- function(data) {
  data |> 
    dplyr::select(Coalition, Seats) |> 
    dplyr::filter(Coalition %in% c("Labour", "National", "ACT", "Green", "NZ First", "Te Pāti Māori")) |>
    mutate(lab_in = ifelse(grepl("^Lab", Coalition), "Labour-based", "National-based"))
}

facet_party_plot <- function(data) {
  current_date <- format(Sys.Date(), "%B %d")
  data %>%
    filter(Coalition %in% c("Labour", "National", "ACT", "Green", "NZ First", "Te Pāti Māori")) %>%
    ggplot(aes(x = Seats, fill = Coalition)) +
    geom_histogram(
      aes(y = ..count../8000),  # Modifying the y-axis here
      alpha = 0.5,
      binwidth = 1,
      position = "identity",
      colour = NA
    ) +
    facet_wrap(~Coalition, scales = "free", ncol = 2) +
    scale_x_continuous(breaks = function(x) {
      if (diff(range(x)) <= 10) return(seq(floor(min(x)), ceiling(max(x)), by = 1))
      else if (diff(range(x)) <= 20) return(seq(floor(min(x)), ceiling(max(x)), by = 2))
      else return(seq(floor(min(x)), ceiling(max(x)), by = 5))
    }) +
    scale_y_continuous() +
    scale_fill_manual(values = c(
      'Labour' = '#d82c20',
      'National' = '#065BAA',
      'ACT' = '#E6B800',         # A darker shade of yellow
      'Green' = '#228B22',
      'NZ First' = '#505050',
      'Te Pāti Māori' = '#800000' # Maroon
    )) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      strip.background = element_rect(fill = '#121617'),
      strip.text = element_text(colour = 'white'),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),  # Remove major grid
      panel.grid.minor = element_blank(),   # Remove minor grid
      plot.title = element_text(hjust = 0.5, face = "bold")  # Center the title
    ) +
    labs(
      y = NULL,
      title = paste("Party seat estimates as of", current_date)
    )
}

list(
  tar_target(election_night_data, coalition_data(seats_election_night)),
  tar_target(election_night_plot, {
    election_night_data |>
      filter(Coalition %in% c("National, ACT",
                              "National, ACT, NZ First",
                              "Labour, Greens",
                              "Labour, Greens,\nTe Pāti Māori")) |>
      coalition_plot()
  }),
  tar_file(election_night620, {
    f <- "output/election_night620.svg"
    ggsave(
      f,
      plot = election_night_plot +
        facet_wrap(~ factor(
          Coalition,
          levels = c(
            "Labour, Greens",
            "Labour, Greens,\nTe Pāti Māori",
            "National, ACT",
            "National, ACT, NZ First"
          )
        ), ncol = 2),
      dpi = 100,
      width = 6.2,
      height = 4
    )
    f
  }),
  tar_file(election_night375, {
    f <- "output/election_night375.svg"
    ggsave(
      f,
      plot = election_night_plot +
        facet_wrap(~ factor(Coalition,
                   levels = c(
                     "Labour, Greens",
                     "Labour, Greens,\nTe Pāti Māori",
                     "National, ACT",
                     "National, ACT, NZ First"
                   )),
                   ncol = 1),
      dpi = 100,
      width = 3.75,
      height = 7
    )
    f
  }),
  tar_file(party620, {
    f <- "output/party620.svg"
    ggsave(
      f,
      plot = election_night_plot +
        facet_wrap(~ factor(
                     Coalition,
                     levels = c(
                       "Labour",
                       "National",
                       "Greens",
                       "ACT",
                       "Te Pāti Māori",
                       "NZ First"
                     )
                   ), ncol = 2),
      dpi = 100,
      width = 6.2,
      height = 4
    )
    f
  }),
  tar_file(party375, {
    f <- "output/party375.svg"
    ggsave(
      f,
      plot = election_night_plot +
        facet_wrap(~ Coalition,
                   ncol = 1),
      dpi = 100,
      width = 3.75,
      height = 7
    )
    f
  }),
  tar_target(party_data, party_seats_data(election_night_data)),
  tar_target(party_plot, facet_party_plot(party_data)),
  tar_file(party_seats_output, {
    f <- "output/party_seats_plot.svg"
    ggplot2::ggsave(
      f,
      plot = party_plot,
      dpi = 100,
      width = 5,
      height = 4
    )
    f
  })
)
