coalition_data <- function(seats) {
  seats  |>
    select(National,
           Labour,
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
    labs(y = NULL) +
    annotate(
      'segment',
      x = 61,
      xend = 61,
      y = 0,
      yend = Inf
    )
}

list(
  tar_target(election_night_data, coalition_data(seats_election_night)),
  tar_target(nowcast_data, coalition_data(seats_nowcast)),
  tar_target(election_night_plot, coalition_plot(election_night_data)),
  tar_target(nowcast_plot, coalition_plot(nowcast_data)),
  tar_file(election_night620, {
    f <- "output/election_night620.svg"
    ggsave(
      f,
      plot = election_night_plot +
        facet_wrap(~ factor(
          Coalition,
          levels = c(
            "Labour",
            "National",
            "Labour, Greens",
            "National, ACT",
            "National, ACT, NZ First",
            "Labour, Greens,\nTe Pāti Māori",
            "Labour, Greens, NZ First"
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
        facet_wrap(~ Coalition,
                   ncol = 1),
      dpi = 100,
      width = 3.75,
      height = 7
    )
    f
  }),
  tar_file(nowcast620, {
    f <- "output/nowcast620.svg"
    ggsave(
      f,
      plot = nowcast_plot +
        facet_wrap(~ factor(
          Coalition,
          levels = c(
            "Labour",
            "National",
            "Labour, Greens",
            "National, ACT",
            "Labour, Greens,\nTe Pāti Māori"
          )
        ), ncol = 2),
      dpi = 100,
      width = 6.2,
      height = 4
    )
    f
  }),
  tar_file(nowcast375, {
    f <- "output/nowcast375.svg"
    ggsave(
      f,
      plot = nowcast_plot +
        facet_wrap(~ Coalition,
                   ncol = 1),
      dpi = 100,
      width = 3.75,
      height = 7
    )
    f
  })
)
