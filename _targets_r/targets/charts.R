list(
  tar_target(
    voting_intention_chartdata,
    model2023_summary_model2023 |>
      filter(str_starts(variable, "mu")) |>
      mutate(
        Party = rep(parties_ss, each = sum(weeks_between_elections)),
        week = rep(1:sum(weeks_between_elections), length(parties_ss)),
        week = min(election_weeks) + weeks(week)
      ) |>
    select(Party, week, mean, q5, q95)
  ),
  tar_target(
    voting_intention_chart,
    voting_intention_chartdata |>
      ggplot(aes(
        x = week,
        y = mean,
        colour = Party,
        fill = Party
      )) +
      geom_point(
        data = gather(
          polls2,
          Party,
          VotingIntention,-Pollster,-MidDate,-ElectionYear,-MidDateNumber
        ) |>
          filter(VotingIntention != 0),
        aes(x = MidDate, y = VotingIntention),
        colour = "black",
        size = 0.5
      ) +
      geom_vline(
        xintercept = as.numeric(election_dates),
        colour = "grey60"
      ) +
    scale_y_continuous(labels = percent) +
    scale_x_date(breaks = ym(
                   c(#'2016-01', '2018-01', '2020-01',
                     '2022-01', '2024-01')
                 ), date_labels = '%Y',
                 limits = as.Date(c("2022-01-01", "2024-01-01"))) +
      scale_fill_manual(values = party_colours$Colour, breaks = party_colours$Party) +
      geom_line(
        data = voting_intention_chartdata |> filter(week <= today()),
        colour = "black"
      ) +
      geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3, colour = NA) +
      labs(y = NULL, x = NULL) +
      theme_clean() +
      theme(
        plot.title.position = "plot",
        legend.position = 'none',
        strip.background = element_rect(fill = '#121617'),
        strip.text = element_text(colour = 'white'),
        plot.background = element_blank()
      )
  ),
  tar_file(voting_intention620, {
    f <- "output/voting_intention620.svg"
    ggsave(
      f,
      plot = voting_intention_chart +
        facet_wrap(
          ~ factor(Party, levels = party_colours$Party),
          scales = "free",
          ncol = 2
        ),
      dpi = 100,
      width = 6.2,
      height = 8
    )
    f
  }),
  tar_target(head_to_head_data, {
    voting_intention_chartdata |>
      pivot_wider(names_from = Party, values_from = c("mean", "q5", "q95")) |>
      mutate(mean_LabourGreenTPM = mean_Labour + mean_Green + `mean_Te Pāti Māori`,
             q5_LabourGreenTPM = q5_Labour + q5_Green + `q5_Te Pāti Māori`,
             q95_LabourGreenTPM = q95_Labour + q95_Green + `q95_Te Pāti Māori`,
             mean_NationalAct = mean_National + mean_ACT,
             q5_NationalAct = q5_National + q5_ACT,
             q95_NationalAct = q95_National + q95_ACT,
             ) |>
      select(week, mean_LabourGreenTPM, q5_LabourGreenTPM, q95_LabourGreenTPM,
             mean_NationalAct, q5_NationalAct, q95_NationalAct) |>
      pivot_longer(
        cols = -week,
        names_to = c(".value", "Party"),
        names_pattern = "(.*)_(.*)"
      ) %>%
      filter(
        Party %in% c("LabourGreenTPM", "NationalAct")
      ) %>%
      mutate(
        Party = case_when(
          Party == "LabourGreenTPM" ~ "Labour + Green + Te Pāti Māori",
          Party == "NationalAct" ~ "National + Act",
          TRUE ~ Party))
  }),
  tar_file(head_to_head_csv, {
    f <- "output/head_to_head.csv"
    write_csv(head_to_head_data, f)
    f
  }),
  tar_target(head_to_head_polls, {
    polls2 |>
      mutate(`Labour + Green + Te Pāti Māori` = Labour + Green + `Te Pāti Māori`,
             `National + Act` = National + ACT) |>
      select(Pollster, MidDate, MidDateNumber, ElectionYear, `Labour + Green + Te Pāti Māori`, `National + Act`)
  }),
  tar_file(head_to_head_plot, {
    f = "output/head_to_head.svg"
    head_to_head_plot <- head_to_head_data |>
      ggplot(aes(
        x = week,
        y = mean,
        colour = Party,
        fill = Party
      )) +
      geom_point(
        data = gather(
          head_to_head_polls,
          Party,
          VotingIntention,-Pollster,-MidDate,-ElectionYear,-MidDateNumber
        ) |>
          filter(VotingIntention != 0),
        aes(x = MidDate, y = VotingIntention),
        colour = "black",
        size = 0.5
      ) +
      geom_vline(
        xintercept = as.numeric(election_dates),
        colour = "grey60"
      ) +
    scale_y_continuous(labels = percent) +
    scale_x_date(breaks = ym(
                   c('2014-01', '2015-01', '2016-01', '2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01', '2023-01', '2024-01')),
                 date_labels = '%Y') +
    scale_fill_manual(values = party_colours$Colour, breaks = party_colours$Party) +
    geom_line(
      data = head_to_head_data |> filter(week <= today())
    ) +
    scale_colour_manual(values = c("#8B0000", "#00008B")) +
    geom_hline(yintercept=0.5, col = 'grey') +
    geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3, colour = NA) +
    labs(y = NULL, x = NULL) +
    theme_clean() +
    theme(
      plot.title.position = "plot",
      legend.position = 'none',
      strip.background = element_rect(fill = '#121617'),
      strip.text = element_text(colour = 'white'),
      plot.background = element_blank()
    )
    ggsave(
      f,
      plot = head_to_head_plot,
      dpi = 100,
      width = 10,
      height = 4
    )
    f
  }),
  tar_file(voting_intention375, {
    f <- "output/voting_intention375.svg"
    ggsave(
      f,
      plot = voting_intention_chart +
        facet_wrap(
          ~ factor(Party, levels = party_colours$Party),
          scales = "free",
          ncol = 1
        ),
      dpi = 100,
      width = 3.75,
      height = 10
    )
    f
  }),
  tar_file(voting_intention_data, {
    f = "output/voting_intention_chartdata.csv"
    write_csv(voting_intention_chartdata, f)
    f
  })
)
