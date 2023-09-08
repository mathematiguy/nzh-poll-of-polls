simulate_seats <- function(sims) {
  t(sapply(1:nrow(sims), function(i) {
    allocate_seats(
      votes = as.numeric(sims[i,]),
      electorate = c(1, 1, 1, 1, 0, 1, 0),
      parties = names(sims)
    )$seats_v
  })) |>
    as_tibble() |>
    mutate(
      NatAct = ACT + National,
      NatActNZF = National + ACT + `NZ First`,
      LabGreen = Labour + Green,
      LabGreenMaori = Labour + Green + `Te Pāti Māori`,
      LabGreenNZF = Labour + Green + `NZ First`
    )
}

list(
  tar_target(
    weekly_mu,
    model2023_mcmc_model2023$draws('mu', format = 'draws_matrix')
  ),
  tar_target(
    sims_election_night, {
      n_parties = 8
      indices <- seq(473, 473 * n_parties, by = 473)
      weekly_mu[, indices] |> as_tibble() |> set_names(parties_ss) |>
        select(all_of(sort(parties_ss))) |>
        select(-Other)
    }),
  tar_target(
    sims_nowcast, {
      n_parties = 8

      # Assume the date of the first election in your data
      first_election_date <- as.Date("2014-10-20")

      # Get the current date
      current_date <- Sys.Date()

      # Calculate the difference in weeks
      weeks_since_first_election <- as.numeric(difftime(current_date, first_election_date, units = "weeks"))
      indices_nowcast <- seq(weeks_since_first_election, weeks_since_first_election + 473 * (n_parties - 1), by = 473)

      weekly_mu[, indices_nowcast] |> as_tibble() |> set_names(parties_ss) |>
        select(all_of(sort(parties_ss))) |>
        select(-Other)
  }),
  tar_target(seats_election_night, simulate_seats(sims_election_night)),
  tar_target(seats_nowcast, simulate_seats(sims_nowcast)),
  tar_target(coalition_odds, {
    seats_election_night |>
      select(Labour, National, ACT, Green, `NZ First`, `Te Pāti Māori`) |>
      summarise(
        NatActNZF = sum((National + ACT + `NZ First` > 60) & (National + ACT <= 60) & (National <= 60)),
        NatAct = sum((National + ACT > 60) & (National <= 60)),
        LabGreen = sum((Labour + Green > 60) & (Labour <= 60)),
        LabGreenNZF = sum((Labour + Green + `NZ First` > 60) & (Labour + Green + `Te Pāti Māori` <= 60) & (Labour + Green <= 60)),
        LabGreenMaori = sum((Labour + Green + `Te Pāti Māori` > 60) & (Labour + Green <= 60)),
        Labour = sum(Labour > 60),
        National = sum(National > 60)
      ) |>
      pivot_longer(everything(), names_to="Coalition", values_to="Sims") |>
      mutate(Prob = Sims / 8000,
             Winner = if_else(str_starts(Coalition, "Lab"), "Labour", "National")) |>
      arrange(Winner, desc(Sims))
  }),
  tar_file(coalition_odds_plot, {
    f <- "output/coalition_odds_plot.svg"

    coalition_odds_plot <- coalition_odds |>
      filter(!(Coalition %in% c("Labour", "National"))) |>
      mutate(
        Percentage = Prob * 100,
        Coalition = gsub("^LabGreen", "Labour, Greens", Coalition),
        Coalition = gsub("^NatAct", "National, ACT", Coalition),
        Coalition = gsub("NZF$", ", NZ First", Coalition),
        Coalition = gsub("Maori", ",\nTe Pāti Māori", Coalition)
      ) |>
      ggplot(aes(x = Coalition, y = Percentage, fill = Winner)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = 0, hjust = -0.4) +
      ggtitle("2023 NZ Election Coalition odds at 27 August") +
      ylim(0, 55) +
      ylab("Percentage odds") +
      coord_flip() +
      scale_fill_manual(values = c('#d82c20', '#065BAA')) +
      theme_clean() +
      theme(
        legend.position = 'none',
        strip.background = element_rect(fill = '#121617'),
        strip.text = element_text(colour = 'white'),
        plot.background = element_blank()
      ) +
      labs(x = NULL)

    ggsave(
      f,
      plot = coalition_odds_plot,
      dpi = 100,
      width = 7.5,
      height = 4
    )
    f
  }),
  tar_file(sims_election_night_csv, {
    f <- "output/sims_election_night.csv"
    write_csv(sims_election_night, f)
    f
  })
)
