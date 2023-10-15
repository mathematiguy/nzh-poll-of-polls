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
  tar_target(seats_election_night, simulate_seats(sims_election_night)),
  tar_target(coalition_odds, {
    seats_election_night |>
      select(Labour, National, ACT, Green, `NZ First`, `Te Pāti Māori`) |>
      summarise(
        NatAct = sum((National + ACT > 60) & (National <= 60)),
        NatActNZF = sum((National + ACT + `NZ First` > 60) & (National + ACT <= 60) & (Labour + Green + `Te Pāti Māori` + `NZ First` <= 60)),
        # LabGreen = sum((Labour + Green > 60) & (Labour <= 60)),
        LabGreenMaori = sum((Labour + Green + `Te Pāti Māori` > 60) & (Labour + Green <= 60)),
        # LabGreenMaoriNZF = sum((Labour + Green + `Te Pāti Māori` + `NZ First` > 60) & (Labour + Green + `Te Pāti Māori` <= 60) & (National + ACT + `NZ First` <= 60)),
        NZF = sum((National + ACT <= 60) & (Labour + Green + `Te Pāti Māori` <= 60) & (National + ACT + `NZ First` > 60) & (Labour + Green + `Te Pāti Māori` + `NZ First` > 60)),
        NoWinner = sum((National + ACT + `NZ First` <= 60) & (Labour + Green + `Te Pāti Māori` + `NZ First`<= 60)) + sum((Labour + Green + `Te Pāti Māori` + `NZ First` > 60) & (Labour + Green + `Te Pāti Māori` <= 60) & (National + ACT + `NZ First` <= 60)),
        Labour = sum(Labour > 60),
        National = sum(National > 60)
      ) |>
      pivot_longer(everything(), names_to="Coalition", values_to="Sims") |>
      mutate(Prob = Sims / 8000,
             Winner = case_when(
               str_starts(Coalition, "Lab") ~ "Labour",
               str_starts(Coalition, "Nat") ~ "National",
               Coalition == "NatActNZF" ~ "National",
               Coalition == "NZF" ~ "NZ First (Kingmaker)",
               Coalition == "NoWinner" ~ "Hung Parliament"
             )) |>
      arrange(Winner, desc(Sims))
  }),
  tar_target(calibration_data, {

    # -------------------------
    # Common Preparation
    # -------------------------
    # Original Election Results
    results_2023 <- c(
      "ACT" = 9.00,
      "Green" = 10.78,
      "Labour" = 26.85,
      "National" = 38.99,
      "NZ First" = 6.46,
      "Te Pāti Māori" = 2.60,
      "TOP" = 2.07
    ) / 100

    # Convert sims to a tibble of numeric vectors
    sims_numeric <- sims_election_night |>
      lapply(as.numeric) |>
      as_tibble()

    # Calculate column means and standard deviations
    result_means <- colMeans(sims_numeric, na.rm = TRUE)
    result_sds <- apply(sims_numeric, 2, sd, na.rm = TRUE)

    # Standardize the simulation results and the actual results
    standardized_sims <- sims_numeric |>
      sweep(2, result_means, "-") |>
      sweep(2, result_sds, "/") |>
      as_tibble()

    z_scores_actual_results <- (results_2023 - result_means) / result_sds
    actual_results_tibble <- tibble(Value = as.numeric(z_scores_actual_results), Party = names(z_scores_actual_results))

    # -------------------------
    # Branch 1: bar_plot
    # -------------------------
    bar_plot <- function() {
      # Statistical Calculations
      alpha <- 0.05
      corrected_alpha <- alpha / length(z_scores_actual_results)
      critical_z <- qnorm(1 - corrected_alpha / 2)

      # Visualization Settings
      party_colours <- c(
        'Labour' = '#d82c20',
        'National' = '#065BAA',
        'ACT' = '#E6B800',
        'Green' = '#228B22',
        'NZ First' = '#505050',
        'Te Pāti Māori' = '#800000',
        'TOP' = '#99C4D2'
      )

      muted_colours <- sapply(party_colours, function(color) {
        adjustcolor(color, alpha.f = 0.8)
      })

      # Bar plot
      ggplot(actual_results_tibble, aes(x = Party, y = Value)) +
        geom_bar(stat = "identity", aes(fill = Party), show.legend = FALSE) +
        geom_hline(yintercept = c(-critical_z, critical_z), linetype = "dashed", color = "red") +
        labs(
          title = "Significance Testing of Z-Scores per Party",
          subtitle = paste("Red dashed lines represent critical Z-score for alpha =", round(corrected_alpha, 4)),
          y = "Z-Score",
          x = "Party"
        ) +
        theme_clean() +
        coord_flip() +
        scale_fill_manual(values = muted_colours) +
        theme(
          legend.position = 'none',
          strip.background = element_rect(fill = '#121617'),
          strip.text = element_text(colour = 'white'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold")
        )
    }

    # -------------------------
    # Branch 2: hist_plot
    # -------------------------
    hist_plot <- function() {
      # Convert the data frame to long format for ggplot2 plotting
      standardized_sims_long <- standardized_sims |>
        pivot_longer(cols = everything(), names_to = "Party", values_to = "Value")

      # Visualization Settings
      party_colours <- c(
        'Labour' = '#d82c20',
        'National' = '#065BAA',
        'ACT' = '#E6B800',
        'Green' = '#228B22',
        'NZ First' = '#505050',
        'Te Pāti Māori' = '#800000',
        'TOP' = '#99C4D2'
      )

      # Histogram plot
      ggplot(standardized_sims_long, aes(x = Value)) +
        geom_histogram(aes(y = ..density.., fill = Party), binwidth = 0.1, alpha = 0.5, colour = NA) +
        geom_vline(data = actual_results_tibble,
                   aes(xintercept = Value),
                   color = "red",
                   linetype = "dashed", size = 1.2) +
        facet_wrap(~ Party, scales = 'free_y', ncol = 2) +
        scale_fill_manual(values = party_colours) +
        labs(title = "Standardized Simulated Election Results with Actual Z-scores",
             x = "Z-Score",
             y = NULL) +
        theme_clean() +
        theme(
          legend.position = 'none',
          strip.background = element_rect(fill = '#121617'),
          strip.text = element_text(colour = 'white'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold")
        )
    }

    # You can now call each function to display the respective plots.
    bar_plot()
    hist_plot()

 })
  tar_file(coalition_odds_plot, {
    f <- "output/coalition_odds_plot.svg"

    todays_date = format(Sys.Date(), "%d %B")
    coalition_odds_plot <- coalition_odds |>
      filter(!(Coalition %in% c("Labour", "National"))) |>
      mutate(
        Percentage = Prob * 100,
        Coalition = case_when(
          Coalition == "LabGreen" ~ "Labour, Greens",
          Coalition == "NatAct" ~ "National, ACT",
          Coalition == "NatActNZF" ~ "National, ACT, NZ First",
          Coalition == "NZF" ~ "NZ First (Kingmaker)",
          Coalition == "LabGreenMaori" ~ "Labour, Greens, Te Pāti Māori",
          Coalition == "LabGreenMaoriNZF" ~ "Labour, Greens, Te Pāti Māori, NZ First",
          Coalition == "NoWinner" ~ "Hung parliament"
        )) |>
      ggplot(aes(x = Coalition, y = Percentage, fill = Winner)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = 0, hjust = -0.05) +
      ggtitle(paste("2023 NZ Election estimated \nWinning Coalitions at", todays_date)) +
      ylim(0, 100) +
      ylab("Probability") +
      coord_flip() +
      scale_fill_manual(values = c('#ABB0B8', '#D82C20', '#065BAA', '#000000')) +
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
