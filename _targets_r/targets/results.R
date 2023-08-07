list(
  tar_target(results2020, fetch_results(2020, 17)),
  tar_target(results2017, fetch_results(2017, 16)),
  tar_target(results2014, fetch_results(2014, 15)),
  tar_target(results2011, fetch_results(2011, 13)),
  tar_target(results2008, fetch_results(2008, 19)),
  tar_target(
    results,
    combine_results(
      results2008,
      results2011,
      results2014,
      results2017,
      results2020
    )
  )
)
