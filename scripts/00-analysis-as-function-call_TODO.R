

LTBIscreening(
  N.mc = 2,
  screen_age_range = 18:45,
  year_cohort = '2009',
  QALY.ENDPOINT = "exit uk",
  cost.ENDPOINT = "exit uk" ,
  screen_0_to_5_year = TRUE,
  parameter_values_file = system.file("data", "scenario-parameter-values_range-limits.xlsx",
                                      package = "LTBIscreeningproject"),
  diroutput = tempdir(),
  plots_folder = system.file("output", "plots", package = "LTBIscreeningproject")
)
