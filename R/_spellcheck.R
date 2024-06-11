source("R/_load_packages.R")

styler::style_dir() %>%
  suppressMessages()

purrr::map(
  list.files(
    pattern = "*.qmd",
    full.names = T
  ),
  spelling::spell_check_files,
  ignore = readLines("wordlist.txt")
) %>%
  unique()
