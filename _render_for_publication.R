tictoc::tic("Render for publication")

# run spellcheck and styling
source("R/_spellcheck.R")

# export latest plotly edits
source("R/_export_plotly.R")

# build and preview quarto
rstudioapi::terminalExecute("quarto render --cache-refresh --to html")
rstudioapi::terminalExecute("quarto preview")

tictoc::toc()
# Render for publication: 416.258 sec elapsed
