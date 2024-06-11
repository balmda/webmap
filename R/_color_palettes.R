
  ssrc_pal <-
    list(
      # Standard SSRC colors
      "ssrc_blue" <- "#002060", 
      "ssrc_magenta" <- "#B626B5", 
      
      # Others
      # Blues
      "medblue" = "#0054A4",
      "liblue" = "#1887F0",
      "dablue" = "#002D57",
      "peerblue" = "#ADBFCC",

      # Browns
      "medbrown" = "#A37731",
      "librown" = "#F09400",
      "dabrown" = "#573600",

      # Reds
      "medred" = "#A33A2C",
      "lired" = "#D64C3A",
      "dared" = "#573631",

      # Greens
      "medgreen" = "#57A22F",
      "ligreen" = "#B0F08D",
      "dagreen" = "#3C7021", 
      
      # MnDOT colors from PDF
      "mn_light_blue" = "#6A9AD0", # 106	154	208	
      "mn_dark_blue" = "#263861", # 38	56	97	
      "mn_green" = "#7EAB55", # 95	128	64	
      "mn_orange" = "#B86029", # 184	96	41	
      "mn_gold" = "#B89230" # 184	146	48	
    )

  cli::cli_inform(
    c("v" = "loaded standard colors\n")
  )
