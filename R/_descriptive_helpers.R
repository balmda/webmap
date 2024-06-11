source("R/_color_palettes.R")


table_info <- function(df) {
  data.frame(
    col_name = colnames(df),
    col_index = 1:ncol(df),
    col_class = sapply(df, class),
    row.names = NULL
  )
}


# Function to summarize mode-specific crashes by one other categorical variable
# WITHOUT SEGMENT DETAILS
ka_mode_summary <- function(x, y, a, b) {
  # x = mode specific MSI variable (e.g., ped_msi_txt)
  # y = other analysis variable
  # a = label to give the mode in caption
  # b = label to give the other variable in caption
  automated_crashes %>% 
    filter(
      valid==TRUE,
      study_per_5yrs==TRUE,
      hwy_flag==FALSE,
      !is.na({{x}}),
      geo_type=="Urban"
    ) %>%
    mutate(
      mode_ka = case_when(
        {{x}} %in% c("Fatal", "Injury (Severe)") ~ "mode_ka",
        TRUE ~ "mode_bco"
      )
    ) %>%
    group_by({{y}}, mode_ka) %>% 
    summarise(n = n()) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = n
    ) %>%
    ungroup() %>%
    mutate(
      tot = mode_ka + mode_bco,
      pct_of_ka = mode_ka / sum(mode_ka),
      pct_of_tot = (mode_ka + mode_bco) / sum(mode_ka + mode_bco)
    ) %>%
    adorn_totals("row") %>%
    mutate(
      pct_resulting_ka = mode_ka / tot
    ) %>%
    mutate(
      across(c(mode_bco, mode_ka, tot), 
             function(x) scales::number(x, big.mark=",", accuracy=1)),
      across(c(pct_of_ka, pct_of_tot, pct_resulting_ka),
             function(x) scales::percent(x, accuracy=0.1))
    ) %>%
    select(
      seg_stress_max,
      mode_ka, 
      tot,
      pct_of_ka,
      pct_of_tot,
      pct_resulting_ka
    ) %>%
    kable(
      col.names = c(
        "Segment Stress",
        "Severe Crashes",
        "Total Crashes",
        "% of Severe Crashes",
        "% of All Crashes",
        "% of Crashes That Are Severe"
      ),
      align = "lrrccc"
    ) %>%
    kable_styling()
}


# Function to summarize a categorical variable by mode
# at both crash and segment level
univariate_mode_summary_segments <- function(
    mode_var, # E.g., ped_msi_txt
    mode_epdo, # E.g., ped_epdo
    other_var_crash, # E.g., seg_stress_max (variable name in crash table)
    other_var_seg, # E.g., seg_stress_max (variable name in segment table)
    ### ASSUMES other_var_crash AND other_var_seg ARE CODED IDENTICALLY!!!
    mode_label, # E.g., Pedestrian
    other_label # E.g., Segment Stress
) {
  
  seg_var_summary <- 
    automated_segments %>%
    filter(fc_valid == TRUE,
           fclass != 'Path',
           fclass != 'Highway') %>% 
    group_by(geo_type, {{other_var_seg}}) %>%
    summarise(mileage=sum(length_mi)) %>% 
    mutate(
      pct_mileage = mileage / sum(mileage),
      mileage = round(mileage,1)
    ) 
  
  mode_var_urban <-
    automated_crashes %>%
    filter(
      valid==TRUE,
      study_per_5yrs==TRUE,      
      hwy_flag==FALSE,
      geo_type=="Urban",
      location_type=="Midblock",
      !is.na({{mode_var}})
    ) %>%
    mutate(
      mode_ka = case_when(
        {{mode_var}} %in% c("Fatal", "Injury (Severe)") ~ "mode_ka",
        TRUE ~ "mode_bco"
      )
    ) %>%
    group_by(mode_ka, geo_type, {{other_var_crash}}) %>% 
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco
    ) %>%
    mutate(
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) %>%
    data.frame()
  
  mode_var_urban_join <-
    left_join(
      mode_var_urban, 
      seg_var_summary, 
      join_by(
        geo_type,
        {{other_var_crash}} == {{other_var_seg}}
      )
    ) %>%
    adorn_totals(
      name = "Urban Subtotal"
    ) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_mile = tot_crash / mileage,
      ka_per_mile = n_mode_ka / mileage
    ) 
  
  
  mode_var_rural <-
    automated_crashes %>%
    filter(
      valid==TRUE,
      study_per_5yrs==TRUE,      
      hwy_flag==FALSE,
      geo_type=="Rural",
      location_type=="Midblock",
      !is.na({{mode_var}})
    ) %>%
    mutate(
      mode_ka = case_when(
        {{mode_var}} %in% c("Fatal", "Injury (Severe)") ~ "mode_ka",
        TRUE ~ "mode_bco"
      )
    ) %>%
    group_by(mode_ka, geo_type, {{other_var_crash}}) %>% 
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco
    ) %>%
    mutate(
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) %>%
    data.frame()
  
  mode_var_rural_join <-
    left_join(
      mode_var_rural, 
      seg_var_summary, 
      join_by(
        geo_type,
        {{other_var_crash}} == {{other_var_seg}}
      )
    ) %>%
    adorn_totals(
      name = "Rural Subtotal"
    ) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_mile = tot_crash / mileage,
      ka_per_mile = n_mode_ka / mileage
    ) 
  
  mode_var_all <-
    rbind(
      mode_var_urban_join,
      mode_var_rural_join
    ) %>%
    # adorn_totals("row") %>%
    mutate(
      # Ugh, have to calculate total rows carefully
      tot_crash = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$tot_crash[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$tot_crash[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ tot_crash
      ),
      pct_tot = case_when(
        geo_type=="Total" ~ NA,
        TRUE ~ pct_tot
      ),
      n_mode_ka = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$n_mode_ka[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$n_mode_ka[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ n_mode_ka
      ),
      pct_ka = case_when(
        geo_type=="Total" ~ NA,
        TRUE ~ pct_ka
      ),
      pct_epdo = case_when(
        geo_type=="Total" ~ NA,
        TRUE ~ pct_epdo
      ),
      tot_epdo = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$tot_epdo[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$tot_epdo[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ tot_epdo
      ),
      mileage = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$mileage[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$mileage[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ mileage
      ),
    ) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_mile = tot_crash / mileage,
      ka_per_mile = n_mode_ka / mileage
    ) %>%
    select(
      geo_type, 
      {{other_var_crash}}, 
      tot_crash, pct_tot, 
      n_mode_ka, pct_ka, pct_severe, 
      pct_epdo, avg_epdo, 
      crash_per_mile, ka_per_mile, 
      mileage, pct_mileage
    ) %>% 
    mutate(
      across(
        c(tot_crash, n_mode_ka, mileage),
        function(x) scales::number(x, accuracy = 1, big.mark = ",")),
      avg_epdo = number(avg_epdo, accuracy = 0.1, big.mark = ","),
      across(
        c(pct_tot, pct_ka, pct_severe, pct_epdo, pct_mileage),
        function(x) scales::percent(x, accuracy = 0.1)),
      across(
        c(crash_per_mile, ka_per_mile),
        function(x) scales::number(100*x, accuracy=0.1, big.mark=","))
    )
  
  mode_var_all %>%
    kable(
      col.names = c(
        "Geography Type",
        {{other_label}}, 
        "Total Crashes", 
        "% of Total Crashes (Within Geo)", 
        "Severe Crashes", 
        "% of Severe Crashes (Within Geo)",
        "% of Crashes That Are Severe",
        "% of EPDO (Within Geo)",
        "Avgerage EPDO",
        "Total Crashes per 100 Miles",
        "Severe Crashes per 100 Miles",
        "Approximate Mileage",
        "% of Mileage (Within Geo)"
      ),
      align="llrcrcccrrrrc",
      table.attr = "style = \"color: black;\""
    ) %>%
    kable_styling() %>% 
    row_spec(which(mode_var_all$geo_type %in% c('Urban Subtotal', 'Rural Subtotal', 'Total')), 
             bold = T, color = "black", background = '#EEEEEE', italic = TRUE) %>%
    row_spec(0, bold = T, color = "white", background = ssrc_blue) 
}




univariate_mode_summary_intersections <- function(
    mode_var, # E.g., ped_msi_txt
    mode_epdo, # E.g., ped_epdo
    other_var_crash, # E.g., seg_stress_max (variable name in crash table)
    other_var_int, # E.g., seg_stress_max (variable name in intersection table)
    ### ASSUMES other_var_crash AND other_var_seg ARE CODED IDENTICALLY!!!
    mode_label, # E.g., Pedestrian
    other_label # E.g., Segment Stress
) {
  
  int_var_summary <- 
    automated_intersections %>%
    filter(valid == TRUE) %>% 
    group_by(geo_type, {{other_var_int}}) %>%
    summarise(n_int=n()) %>%
    mutate(pct_int = n_int / sum(n_int))
  
  mode_var_urban <-
    automated_crashes %>%
    filter(
      valid==TRUE,
      study_per_5yrs==TRUE,      
      geo_type=="Urban",
      location_type=="Intersection",
      !is.na({{mode_var}})
    ) %>%
    mutate(
      mode_ka = case_when(
        {{mode_var}} %in% c("Fatal", "Injury (Severe)") ~ "mode_ka",
        TRUE ~ "mode_bco"
      )
    ) %>%
    group_by(mode_ka, geo_type, {{other_var_crash}}) %>% 
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco
    ) %>%
    mutate(
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) %>%
    data.frame()
  
  mode_var_urban_join <-
    left_join(
      mode_var_urban, 
      int_var_summary, 
      join_by(
        geo_type,
        {{other_var_crash}} == {{other_var_int}}
      )
    ) %>%
    adorn_totals(
      name = "Urban Subtotal"
    ) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_int = tot_crash / n_int,
      ka_per_int = n_mode_ka / n_int
    ) 
  
  
  mode_var_rural <-
    automated_crashes %>%
    filter(
      valid==TRUE,
      study_per_5yrs==TRUE,      
      geo_type=="Rural",
      location_type=="Intersection",
      !is.na({{mode_var}})
    ) %>%
    mutate(
      mode_ka = case_when(
        {{mode_var}} %in% c("Fatal", "Injury (Severe)") ~ "mode_ka",
        TRUE ~ "mode_bco"
      )
    ) %>%
    group_by(mode_ka, geo_type, {{other_var_crash}}) %>% 
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco
    ) %>%
    mutate(
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) %>%
    data.frame()
  
  mode_var_rural_join <-
    left_join(
      mode_var_rural, 
      int_var_summary, 
      join_by(
        geo_type,
        {{other_var_crash}} == {{other_var_int}}
      )
    ) %>%
    adorn_totals(
      name = "Rural Subtotal"
    ) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_int = tot_crash / n_int,
      ka_per_int = n_mode_ka / n_int
    ) 
  
  mode_var_all <-
    rbind(
      mode_var_urban_join,
      mode_var_rural_join
    ) %>%
    # adorn_totals("row") %>%
    mutate(
      # Ugh, have to calculate total rows carefully
      tot_crash = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$tot_crash[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$tot_crash[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ tot_crash
      ),
      pct_tot = case_when(
        geo_type=="Total" ~ NA,
        TRUE ~ pct_tot
      ),
      n_mode_ka = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$n_mode_ka[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$n_mode_ka[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ n_mode_ka
      ),
      pct_ka = case_when(
        geo_type=="Total" ~ NA,
        TRUE ~ pct_ka
      ),
      pct_epdo = case_when(
        geo_type=="Total" ~ NA,
        TRUE ~ pct_epdo
      ),
      tot_epdo = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$tot_epdo[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$tot_epdo[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ tot_epdo
      ),
      n_int = case_when(
        geo_type=="Total" ~ 
          mode_var_urban_join$n_int[mode_var_urban_join$geo_type=="Urban Subtotal"] +
          mode_var_rural_join$n_int[mode_var_rural_join$geo_type=="Rural Subtotal"],
        TRUE ~ n_int
      ),
    ) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_int = tot_crash / n_int,
      ka_per_int = n_mode_ka / n_int
    ) %>%
    select(
      geo_type, 
      {{other_var_crash}}, 
      tot_crash, pct_tot, 
      n_mode_ka, pct_ka, pct_severe, 
      pct_epdo, avg_epdo, 
      crash_per_int, ka_per_int, 
      n_int, pct_int
    ) %>% 
    mutate(
      across(
        c(tot_crash, n_mode_ka, n_int),
        function(x) scales::number(x, accuracy = 1, big.mark = ",")),
      avg_epdo = number(avg_epdo, accuracy = 0.1, big.mark = ","),
      across(
        c(pct_tot, pct_ka, pct_severe, pct_epdo, pct_int),
        function(x) scales::percent(x, accuracy = 0.1)),
      across(
        c(crash_per_int, ka_per_int),
        function(x) scales::number(1000 * x, accuracy=0.1, big.mark=","))
    )
  
  mode_var_all %>%
    kable(
      col.names = c(
        "Geography Type",
        {{other_label}}, 
        "Total Crashes", 
        "% of Total Crashes (Within Geo)", 
        "Severe Crashes", 
        "% of Severe Crashes (Within Geo)",
        "% of Crashes That Are Severe",
        "% of EPDO (Within Geo)",
        "Avgerage EPDO",
        "Total Crashes per 1,000 Intersections",
        "Severe Crashes per 1,000 Intersections",
        "Approximate # of Intersections",
        "% of Intersections (Within Geo)"
      ),
      align="llrcrcccrrrrc",
      table.attr = "style = \"color: black;\""
    ) %>%
    kable_styling() %>% 
    row_spec(which(mode_var_all$geo_type %in% c('Urban Subtotal', 'Rural Subtotal', 'Total')), 
             bold = T, color = "black", background = '#EEEEEE', italic = TRUE) %>%
    row_spec(0, bold = T, color = "white", background = ssrc_blue) 
}



mode_summary_intersections_flex <- function(
    crash_df,
    int_df,
    mode_inj_var,
    mode_epdo, # E.g., ped_epdo
    other_var_crash, # E.g., seg_stress_max (variable name in crash table)
    other_var_int, # E.g., seg_stress_max (variable name in intersection table)
    ### ASSUMES other_var_crash AND other_var_seg ARE CODED IDENTICALLY!!!
    mode_label, # E.g., Pedestrian
    other_label # E.g., Segment Stress,
) {
  
  int_var_summary <-
    int_df %>%
    group_by({{other_var_int}}) %>%
    summarise(n_int=n())
  
  crash_df_summary <-
    crash_df %>%
    mutate(
      mode_ka = case_when(
        {{mode_inj_var}} %in% c("Fatal", "Injury (Severe)") ~ "mode_ka",
        TRUE ~ "mode_bco"
      )) %>%
    group_by(mode_ka, {{other_var_crash}}) %>%
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco,
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) %>%
    data.frame()
  
  mode_var_all <-
    left_join(
      crash_df_summary, int_var_summary,
      join_by({{other_var_crash}} == {{other_var_int}})
    ) %>%
    adorn_totals(name = "Total") %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_int = tot_crash / n_int,
      ka_per_int = n_mode_ka / n_int
    ) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_int = tot_crash / n_int,
      ka_per_int = n_mode_ka / n_int
    ) %>%
    select(
      {{other_var_crash}},
      tot_crash, pct_tot,
      n_mode_ka, pct_ka, pct_severe,
      pct_epdo, avg_epdo,
      crash_per_int, ka_per_int,
      n_int
    ) %>%
    mutate(
      across(
        c(tot_crash, n_mode_ka, n_int),
        function(x) scales::number(x, accuracy = 1, big.mark = ",")),
      avg_epdo = number(avg_epdo, accuracy = 0.1, big.mark = ","),
      across(
        c(pct_tot, pct_ka, pct_severe, pct_epdo),
        function(x) scales::percent(x, accuracy = 0.1)),
      across(
        c(crash_per_int, ka_per_int),
        function(x) scales::number(100 * x, accuracy=0.1, big.mark=","))
    )
  
  mode_var_all %>%
    kable(
      col.names = c(
        {{other_label}},
        "Total Crashes",
        "% of Total Crashes",
        "Severe Crashes",
        "% of Severe Crashes",
        "% of Crashes That Are Severe",
        "% of EPDO",
        "Avgerage EPDO",
        "Total Crashes per 1,000 Intersections",
        "Severe Crashes per 1,000 Intersections",
        "Approximate # of Intersections"
      ),
      align="llrcrcccrrrr",
      table.attr = "style = \"color: black;\""
    ) %>%
    kable_styling() %>%
    row_spec(0, bold = T, color = "white", background = ssrc_blue)
}



mode_summary_network_univariate <- function(
    crash_df, 
    network_df, 
    network_multiplier = 1,
    network_unit,
    network_point = FALSE,
    network_length_field,
    mode_inj_var,
    mode_ksi_list = c("Fatal", "Injury (Severe)"),
    mode_epdo,
    other_var_crash,
    other_var_network, 
    mode_label='xxx', 
    other_label, 
    formatted = TRUE, 
    total_name = 'Total') {
  
  if (network_point == FALSE) {
    network_var_summary <- 
      network_df %>%
      group_by({{other_var_network}}) %>%
      summarise(network_n = round(sum({{network_length_field}}),1)) %>%
      mutate(network_pct = network_n / sum(network_n))
    
  } else {
    network_var_summary <- 
      network_df %>%
      group_by({{other_var_network}}) %>%
      summarise(network_n = n()) %>%
      mutate(network_pct = network_n / sum(network_n))
  }
  
  crash_df_summary <-
    crash_df %>%
    mutate(
      mode_ka = case_when(
        {{mode_inj_var}} %in% {{mode_ksi_list}} ~ "mode_ka",
        TRUE ~ "mode_bco"
      )) %>%
    group_by(mode_ka, {{other_var_crash}}) %>% 
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco,
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) %>%
    data.frame()
  
  mode_var_all <-
    left_join(
      crash_df_summary, network_var_summary,
      join_by({{other_var_crash}} == {{other_var_network}})
    ) %>%
    adorn_totals(name = {{total_name}}) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash,
      crash_per_unit = tot_crash / network_n,
      ka_per_unit = n_mode_ka / network_n
    ) %>% 
    select(
      {{other_var_crash}},
      tot_crash, pct_tot,
      n_mode_ka, pct_ka, pct_severe,
      pct_epdo, avg_epdo,
      crash_per_unit, ka_per_unit,
      network_n, network_pct
    ) 
  
  if (formatted == TRUE) {
    mode_var_all %>%
      mutate(
        across(
          c(tot_crash, n_mode_ka, network_n),
          function(x) scales::number(x, accuracy = 1, big.mark = ",")),
        avg_epdo = number(avg_epdo, accuracy = 0.1, big.mark = ","),
        across(
          c(pct_tot, pct_ka, pct_severe, pct_epdo, network_pct),
          function(x) scales::percent(x, accuracy = 0.1)),
        across(
          c(crash_per_unit, ka_per_unit),
          function(x) scales::number(x * {{network_multiplier}}, accuracy=0.1, big.mark=","))
      ) %>% 
      kable(
        col.names = c(
          {{other_label}},
          "Total Crashes",
          "% of Total Crashes",
          "# of Severe Crashes",
          "% of Severe Crashes",
          "% of Crashes That Are Severe",
          "% of EPDO",
          "Average EPDO",
          paste0("Total Crashes per ", {{network_multiplier}}, " ", {{network_unit}}),
          paste0("Severe Crashes per ", {{network_multiplier}}, " ", {{network_unit}}),
          paste0("Approximate # of ", {{network_unit}}),
          paste0("Approximate % of ", {{network_unit}})
        ),
        align="lrcrcccrrrrc",
        table.attr = "style = \"color: black;\""
      ) %>%
      kable_styling() %>%
      row_spec(0, bold = T, color = "white", background = ssrc_blue)
  } else {
    mode_var_all %>%
      data.frame()
  }
}

crash_mode_summary_var1 <- function(
    crash_df, 
    mode_inj_var,
    mode_ksi_list = c("Fatal", "Injury (Severe)"),
    mode_epdo,
    var1,
    var1_label,
    mode_label='', 
    formatted = TRUE, 
    sort_by = 'n_mode_ka',
    total_name = 'Total') {
  
  crash_df_summary <-
    crash_df %>%
    mutate(
      mode_ka = case_when(
        {{mode_inj_var}} %in% {{mode_ksi_list}} ~ "mode_ka",
        TRUE ~ "mode_bco"
      )) %>%
    group_by(mode_ka, {{var1}}) %>% 
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco,
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) 
  
  if (sort_by == 'n_mode_ka') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange(desc(n_mode_ka))
  } else if (sort_by == 'tot_crash') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange(desc(tot_crash))
  } else if (sort_by == 'var1') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange({{var1}})
  } else {
    crash_df_summary %>%
      data.frame()
  }
  
crash_df_summary <- 
  crash_df_summary %>% 
    adorn_totals(name = {{total_name}}) %>%
    mutate(
      pct_severe = n_mode_ka / tot_crash,
      avg_epdo = tot_epdo / tot_crash
    ) %>% 
    select(
      {{var1}},
      tot_crash, pct_tot,
      n_mode_ka, pct_ka, pct_severe,
      pct_epdo, avg_epdo
    ) %>%
    data.frame()
  
  if (formatted == TRUE) {
    crash_df_summary %>%
      mutate(
        across(
          c(tot_crash, n_mode_ka),
          function(x) scales::number(x, accuracy = 1, big.mark = ",")),
        avg_epdo = number(avg_epdo, accuracy = 0.1, big.mark = ","),
        across(
          c(pct_tot, pct_ka, pct_severe, pct_epdo),
          function(x) scales::percent(x, accuracy = 0.1))
      ) %>% 
      kable(
        col.names = c(
          {{var1_label}},
          "Total Crashes",
          "% of Total Crashes",
          "# of Severe Crashes",
          "% of Severe Crashes",
          "% of Crashes That Are Severe",
          "% of EPDO",
          "Average EPDO"
        ),
        align="lrcrcccrrrrc",
        table.attr = "style = \"color: black;\""
      ) %>%
      kable_styling() %>%
      row_spec(0, bold = T, color = "white", background = ssrc_blue)
  } else {
    crash_df_summary %>%
      data.frame()
  }
}


crash_mode_summary_var2 <- function(
    crash_df, 
    mode_inj_var,
    mode_ksi_list = c("Fatal", "Injury (Severe)"),
    mode_epdo,
    var1,
    var1_label,
    var2,
    var2_label,
    num_vars = 2,
    mode_label, 
    formatted = TRUE, 
    sort_by = 'n_mode_ka',
    total_name = 'Total') {
  
  crash_df_summary <-
    crash_df %>%
    mutate(
      mode_ka = case_when(
        {{mode_inj_var}} %in% {{mode_ksi_list}} ~ "mode_ka",
        TRUE ~ "mode_bco"
      )) %>%
    group_by(mode_ka, {{var1}}, {{var2}}) %>% 
    summarise(
      n = n(),
      epdo = sum({{mode_epdo}})
    ) %>%
    pivot_wider(
      names_from = mode_ka,
      values_from = c(n, epdo)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(n_mode_bco, n_mode_ka, epdo_mode_bco, epdo_mode_ka),
        function(x) replace_na(x, 0))
    ) %>%
    mutate(
      tot_crash = n_mode_ka + n_mode_bco,
      tot_epdo = epdo_mode_ka + epdo_mode_bco,
      pct_tot = tot_crash / sum(tot_crash),
      pct_ka = n_mode_ka / sum(n_mode_ka),
      pct_epdo = tot_epdo / sum(tot_epdo)
    ) 
  
  if (sort_by == 'n_mode_ka') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange(desc(n_mode_ka))
  } else if (sort_by == 'tot_crash') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange(desc(tot_crash))
  } else if (sort_by == 'var1') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange({{var1}})
  } else if (sort_by == 'var2') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange({{var2}})
  } else if (sort_by == 'both') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange({{var1}},{{var2}})
  } else if (sort_by == 'var1_tot') {
    crash_df_summary <- 
      crash_df_summary %>%
      arrange({{var1}},desc(tot_crash))
  } else {
    crash_df_summary %>%
      data.frame()
  }
  
  
crash_df_summary <- 
  crash_df_summary %>% 
  adorn_totals(name = {{total_name}}) %>%
  mutate(
    pct_severe = n_mode_ka / tot_crash,
    avg_epdo = tot_epdo / tot_crash
  ) %>% 
  select(
    {{var1}},{{var2}},
    tot_crash, pct_tot,
    n_mode_ka, pct_ka, pct_severe,
    pct_epdo, avg_epdo
  ) %>%
  data.frame()
  
  if (formatted == TRUE) {
    crash_df_summary %>%
      mutate(
        across(
          c(tot_crash, n_mode_ka),
          function(x) scales::number(x, accuracy = 1, big.mark = ",")),
        avg_epdo = number(avg_epdo, accuracy = 0.1, big.mark = ","),
        across(
          c(pct_tot, pct_ka, pct_severe, pct_epdo),
          function(x) scales::percent(x, accuracy = 0.1))
      ) %>% 
      kable(
        col.names = c(
          {{var1_label}},
          {{var2_label}},
          "Total Crashes",
          "% of Total Crashes",
          "# of Severe Crashes",
          "% of Severe Crashes",
          "% of Crashes That Are Severe",
          "% of EPDO",
          "Average EPDO"
        ),
        align="lrcrcccrrrrc",
        table.attr = "style = \"color: black;\""
      ) %>%
      kable_styling() %>%
      row_spec(0, bold = T, color = "white", background = ssrc_blue)
  } else {
    crash_df_summary %>%
      data.frame()
  }
}


hex_summary_equity <- function(
    hex_df, 
    mode_tot,
    mode_ka,
    mode_sfn_col,
    mode_label, 
    multiplier = 100, 
    per_cap_multiplier = 100000,
    total_name = 'Total', 
    stacking = FALSE) {
  
  hex_df_summary <-
    hex_df %>%
    group_by(dis_adv_com) %>% 
    summarise(
      n_hex = n(), 
      mode_ka = sum({{mode_ka}}), 
      mode_tot = sum({{mode_tot}}), 
      sfn_mileage_all = sum(sfn_mileage_all),
      sfn_mileage_mode = sum({{mode_sfn_col}}), 
      tot_pop = sum(pop_total)
    ) %>% 
    mutate(
      mode = {{mode_label}},
      across(
        c(mode_ka, mode_tot,
          sfn_mileage_all, 
          sfn_mileage_mode, 
          tot_pop),
        function(x) replace_na(x, 0))
    ) %>% 
    ungroup() %>%
    mutate(
      pct_hex = n_hex / sum(n_hex),
      pct_mode_ka = mode_ka / sum(mode_ka),
      pct_mode_tot = mode_tot / sum(mode_tot),
      pct_sfn_mileage_all = sfn_mileage_all / sum(sfn_mileage_all),
      pct_sfn_mileage_mode= sfn_mileage_mode / sum(sfn_mileage_mode), 
      pct_pop = tot_pop / sum(tot_pop)
    ) %>% 
    arrange(dis_adv_com) %>% 
    adorn_totals(name = {{total_name}}) %>% 
    mutate(
      mode_ka_per_x_hex = mode_ka / n_hex * {{multiplier}}, 
      mode_tot_per_x_hex = mode_tot / n_hex * {{multiplier}}, 
      mode_sfn_per_x_hex = sfn_mileage_mode / n_hex * {{multiplier}}, 
      tot_pop_per_x_hex = tot_pop / n_hex * {{multiplier}}, 
      mode_ka_per_cap = mode_ka / tot_pop * {{per_cap_multiplier}}, 
      mode_tot_per_cap = mode_tot / tot_pop * {{per_cap_multiplier}}, 
      mode_sfn_per_cap = sfn_mileage_mode / tot_pop * {{per_cap_multiplier}}
    ) %>% 
    data.frame()
  
  #----------- KA Ratio
  hex_df_summary$mode_ka_ratio <- 
    (hex_df_summary %>% filter(dis_adv_com %in% c('Yes')))$mode_ka_per_x_hex / 
    (hex_df_summary %>% filter(dis_adv_com %in% c('No')))$mode_ka_per_x_hex
  
  #----------- Tot Crash Ratio
  hex_df_summary$mode_tot_ratio <- 
    (hex_df_summary %>% filter(dis_adv_com %in% c('Yes')))$mode_tot_per_x_hex / 
    (hex_df_summary %>% filter(dis_adv_com %in% c('No')))$mode_tot_per_x_hex
  
  #----------- SFN Ratio
  hex_df_summary$mode_sfn_ratio <- 
    (hex_df_summary %>% filter(dis_adv_com %in% c('Yes')))$mode_sfn_per_x_hex / 
    (hex_df_summary %>% filter(dis_adv_com %in% c('No')))$mode_sfn_per_x_hex
  
  #----------- KA per Cap Ratio
  hex_df_summary$mode_ka_cap_ratio <- 
    (hex_df_summary %>% filter(dis_adv_com %in% c('Yes')))$mode_ka_per_cap / 
    (hex_df_summary %>% filter(dis_adv_com %in% c('No')))$mode_ka_per_cap
  
  #----------- SFN Ratio
  hex_df_summary$mode_sfn_cap_ratio <- 
    (hex_df_summary %>% filter(dis_adv_com %in% c('Yes')))$mode_sfn_per_cap / 
    (hex_df_summary %>% filter(dis_adv_com %in% c('No')))$mode_sfn_per_cap
  
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_sfn_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_ka_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_tot_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_ka_cap_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_sfn_cap_ratio"] <- NA
  
  
  hex_df_summary_pre_fmt <- 
    hex_df_summary %>% 
    mutate(
      across(
        c(pct_hex,
          pct_mode_ka, pct_mode_tot,
          pct_sfn_mileage_all,
          pct_sfn_mileage_mode, 
          pct_pop),
        function(x) percent(x, accuracy=0.1)),
      across(
        c(n_hex,
          mode_ka, mode_tot,
          sfn_mileage_all,
          sfn_mileage_mode, 
          tot_pop),
        function(x) number(x, accuracy=1, big.mark = ",")), 
      across(
        c(mode_ka_per_x_hex, mode_tot_per_x_hex, 
          mode_ka_ratio, mode_tot_ratio, 
          mode_sfn_ratio, mode_sfn_per_x_hex, 
          mode_ka_per_cap, mode_tot_per_cap, mode_sfn_per_cap, 
          mode_ka_cap_ratio, mode_sfn_cap_ratio),
        function(x) format(round(as.numeric(x), 2), nsmall=2, big.mark=","))
    ) 
  
  if (stacking == TRUE) {
    hex_df_summary_stack <- 
      hex_df_summary_pre_fmt %>% 
      select(
        mode,
        dis_adv_com, 
        pct_hex,
        mode_tot, pct_mode_tot, 
        mode_ka, pct_mode_ka, 
        sfn_mileage_mode, pct_sfn_mileage_mode, mode_sfn_per_x_hex, mode_sfn_ratio, 
        mode_tot_per_x_hex, mode_tot_ratio, 
        mode_ka_per_x_hex, mode_ka_ratio, 
        mode_ka_per_cap, mode_tot_per_cap, mode_sfn_per_cap, 
        tot_pop, pct_pop, 
        mode_ka_cap_ratio, mode_sfn_cap_ratio
      ) %>% 
      data.frame() 
    
    return(hex_df_summary_stack)
    
  } else { 
    hex_df_summary_fmt <-
      hex_df_summary_pre_fmt %>% 
      select(
        mode,
        dis_adv_com, 
        pct_hex,
        tot_pop, 
        pct_pop,
        mode_tot, 
        pct_mode_tot, 
        mode_ka, 
        pct_mode_ka, 
        sfn_mileage_mode, 
        pct_sfn_mileage_mode, 
        mode_sfn_per_x_hex,
        mode_sfn_ratio,
        mode_tot_per_x_hex, 
        mode_tot_ratio, 
        mode_ka_per_x_hex, 
        mode_ka_ratio, 
        mode_tot_per_cap, 
        mode_ka_per_cap, 
        mode_ka_cap_ratio,
        mode_sfn_per_cap, 
        mode_sfn_cap_ratio
      ) %>% 
      rename(# Not updating the column rename for this section. Not being used right now.
        `Mode` = mode,
        `Total Population` = tot_pop, 
        `% Population` = pct_pop,
        `Disadvantaged Community` = dis_adv_com,
        `% of Hex Cells` = pct_hex,
        `# of Crashes` = mode_tot,
        `% of Crashes` = pct_mode_tot,
        `# of KA Crashes` = mode_ka,
        `% of KA Crashes` = pct_mode_ka,
        `# KA Crashes per 100 Hex Cells` = mode_ka_per_x_hex,
        `# Crashes per 100 Hex Cells` = mode_tot_per_x_hex,
        `KA Crash Ratio (Equity Area : Non-Equity Area)` = mode_ka_ratio,
        `Crash Ratio (Equity Area : Non-Equity Area)` = mode_tot_ratio,
        `SFN Mileage per 100 Hex Ratio (Equity Area : Non-Equity Area)` = mode_sfn_ratio,
        `# of Modael SFN` = sfn_mileage_mode,
        `% of Modael SFN` = pct_sfn_mileage_mode, 
        `# Miles of Modal SFN per 100 Hex Cells` = mode_sfn_per_x_hex,
        `KA Crashes per 100K residents` = mode_ka_per_cap, 
        `Crashes per 100K residents` = mode_tot_per_cap, 
        `SFM Miles per 100K residents` = mode_sfn_per_cap
      ) %>%
      kable(
        align= 'lrrrrrrrrrcrcrrr',# Not updating. Not being used right now.
        table.attr = "style = \"color: black;\""
      ) %>%
      collapse_rows(columns = c(1, 12, 14, 16)) %>% # Not updating. Not being used right now.
      kable_styling() %>% 
      row_spec(0, bold = T, color = "white", background = ssrc_blue) %>% 
      row_spec(which(hex_df_summary_pre_fmt$dis_adv_com == total_name), bold = T, color = "black", background = '#EEEEEE', italic = TRUE) %>%
      
      
      return(hex_df_summary_fmt)
  }
}



hex_summary_equity_geo <- function(
    hex_df, 
    mode_tot,
    mode_ka,
    mode_sfn_col,
    mode_label, 
    var, 
    var_label,
    multiplier = 100, 
    per_cap_multiplier = 100000,
    total_name = 'Total', 
    stacking = FALSE) {
  
  hex_df_summary <-
    hex_df %>%
    group_by(dis_adv_com, {{var}}) %>% 
    summarise(
      n_hex = n(), 
      mode_ka = sum({{mode_ka}}), 
      mode_tot = sum({{mode_tot}}), 
      sfn_mileage_all = sum(sfn_mileage_all),
      sfn_mileage_mode = sum({{mode_sfn_col}}), 
      tot_pop = sum(pop_total)
    ) %>% 
    mutate(
      mode = {{mode_label}},
      across(
        c(mode_ka, mode_tot,
          sfn_mileage_all, 
          sfn_mileage_mode, 
          tot_pop),
        function(x) replace_na(x, 0))
    ) %>% 
    ungroup() %>%
    mutate(
      pct_hex = n_hex / sum(n_hex),
      pct_mode_ka = mode_ka / sum(mode_ka),
      pct_mode_tot = mode_tot / sum(mode_tot),
      pct_sfn_mileage_all = sfn_mileage_all / sum(sfn_mileage_all),
      pct_sfn_mileage_mode= sfn_mileage_mode / sum(sfn_mileage_mode), 
      pct_pop = tot_pop / sum(tot_pop)
    ) %>% 
    arrange({{var}}, dis_adv_com) %>% 
    adorn_totals(name = {{total_name}}) %>% 
    mutate(
      mode_ka_per_x_hex = mode_ka / n_hex * {{multiplier}}, 
      mode_tot_per_x_hex = mode_tot / n_hex * {{multiplier}}, 
      mode_sfn_per_x_hex = sfn_mileage_mode / n_hex * {{multiplier}}, 
      mode_ka_per_cap = mode_ka / tot_pop * {{per_cap_multiplier}}, 
      mode_tot_per_cap = mode_tot / tot_pop * {{per_cap_multiplier}}, 
      mode_sfn_per_cap = sfn_mileage_mode / tot_pop * {{per_cap_multiplier}}
    ) %>% 
    data.frame()
  
  #------------- KA ratio 
  hex_df_summary$mode_ka_ratio_urban <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'urban'))$mode_ka_per_x_hex / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'urban'))$mode_ka_per_x_hex
  
  hex_df_summary$mode_ka_ratio_rural <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'rural'))$mode_ka_per_x_hex / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'rural'))$mode_ka_per_x_hex
  
  #------------- tot ratio 
  hex_df_summary$mode_tot_ratio_urban <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'urban'))$mode_tot_per_x_hex / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'urban'))$mode_tot_per_x_hex
  
  hex_df_summary$mode_tot_ratio_rural <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'rural'))$mode_tot_per_x_hex / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'rural'))$mode_tot_per_x_hex
  
  #------------- SFN ratio
  hex_df_summary$mode_sfn_ratio_urban <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'urban'))$mode_sfn_per_x_hex / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'urban'))$mode_sfn_per_x_hex
  
  hex_df_summary$mode_sfn_ratio_rural <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'rural'))$mode_sfn_per_x_hex / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'rural'))$mode_sfn_per_x_hex
  
  #------------- KA Capita ratio 
  hex_df_summary$mode_ka_cap_ratio_urban <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'urban'))$mode_ka_per_cap / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'urban'))$mode_ka_per_cap
  
  hex_df_summary$mode_ka_cap_ratio_rural <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'rural'))$mode_ka_per_cap / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'rural'))$mode_ka_per_cap
  
  #------------- SFN Capita ratio 
  hex_df_summary$mode_sfn_cap_ratio_urban <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'urban'))$mode_sfn_per_cap / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'urban'))$mode_sfn_per_cap
  
  hex_df_summary$mode_sfn_cap_ratio_rural <- 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('Yes'), 
      geo_type == 'rural'))$mode_sfn_per_cap / 
    (hex_df_summary %>% filter(
      dis_adv_com %in% c('No'), 
      geo_type == 'rural'))$mode_sfn_per_cap
  
  #-------------
  # collapse ratios between urb and rur
  hex_df_summary <- 
    hex_df_summary %>% 
    mutate(
      mode_ka_ratio = case_when(
        geo_type == 'urban' ~ mode_ka_ratio_urban, 
        geo_type == 'rural' ~ mode_ka_ratio_rural, 
        TRUE ~ NA), 
      mode_tot_ratio = case_when(
        geo_type == 'urban' ~ mode_tot_ratio_urban, 
        geo_type == 'rural' ~ mode_tot_ratio_rural, 
        TRUE ~ NA), 
      mode_sfn_ratio = case_when(
        geo_type == 'urban' ~ mode_sfn_ratio_urban, 
        geo_type == 'rural' ~ mode_sfn_ratio_rural, 
        TRUE ~ NA), 
      mode_ka_cap_ratio = case_when(
        geo_type == 'urban' ~ mode_ka_cap_ratio_urban, 
        geo_type == 'rural' ~ mode_ka_cap_ratio_rural, 
        TRUE ~ NA), 
      mode_sfn_cap_ratio = case_when(
        geo_type == 'urban' ~ mode_sfn_cap_ratio_urban, 
        geo_type == 'rural' ~ mode_sfn_cap_ratio_rural, 
        TRUE ~ NA) 
    )
  
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_ka_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_tot_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_sfn_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_ka_cap_ratio"] <- NA
  hex_df_summary[hex_df_summary$dis_adv_com==total_name, "mode_sfn_cap_ratio"] <- NA
  
  
  hex_df_summary_pre_fmt <- 
    hex_df_summary %>% 
    mutate(
      across(
        c(pct_hex,
          pct_mode_ka, pct_mode_tot,
          pct_sfn_mileage_all,
          pct_sfn_mileage_mode, pct_pop),
        function(x) percent(x, accuracy=0.1)),
      across(
        c(n_hex,
          mode_ka, mode_tot,
          sfn_mileage_all,
          sfn_mileage_mode, tot_pop),
        function(x) number(x, accuracy=1, big.mark = ",")), 
      across(
        c(mode_ka_per_x_hex, mode_tot_per_x_hex, 
          mode_ka_ratio, mode_tot_ratio, 
          mode_sfn_ratio, mode_sfn_per_x_hex, 
          mode_tot_per_cap, mode_ka_per_cap, mode_sfn_per_cap, 
          mode_ka_cap_ratio, mode_sfn_cap_ratio),
        function(x) format(round(as.numeric(x), 2), nsmall=2, big.mark=","))
    ) 
  
  if (stacking == TRUE) {
    hex_df_summary_stack <- 
      hex_df_summary_pre_fmt %>% 
      select(
        mode,
        {{var}},
        dis_adv_com, 
        n_hex,
        pct_hex,
        mode_tot, pct_mode_tot, 
        mode_ka, pct_mode_ka, 
        sfn_mileage_mode, pct_sfn_mileage_mode, mode_sfn_per_x_hex, mode_sfn_ratio, 
        mode_tot_per_x_hex, mode_tot_ratio, 
        mode_ka_per_x_hex, mode_ka_ratio, 
        mode_tot_per_cap, mode_ka_per_cap, mode_sfn_per_cap,
        tot_pop, pct_pop,
        mode_ka_cap_ratio, mode_sfn_cap_ratio
      ) %>% 
      data.frame() 
    
    return(hex_df_summary_stack)
    
  } else {
    hex_df_summary_fmt <-
      hex_df_summary_pre_fmt %>% 
      select(
        mode,
        {{var}},
        dis_adv_com, 
        n_hex,
        pct_hex,
        tot_pop, 
        pct_pop,
        mode_tot, 
        pct_mode_tot, 
        mode_ka, 
        pct_mode_ka, 
        sfn_mileage_mode, 
        pct_sfn_mileage_mode, 
        mode_sfn_per_x_hex, 
        mode_sfn_ratio,
        mode_tot_per_x_hex,
        mode_tot_ratio, 
        mode_ka_per_x_hex, 
        mode_ka_ratio, 
        mode_tot_per_cap, 
        mode_ka_per_cap, 
        mode_ka_cap_ratio,
        mode_sfn_per_cap, 
        mode_sfn_cap_ratio
      ) %>% 
    kable(
      col.names = c(
        'Mode',
        'Disadvantaged Community',
        {{var_label}},
        '# of Hex Cells', 
        '% of Hex Cells', 
        'Total Population', 
        '% Population',
        '# of Crashes', 
        '% of Crashes', 
        '# of KA Crashes', 
        '% KA of Crashes',
        "# of Modal SFN",
        "% of Modal SFN", 
        "# Miles of Modal SFN per 100 Hex Cells", 
        "SFN Mileage per 100 Hex Ratio",
        "# Crashes per 100 Hex Cells",
        "Crash Ratio",
        "# KA Crashes per 100 Hex Cells",
        "KA Crash Ratio", 
        'KA Crashes per 100K residents', 
        'KA Crashes per Capita Ratio',
        'Crashes per 100K residents', 
        'SFN Miles per 100K residents', 
        'SFN Miles per Capita Ratio'
      ),
      align= 'lrrrrrrrrrcrcrr',
      table.attr = "style = \"color: black;\""
    ) %>%
      collapse_rows(columns = c(1, 12, 14, 16)) %>% 
      kable_styling() %>% 
      row_spec(0, bold = T, color = "white", background = ssrc_blue) %>% 
      row_spec(which(hex_df_summary_pre_fmt$dis_adv_com == total_name), bold = T, color = "black", background = '#EEEEEE', italic = TRUE) %>%
      
      
      return(hex_df_summary_fmt)
  }
}


cli::cli_inform(
  c("v" = "Hi! I'm a Descriptive Helper...and I'm here to help!\n")
)
