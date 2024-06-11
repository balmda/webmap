source("R/_load_packages.R")
source("R/_color_palettes.R")

acronym <- data.frame(
  `Acronym`=c(
    'AADT', 
    'ADA', 
    'FHWA', 
    'GIS', 
    'HIN', 
    'KA',
    'MSI',
    'MC',
    'MV',
    'PED',
    'PHB',
    'RRFB',
    'SSA',
    'VPD',
    'VRU',
    'VRUSA',
    'K [**K**ABCO]' ,
    'A [K**A**BCO]' ,
    'C [KA**B**CO]',
    'C [KAB**C**O]',
    'O [KABC**O**] or PDO' ,
    'KA or KSI'
  ),
  `Definition`=c(
    'Annual Average Daily Traffic',
    'Americans with Disabilities Act',
    'Federal Highway Administration',
    'Geographic Information System',
    'High Injury Network',
    'Killed of severely injured',
    'Most severely injured',
    'Motorcycle',
    'Motor Vehicle',
    'Pedestrian',
    'Pedestrian Hybrid Beacon',
    'Rectangular Rapid Flashing Beacon',
    'Safe System Approach',
    'Vehicles per day',
    'Vulnerable road user',
    'Vulnerable Road User Safety Assessment',
    'Fatal Injury Severity',
    'Suspected Serious Injury',
    'Minor Injury',
    'Possible Injury',
    'Property Damage Only',
    'Killed or Seriously Injured'
  )
) %>% 
  kable(
    align = "ll", 
    format = "html", escape = F,
    table.attr = "style = \"color: black;\""
  ) %>%
  kable_styling()%>% 
  row_spec(0, bold = T, color = "white", background = ssrc_blue)




