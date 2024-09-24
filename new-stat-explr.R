sumsts <- read.csv("sumsts.csv")

sumsts <- sumsts %>%
  mutate(perc = case_when(
    sp == "American_Golden-Plover" ~ "-73.31",
    sp == "Bairds_Sandpiper" ~ "-5.28",
    sp == "Black-bellied_Plover" ~ "-57.39",
    sp == "Buff-breasted_Sandpiper" ~ "-58.17",
    sp == "Cackling_Goose" ~ "833.13",
    sp == "Glaucous_Gull" ~ "0",
    sp == "King_Eider" ~ "-83.34",
    sp == "Long-tailed_Duck" ~ "-65.04",
    sp == "Long-tailed_Jaeger" ~ "0",
    sp == "Pacific_Loon" ~ "0",
    sp == "Parasitic_Jaeger" ~ "0",
    sp == "Pectoral_Sandpiper" ~ "-64.40",
    sp == "Red_Knot" ~ "-94.05",
    sp == "Red-throated_Loon" ~ "21.18",
    sp == "Ruddy_Turnstone" ~ "-76.29",
    sp == "Snow_Goose" ~ "1",
    sp == "Tundra_Swan" ~ "1",
    sp == "White-rumped_Sandpiper" ~ "-26.57",
  ))
sumsts$perc <- as.numeric(sumsts$perc)
sumsts <- sumsts %>%
  select(-max, -min, -sd)

amg <- sumsts[grepl("American_Golden-Plover", sumsts$sp), ]
amg <- amg[grepl("155", amg$X), ] # looking at just population density (venter)

model <- lm(perc ~ mean, data = amg)  
summary(model)
