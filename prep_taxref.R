taxref <- archive::archive_read("TAXREF_v18_2025.zip", file = "TAXREFv18.txt")
taxref_araignees <- taxref |> dplyr::filter(ORDRE == "Araneae", FR == "P" & !is.na(FR))
saveRDS(taxref_araignees, file = "taxref_araignees.rds")
