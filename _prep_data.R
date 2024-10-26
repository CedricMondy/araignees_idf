source("arachno_piwigo.R")
source("araneae_nmbe.R")

pages_familles <- list_family_pages()
pages_genitalia <- list_family_pages_genitalia()
especes_piwigo <- list_species_pages()

especes_nmbe <- list_species_pages_nmbe()

save(pages_familles, pages_genitalia, especes_piwigo, especes_nmbe, file = "urls.rda")
