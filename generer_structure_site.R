# web analytics: https://araignee-idf.goatcounter.com
if (!require("pak")) install.packages("pak")
pak::pkg_install("CedricMondy/taxref4R")

taxref_araignees <- readRDS("taxref_araignees.rds")
liste_rouge <- readRDS("lrn.RDS")
load("urls.rda")

donnees_importees <- openxlsx2::read_xlsx("liste-sp-idf-identif-a-vue-v2024-1.xlsx", sheet = "diffusion-v1") |>
  tibble::as_tibble() |>
  janitor::clean_names()

liste_esp <- donnees_importees |>
  purrr::set_names(
    c("famille", "taxon", "nb_sp_idf", "id_vue", "difficulte_id", "condition", "confusions", "commentaires")
  ) |>
  tidyr::drop_na(famille, taxon) |>
  dplyr::left_join(
    openxlsx2::read_xlsx("photos.xlsx") |>
      dplyr::filter(!dplyr::if_all(c(male, femelle), is.na)),
    by =  "taxon"
  )

liste_familles <- liste_esp |>
  dplyr::distinct(famille) |>
  dplyr::pull(famille)

liste_genre <- liste_esp |>
  dplyr::filter(!is.na(nb_sp_idf)) |>
  dplyr::select(famille, genre = taxon, nb_sp_idf)

liste_espece <- liste_esp |>
  dplyr::filter(is.na(nb_sp_idf)) |>
  tidyr::drop_na(famille, taxon) |>
  dplyr::left_join(liste_rouge, by = c("taxon" = "LB_NOM")) |>
  dplyr::left_join(especes_piwigo, by = c("taxon" = "espece")) |>
  dplyr::left_join(especes_nmbe, by = c("taxon" = "espece"), suffix = c("_piwigo", "_nmbe"))

# Si nécessaire, mise à jour des CD_REF
# liste_espece$CD_REF[is.na(liste_espece$CD_REF)] <- sapply(
#   liste_espece$taxon[is.na(liste_espece$CD_REF)],
#   function(x) {
#     taxref4R::search_taxa(scientificNames = x)$referenceId[1]
#   }
# )

liste_espece$CD_REF <- liste_espece |>
  dplyr::select(CD_REF) |>
  dplyr::left_join(
    taxref_araignees |>
      dplyr::select(CD_NOM, CD_REF),
    by = c("CD_REF"="CD_NOM")
    ) |>
  dplyr::pull(CD_REF.y)

purrr::walk(
  liste_familles,
  function(family) {
    genera <- liste_genre |>
      dplyr::filter(famille == family) |>
      dplyr::pull(genre)

    brew::brew(file = "template_famille.qmd", output = paste0(family, ".qmd"))
  }
  )


panneau_gauche <- list(
  style = "docked",
  contents = list(
    list(
      section = "Liste des familles",
      contents = list.files(pattern = "dae.qmd") |>
        stringr::str_remove(pattern = "\\.qmd") |>
        lapply(
          function(famille) {
            list(
              text = famille,
              href = paste0(famille, ".html")
            )
          }
        )
    )
  )
)

source("prep_graphes_index.R")

quarto_config <- yaml::read_yaml("_quarto_raw.yml")
quarto_config$website$sidebar <- panneau_gauche
yaml::write_yaml(quarto_config, "_quarto.yml")
readLines("_quarto.yml") |>
  stringr::str_replace(pattern = "toc: yes", replacement = "toc: true") |>
  stringr::str_replace(pattern = "toc: no", replacement = "toc: false") |>
  stringr::str_replace(pattern = "reader-mode: yes", replacement = "reader-mode: true") |>
  stringr::str_replace(pattern = "reader-mode: no", replacement = "reader-mode: false") |>
  writeLines("_quarto.yml")
