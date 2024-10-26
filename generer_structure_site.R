if (!require("pak")) install.packages("pak")
pak::pkg_install("CedricMondy/taxref4R")

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

liste_espece$CD_REF[is.na(liste_espece$CD_REF)] <- sapply(
  liste_espece$taxon[is.na(liste_espece$CD_REF)],
  function(x) {
    taxref4R::search_taxa(scientificNames = x)$referenceId[1]
  }
)


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
  list(
    text = "Familles",
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

quarto_config <- yaml::read_yaml("_quarto_raw.yml")
quarto_config$website$sidebar <- panneau_gauche
yaml::write_yaml(quarto_config, "_quarto.yml")
readLines("_quarto.yml") |>
  stringr::str_replace(pattern = "toc: yes", replacement = "toc: true") |>
  writeLines("_quarto.yml")
