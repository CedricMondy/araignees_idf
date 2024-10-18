donnees_importees <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load("liste-especes-araignées-id-vue-IdF.xlsx")) |>
  purrr::map(
    function(sheet_name) {
      openxlsx2::read_xlsx(file = "liste-especes-araignées-id-vue-IdF.xlsx", sheet = sheet_name)
    }
  )

liste_esp <- donnees_importees[-1] |>
  purrr::map(
    function(df) {
      df[-seq(5),] |>
        janitor::clean_names() |>
        dplyr::mutate(
          dplyr::across(dplyr::everything(), as.character)
        )
    }
  ) |>
  purrr::list_rbind() |>
  dplyr::select(
    -paste0("na", c("", "_3", "_4", "_6", "_7", "_8", "_9", "_10", "_11", "_13", "_17", "_19", "_20", "_22"))
  ) |>
  purrr::set_names(
    c("famille", "taxon", "nb_sp_idf", "id_vue", "difficulte_id", "condition", "condition_2", "confusions", "confusions_2", "commentaires")
  ) |>
  tidyr::drop_na(famille, taxon)

liste_familles <- liste_esp |>
  dplyr::distinct(famille) |>
  dplyr::pull(famille)

liste_genre <- liste_esp |>
  dplyr::filter(!is.na(nb_sp_idf)) |>
  dplyr::select(famille, genre = taxon, nb_sp_idf)

liste_esp <- liste_esp |>
  dplyr::filter(is.na(nb_sp_idf)) |>
  tidyr::drop_na(famille, taxon)

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
