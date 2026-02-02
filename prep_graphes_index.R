if (!require("webpea")) pak::pkg_install("nucleic-acid/webpea")

photos <- openxlsx2::read_xlsx("photos.xlsx")

completude <- photos |>
  janitor::clean_names() |>
  dplyr::mutate(id_a_vue = stringr::str_replace(string = id_a_vue, pattern = "\\?", replacement = "non")) |>
  dplyr::group_by(id_a_vue) |>
  dplyr::summarise(
    nb_sp = dplyr::n_distinct(taxon),
    nb_fiches = dplyr::n_distinct(taxon[fiche])
  ) |>
  dplyr::mutate(
    nb_sans_fiches = nb_sp - nb_fiches
  ) |>
  dplyr::select(-nb_sp) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("nb_"),
    names_to = "fiches",
    values_to = "nombre"
  ) |>
  dplyr::mutate(
    fiches = dplyr::case_when(
      fiches == "nb_fiches" ~ "oui",
      fiches == "nb_sans_fiches" ~ "non"
    ),
    id_a_vue = dplyr::case_when(
      id_a_vue == "oui" ~ "Identifiables à vue",
      id_a_vue == "non" ~ "Non identifiables à vue"
    ),
    label = ifelse(fiches == "oui", nombre, "")
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = nombre, y = id_a_vue, fill = fiches)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(mapping = ggplot2::aes(label = label), hjust = -.4) +
  ggplot2::facet_wrap(ggplot2::vars(id_a_vue), ncol = 1, scales = "free") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    legend.position = "none",
    axis.text.y = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 12)
  ) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::scale_fill_manual(
    values = c(oui = "#104E8B", non = "#C1CDCD")
  )

webpea::webpea(
  plot = completude,
  filename =  "www/media/plots/completude.webp",
  width = 1600, height = 550, units = "px"
)

completude_sexe <- photos |>
  janitor::clean_names() |>
  dplyr::summarise(
    Femelle = photos |>
      dplyr::filter(!is.na(femelle)) |>
      nrow(),
    `Mâle` = photos |>
      dplyr::filter(!is.na(male)) |>
      nrow(),
    `Mâle et femelle` = photos |>
      dplyr::filter(!is.na(femelle), !is.na(male)) |>
      nrow(),
    toutes = nrow(photos)
  ) |>
  tidyr::pivot_longer(
    cols = -toutes,
    names_to = "sexe",
    values_to = "oui"
  ) |>
  dplyr::mutate(
    non = toutes - oui
  ) |>
  dplyr::select(-toutes) |>
  tidyr::pivot_longer(
    cols = c(oui, non),
    names_to = "fiches",
    values_to = "nombre"
  ) |>
  dplyr::mutate(
    label = ifelse(fiches == "oui", nombre, "")
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = nombre, y = sexe, fill = fiches)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(mapping = ggplot2::aes(label = label), hjust = -.4) +
  ggplot2::facet_wrap(ggplot2::vars(sexe), ncol = 1, scales = "free_y") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    legend.position = "none",
    axis.text.y = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 12)
  ) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::scale_fill_manual(
    values = c(oui = "#104E8B", non = "#C1CDCD")
  )

webpea::webpea(
  plot = completude,
  filename = "www/media/plots/completude_sexe.webp",
  width = 1600, height = 700, units = "px"
)
