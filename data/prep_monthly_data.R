openobs <- archive::archive_read("data/records-2025-03-04.zip", file = "records-2025-03-04.csv") |>
  vroom::vroom() |>
  janitor::clean_names() |>
  dplyr::filter(
    ! region %in% c("Guadeloupe", "Guyane française", "La Réunion", "Martinique", "Mayotte", "Nouvelle-Calédonie", "Polynésie française", "Saint-Barthélemy", "Saint-Martin", "Saint-Pierre-et-Miquelon", "TAAF : Îles éparses", "Wallis et Futuna")
  )
geonat <- vroom::vroom("data/synthese_observations_2025-03-03T21_38_54.255Z.csv") |>
  janitor::clean_names()

synthese <- geonat |>
  dplyr::filter(rang_taxo == "ES") |>
  dplyr::select(
    uuid = uuid_perm_sinp,
    observateur = observateurs, date = date_debut,
    cd_ref, stade_vie, sexe
  ) |>
  dplyr::bind_rows(
    openobs |>
      dplyr::filter(
        rang_taxo == "species",
        ! id_sinp_occ_tax %in% geonat$uuid_perm_sinp
      ) |>
      dplyr::select(
        uuid = id_sinp_occ_tax,
        observateur, date = date_observation,
        cd_ref, stade_vie = occ_stade_de_vie, sexe = occ_sexe
      )
  ) |>
  dplyr::mutate(
    annee = lubridate::year(date),
    mois = lubridate::month(date, label = TRUE),
    stade_vie = dplyr::case_when(
      stade_vie %in% c("Adulte", "Imago") ~ "adulte",
      stade_vie %in% c("Immature", "Juvénile", "Sub-adulte") ~ "juvénile",
      stade_vie %in% c("Inconnu", "Indéterminé") ~ "inconnu",
      TRUE ~ stade_vie
    )
  ) |>
  dplyr::filter(
    annee > 2000,
    stade_vie %in% c("inconnu", "juvénile", "adulte")
    ) |>
  dplyr::count(
    cd_ref, mois, stade_vie
  ) |>
  tidyr::complete(cd_ref, mois, stade_vie, fill = list(n = 0)) |>
  dplyr::group_by(cd_ref) |>
  dplyr::mutate(
    p = 100 * n / sum(n),
    stade_vie = stade_vie |>
      factor(levels = c( "adulte", "juvénile", "inconnu"))
    )


synthese |>
  dplyr::group_by(cd_ref) |>
  dplyr::group_split(.keep = TRUE) |>
  purrr::walk(
    function(df) {
      p <- df |>
        ggplot2::ggplot() +
        ggplot2::geom_col(
          mapping = ggplot2::aes(
            x = mois, y = p, fill = stade_vie
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = "white"),
          plot.background = ggplot2::element_rect(fill = "white"),
          panel.border = ggplot2::element_blank()
        ) +
        ggplot2::scale_fill_manual(
          name = "Stade de vie",
          values = c(
            inconnu = "darkgrey",
            juvénile = "lightblue",
            adulte = "darkblue"
          )
        )

      webpea::webpea(
        plot = p,
        filename = paste0(
          "www/media/plots/", unique(df$cd_ref), ".webp"
        ),
        width = 9, height = 3
      )
    },
    .progress = TRUE
  )


