openobs <- archive::archive_read("data/records-2025-03-04.zip", file = "records-2025-03-04.csv") |>
  vroom::vroom() |>
  janitor::clean_names() |>
  dplyr::filter(
    ! region %in% c("Guadeloupe", "Guyane française", "La Réunion", "Martinique", "Mayotte", "Nouvelle-Calédonie", "Polynésie française", "Saint-Barthélemy", "Saint-Martin", "Saint-Pierre-et-Miquelon", "TAAF : Îles éparses", "Wallis et Futuna"),
    rang_taxo == "species"
  )
geonat <- vroom::vroom("data/synthese_observations_2025-03-03T21_38_54.255Z.csv") |>
  janitor::clean_names() |>
  dplyr::filter(rang_taxo == "ES")

idf <- COGiter::regions_metro_geo |>
  dplyr::filter(REG == 11) |>
  sf::st_buffer(dist = 1000) |>
  sf::st_transform(crs = 4326)

geonat |>
  dplyr::filter(
    niveau_precision_diffusion != "Maille"
  ) |>
  dplyr::select(
    uuid = uuid_perm_sinp,
    cd_ref, geom = geometrie_wkt_4326
  ) |>
  dplyr::bind_rows(
    openobs |>
      dplyr::filter(
        ! id_sinp_occ_tax %in% geonat$uuid_perm_sinp,
        ! precision_localisation %in% c(paste0("XY centroïde", c("maille", "commune")), "pas de XY (département)"),
        region == "Île-de-France"
      ) |>
      dplyr::select(
        uuid = id_sinp_occ_tax,
        cd_ref, geom = objet_geo_wkt
      )
  ) |>
  dplyr::filter(
    !is.na(geom),
    stringr::str_detect(geom, "POINT")
  ) |>
  sf::st_as_sf(wkt = "geom", crs = 4326) |>
  sf::st_transform(crs = 2154) |>
  sf::st_buffer(dist = 25) |>
  sf::st_write("data/obs_point.gpkg")

sf::st_read("data/mos_ecomos.gpkg") |>
  dplyr::mutate(
    source = ifelse(layer == "Différence", "mos", "ecomos"),
    code = dplyr::case_when(
      code == 1 ~ "1 Forêts",
      code == 2 ~ "2 Milieux semi-naturels",
      code == 3 ~ "3 Espaces agricoles",
      code == 4 ~ "4 Eau",
      code == 5 ~ "5 Espaces ouverts artificialisés",
      code == 6 ~ "6 Habitat individuel",
      code == 7 ~ "7 Habitat collectif",
      code == 8 ~ "8 Activités",
      code == 9 ~ "9 Equipements",
      code == 10 ~ "10 Transports",
      code == 11 ~ "11 Carrières, décharges et chantiers",
      TRUE ~ code
    )
  ) |>
  sf::st_write("data/mos_ecomos.gpkg", delete_layer = TRUE)


synthese <- geonat |>
  dplyr::select(
    uuid = uuid_perm_sinp,
    observateur = observateurs, date = date_debut,
    cd_ref, stade_vie, sexe
  ) |>
  dplyr::bind_rows(
    openobs |>
      dplyr::filter(
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
        ggplot2::theme_light() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
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
        ) +
        ggplot2::labs(
          title = "Temporalité des observations en France"
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

os_by_taxon <- sf::st_read("data/obs_point.gpkg", layer = "os") |>
  dplyr::group_by(cd_ref, code) |>
  dplyr::summarise(.groups = "drop") |>
  dplyr::mutate(surf = sf::st_area(geom)) |>
  sf::st_drop_geometry() |>
  dplyr::group_by(cd_ref) |>
  dplyr::mutate(
    p = round(100 * as.numeric(surf) / sum(as.numeric(surf))),
    code = code |>
      stringr::str_remove(pattern = "\\d* ") |>
      stringr::str_trim() |>
      stringr::str_replace(
        pattern = "Surfaces essentiellement agricoles, interrompues par des espaces naturels importants",
        replacement = "Surfaces essentiellement agricoles avec des espaces naturels"
      ) |>
      factor(
        levels = c(
          "Eau", "Marais intérieurs", "Plans d'eau",
          "Forêts", "Forêts de conifères", "Forêts de feuillus", "Forêts et végétation arbustive en mutation", "Forêts mélangées",
          "Landes et broussailles", "Pelouses et pâturages naturels", "Prairies", "Roches nues", "Végétation clairsemée",
          "Milieux semi-naturels",
          "Surfaces essentiellement agricoles avec des espaces naturels", "Systèmes culturaux et parcellaires complexes",  "Vergers et petits fruits",
          "Espaces agricoles",
          "Espaces ouverts artificialisés",
          "Habitat individuel",
          "Habitat collectif",
          "Activités",
          "Equipements",
          "Transports",
          "Carrières, décharges et chantiers"
        )
      )
    ) |>
  dplyr::ungroup() |>
  tidyr::complete(
    cd_ref, code,
    fill = list(p=0)
  )

os_by_taxon|>
  dplyr::group_by(cd_ref) |>
  dplyr::group_split(.keep = TRUE) |>
  purrr::walk(
    function(df_i) {
      p <- df_i |>
        ggplot2::ggplot() +
        ggplot2::geom_col(
          mapping = ggplot2::aes(
            y = code,
            x = p,
            fill = code
          ),
          show.legend = FALSE
        ) +
        ggplot2::geom_text(
          x = 0,
          hjust = 0,
          size = 4,
          mapping = ggplot2::aes(
            y = code,
            label = code
          )
        ) +
        ggplot2::theme_light() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_text(hjust = 1),
          axis.ticks = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank()
        ) +
        ggplot2::scale_x_continuous(
          name = "Pourcentage d'occupation du sol dans\nun rayon de 25m autour des observations",
          limits = c(0,100),
          breaks = c(0, 25, 50, 75, 100)
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            `Eau` = "lightblue",
            `Marais intérieurs` = "lightblue",
            `Plans d'eau` = "lightblue",
            `Forêts` = "darkgreen",
            `Forêts de conifères` = "darkgreen",
            `Forêts de feuillus` = "darkgreen",
            `Forêts et végétation arbustive en mutation` = "darkgreen",
            `Forêts mélangées` = "darkgreen",
            `Landes et broussailles` = "lightgreen",
            `Pelouses et pâturages naturels` = "lightgreen",
            `Prairies` = "lightgreen",
            `Roches nues` = "grey",
            `Végétation clairsemée` = "#A9AB3A",
            `Milieux semi-naturels` = "#A9AB3A",
            `Surfaces essentiellement agricoles avec des espaces naturels` = "#EBFF66",
            `Systèmes culturaux et parcellaires complexes` = "#EBFF66",
            `Vergers et petits fruits` = "#EBFF66",
            `Espaces agricoles` = "#EBFF66",
            `Espaces ouverts artificialisés` = "#78B886",
            `Habitat individuel` = "grey40",
            `Habitat collectif` = "grey40",
            `Activités` = "grey40",
            `Equipements` = "grey40",
            `Transports` = "grey40",
            `Carrières, décharges et chantiers` = "grey40"
          )
        ) +
        ggplot2::labs(
          title = "Contexte paysager des observations franciliennes"
        )

      webpea::webpea(
        plot = p,
        filename = paste0(
          "www/media/plots/os_", unique(df_i$cd_ref), ".webp"
        ),
        width = 1500, height = 2000, units = "px"
      )
    },
    .progress = TRUE
  )
