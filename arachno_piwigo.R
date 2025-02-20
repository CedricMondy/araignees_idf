list_family_pages_genitalia <- function() {

  extract_family <- function(x) {x[stringr::str_detect(x, "dae")]}

  paste0(
    "https://arachno.piwigo.com/",
    httr::GET("https://arachno.piwigo.com/index?/category/familles") |>
      rvest::read_html() |>
      rvest::html_elements("dd > div > span > a") |>
      rvest::html_attr("href") |>
      extract_family()
  )

}

list_family_pages <- function() {
  extract_family <- function(x) {x[stringr::str_detect(x, "dae")]}

  paste0(
    "https://arachno.piwigo.com/",
    httr::GET("https://arachno.piwigo.com/index?/category/familles") |>
      rvest::read_html() |>
      rvest::html_elements("ul.thumbnailCategories > li > div.thumbnailCategory > div.illustration > a") |>
      rvest::html_attr("href") |>
      extract_family()
  )
}

list_species_pages <- function() {
  list_family_pages() |>
    purrr::map(
      function(url) {
        url_specs <- url |>
          httr::GET() |>
          rvest::read_html() |>
          rvest::html_elements("body > div > div > ul > li > div > div.description > h3 > a")

        tibble::tibble(
          espece = url_specs |>
            rvest::html_text(),
          url = paste0(
            "https://arachno.piwigo.com/",
            url_specs |>
              rvest::html_attr("href")
          )
        )
      },
      .progress = TRUE
    ) |>
    purrr::list_rbind()
}

#content > ul > li:nth-child(1) > div > div.description > h3 > a
