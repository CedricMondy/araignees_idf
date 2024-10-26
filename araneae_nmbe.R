list_species_pages_nmbe <- function() {
  extract_species_list_url <- function(x){
    x[stringr::str_detect(x, "genspec")]
  }
  extract_species_data <- function(x){
    x[stringr::str_detect(x, "data")]
  }

  genspec <- paste0(
    "https://araneae.nmbe.ch",
    httr::GET("https://araneae.nmbe.ch/list/families") |>
      rvest::read_html() |>
      rvest::html_elements("body > div.wrapper > div.container > div.table-responsive > table > tbody > tr > td > a") |>
      rvest::html_attr("href") |>
      extract_species_list_url()
  )

  spec_urls <- paste0(
    "https://araneae.nmbe.ch",
    genspec |>
      purrr::map(
        function(species_list) {
          if (httr::status_code(httr::GET(species_list)) == 200) {
            species_list |>
              httr::GET() |>
              rvest::read_html() |>
              rvest::html_elements("body > div.wrapper > div.container > div.table-responsive > table > tbody > tr > td > a") |>
              rvest::html_attr("href") |>
              extract_species_data()
          }
          }
        ) |>
      purrr::list_c()
  )

  tibble::tibble(
    espece =  spec_urls |>
      purrr::map(
        function(url) {
          if (httr::status_code(httr::GET(url)) == 200) {
            url |>
              httr::GET() |>
              rvest::read_html() |>
              rvest::html_elements("body > div.wrapper > div.container > div.row > div.col-md-6 > h4 > em") |>
              rvest::html_text()
          } else {
            NA
          }
          },
        .progress = TRUE
      ) |>
      purrr::list_c(),
    url = spec_urls
  )



}
