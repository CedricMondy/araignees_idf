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

  spec_urls |>
    purrr::map(
      function(url) {
        if (httr::status_code(httr::GET(url)) == 200) {
          page_url <- url |>
            httr::GET() |>
            rvest::read_html()

          tibble::tibble(
            espece = page_url |>
              rvest::html_elements("body > div.wrapper > div.container > div.row > div.col-md-6 > h4 > em") |>
              rvest::html_text(),
            size = page_url |>
              rvest::html_elements("div.sp_description") |>
              rvest::html_text() |>
              stringr::str_extract_all(pattern = "Body length.*mm") |>
              purrr::list_c() |>
              stringr::str_remove_all(
                pattern = "Body length"
              ) |>
              stringr::str_trim()|>
              paste(collapse = "\n"),
            url = url
          )
        }
      },
      .progress = TRUE
    ) |>
    purrr::list_rbind()

}

