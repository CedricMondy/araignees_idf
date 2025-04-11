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
  url_familles <- list_family_pages()
  species <- list()
  for (url_famille in url_familles) {
     url_specs <- url_famille |>
          httr::GET() |>
          rvest::read_html() |>
          rvest::html_elements("body > div > div > ul > li > div > div.description > h3 > a")

     species[[url_famille]] <- tibble::tibble(
       famille = url_famille |>
         stringr::str_extract(pattern = "(?<=[-/])[a-z]+dae$") |>
         stringr::str_to_sentence(),
       espece = url_specs |>
         rvest::html_text(),
       url = paste0(
         "https://arachno.piwigo.com/",
         url_specs |>
           rvest::html_attr("href")
       )
     )
     Sys.sleep(.5)
  }

  species |>
    purrr::list_rbind()
}

#content > ul > li:nth-child(1) > div > div.description > h3 > a
