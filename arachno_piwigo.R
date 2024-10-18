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

