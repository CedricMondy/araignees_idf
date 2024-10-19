# Utiliser rocker/tidyverse comme image de base
FROM rocker/tidyverse

# Mettre à jour les packages du système
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libcurl4-openssl-dev

# Installer les packages R
RUN install2.r --error \
  openxlsx2 \
  janitor\
  brew \
  yaml \
  rvest
