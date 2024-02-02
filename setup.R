here::i_am("setup.R")

needed_packages = list(
  CRAN = c("worrms", "readr", "rfishbase", "dplyr", "here", "remotes",
           "rentrez", "stringr", "argparser"),
  github = c(charlier = "BigelowLab")
)

installed = installed.packages() |> rownames()
for (pkg in needed_packages$CRAN) {
  if (!pkg %in% installed) install.packages(pkg, repos = "https://cloud.r-project.org/")
}
for (pkg in names(needed_packages$github)) {
  if (!pkg %in% installed) {
    p = paste(needed_packages$github[[pkg]], pkg, sep = "/")
    remotes::install_github(p)
  }
}


suppressPackageStartupMessages({
  for (p in needed_packages$CRAN) library(p, character.only = TRUE)
  for (p in names(needed_packages$github)) library(p, character.only = TRUE)
})

ff = list.files("functions", full.names = TRUE, pattern = "^.*\\.R$")
for (f in ff) source(f)
