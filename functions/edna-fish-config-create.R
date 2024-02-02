# this is a temporary file and script used for development only

cfg = list(
  version = "v0.000",
  rootpath = "/mnt/s1/projects/ecocast/projects/yesmaelli/edna-fish-refdb/yasmina",
  region = "nwa",
  filename = "SpeciesList_UpdatedNames.csv",
  groups = list(fish = c("Actinopterygii","Elasmobranchii")),
  default_label = "nonfish",
  lazy_fishbase = TRUE,
  fish_filters = list(
    country = list(country = c( "USA", "Canada","Greenland")),
    dist = list(FAO = c("Atlantic, Northwest", "Atlantic, Western Central",
                        "America, North - Inland waters","Arctic Ocean")),
    eco= list(EcosystemName = c("Arctic", "Arctic Complex","Atlantic Complex",
                                "Atlantic Ocean","Canadian Eastern Arctic - West Greenland",
                                "Canadian High Arctic - North Greenland",
                                "Chesapeake Bay", "Gulf of Maine",
                                "Hudson Bay Complex", "Hudson Complex",
                                "Labrador - Newfoundland",
                                "Northeast U.S. Continental Shelf",
                                "St. Lawrence Complex",
                                "St. Lawrence","West Greenland Shelf"))),
  entrez = list(
    search_modifier_1 = "[ORGN] AND 12S[ALL] AND voucher[ALL]",
    db_1 = "nucleotide",
    search_modifier_2 = "[ORGN] AND 12S[ALL]",
    db_2 = "nucleotide"
  )

) |>
  write_configuration()



