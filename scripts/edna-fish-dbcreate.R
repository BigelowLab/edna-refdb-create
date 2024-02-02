#' usage: edna-fish-dbcreate.R [--] [--help] [--config_file CONFIG_FILE]
#' 
#' eDNA Regional Fish Database Generator
#' 
#' flags:
#'   -h, --help         show this help message and exit
#' 
#' optional arguments:
#'   -c, --config_file  Configuration file in YAML format 
#' [default: /mnt/s1/projects/ecocast/projects/yesmaelli/edna -fish-refdb/yasmina/data/versions/v0/v0.000/v0.000.yaml]

source("setup.R")

Args = argparser::arg_parser("eDNA Regional Fish Database Generator",
                             name = "edna-fish-dbcreate.R",
                             hide.opts = TRUE) |>
  argparser::add_argument("--config_file",
                          help = "Configuration file in YAML format",
                          default = "example/v0.000.yaml") |>
  argparser::parse_args()

cfg = read_configuration(Args$config_file)

x = select_target_species(cfg)

y = search_target_species(cfg)

