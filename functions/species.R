#' Read a species list
#'
#' @param filename chr, the name of the file (with extension and path)
#' @param keep chr, one or more column names to keep.  "all" to keep them all.
read_species_list = function(filename = "SpeciesList_UpdatedNames.csv",
                             keep = c("ScientificName_accepted", "Phylum/Division",
                                      "Class", "Order", "Family")){
  x = readr::read_csv(filename, show_col_types = FALSE)
  if (!('all' %in% keep)) x = dplyr::select(x, dplyr::all_of(keep))
  x
}

#' Label a species table by user specified groups.  Each row is assigned to at 
#' least one group or the default group.
#'
#' @param x table of species
#' @param variable char name of the column to filter
#' @param groups named list, one or more character vectors of values belong to the group(s)
#' @param default chr, the default grouping for ungroup rows
#' @return the input with new column of group.  Ungrouped rows are assigned the default.
label_species_list = function(x = read_species(),
                              variable = "Class",
                              groups = list(fish = c("Actinopterygii","Elasmobranchii")),
                              default = NA_character_){

  x = x |>
    dplyr::mutate(group = default, .before = 1)

  for (nm in names(groups)){
    ix = x[[variable[1]]] %in% groups[[nm]]
    x$group[ix] = nm
  }

  x
}


#' A function narrow the scope of species to include in the database
#' 
#' User provides a configuration file with fishbase and/or sealifebase search 
#' specifications. User may also provide an optional "preselection" to narrow
#' the search range.
#'
#' @param cfg a configuration list
#' @param save_config logical, if TRUE save the provided config in the destination
#'   directory
#' @return a table of target species
select_target_species = function(cfg, save_config = TRUE){

  output_path = version_path(cfg$version, root_path = cfg$rootpath)
  if (!dir.exists(output_path)) ok = dir.create(output_path, recursive = TRUE)
  
  if (save_config) ok = write_configuration(cfg)
  
  
  # user provides preselected listing of species OR
  # possibly NULL which mean 'get them all'
  if (!is.null(cfg$preselect_filename)){
    species = read_species_list(filename = cfg$preselect_filename) |>
      label_species_list(groups = cfg$groups, default = cfg$default_label)
    species_list = dplyr::filter(species, group %in% names(cfg$groups)) |>
      dplyr::pull(dplyr::all_of(cfg$preselect_column))
  } else {
    species_list = NULL
  }

  
  fb_path = file.path(cfg$rootpath, "data", "fishbase")
  if (!dir.exists(fb_path)) ok = dir.create(fb_path)
  sb_path = file.path(cfg$rootpath, "data", "sealifebase")
  if (!dir.exists(sb_path)) ok = dir.create(sb_path)
  
  # lazy means read the file that already exist in data/fishbase and/or data/sealifebase
  # not lazy means use rfishbase to query and then save to the above
  if (cfg$lazy_database){
    
    if (!is.null(cfg$fishbase)){
      fb = sapply(names(cfg$fishbase),
                  function(n){
                    readr::read_csv(file.path(fb_path,
                                              sprintf("%s-%s.csv.gz", cfg$region, n)),
                                    show_col_types = FALSE)
                  }, simplify = FALSE)
    } else {
      fb = NULL
    }
    
    if (!is.null(cfg$sealifebase)){
      fb = sapply(names(cfg$sealifebase),
                  function(n){
                    readr::read_csv(file.path(sb_path,
                                              sprintf("%s-%s.csv.gz", cfg$region, n)),
                                    show_col_types = FALSE)
                  }, simplify = FALSE)
    } else {
      sb = NULL
    }
    
  } else {
    # here we leverage rfishbase::<function> where function is country, 
    # ecosystem, distribution, etc to fetch candidates
    if (!is.null(cfg$fishbase)){
      fb = lapply(names(cfg$fishbase),
                  function(n){
                    f = getFromNamespace(n, ns = "rfishbase", server = "fishbase")
                    f(species_list = species_list) |>
                      readr::write_csv(file.path(fb_path,
                                                 sprintf("%s-%s.csv.gz", cfg$region, n))) 
                  })
    } else {
      fb = NULL
    }
    
    if (!is.null(cfg$sealifebase)){
      sb = lapply(names(cfg$sealifebase),
                  function(n){
                    f = getFromNamespace(n, ns = "rfishbase", server = "sealifebase")
                    f(species_list = species_list) |>
                      readr::write_csv(file.path(sb_path, 
                                                 sprintf("%s-%s.csv.gz", cfg$region, n)))
                  })
    } else {
      sb = NULL
    }
    
  } #load lazily or fetch-and-save


  # select desired columns and then filter
  if (!is.null(cfg$fishbase) && !is.null(fb)){
    fb = lapply(names(fb),
               function(nm){
                 cname = names(cfg$fishbase[[nm]])
                 vals = cfg$fishbase[[nm]][[cname]]
                 dplyr::select(fb[[nm]], dplyr::all_of(c("Species", cname))) |>
                   dplyr::filter(get({{cname}}) %in% vals) |>
                   dplyr::select(dplyr::all_of("Species"))
               })
  }
  
  # select desired columns and then filter
  if (!is.null(cfg$sealifebase) && !is.null(sb)){
    sb = lapply(names(sb),
                function(nm){
                  cname = names(cfg$sealifebase[[nm]])
                  vals = cfg$sealifebase[[nm]][[cname]]
                  dplyr::select(sb[[nm]], dplyr::all_of(c("Species", cname))) |>
                    dplyr::filter(get({{cname}}) %in% vals)|>
                    dplyr::select(dplyr::all_of("Species"))
                })
  }
  
  db = c(fb, sb) |>
    unname() |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    readr::write_csv(file.path(output_path, sprintf("%s-species.csv.gz", cfg$region)))

}

