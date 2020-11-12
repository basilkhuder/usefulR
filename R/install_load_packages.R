install_load_packages <- function(cran_pkg = NULL,
                                  bioconductor_pkg = NULL,
                                  github_pkg = NULL,
                                  update_bioc = FALSE,
                                  def_func = NULL) {
  if (!is.null(cran_pkg)) {
    cran_pkg_install <-
      cran_pkg[!cran_pkg %in% installed.packages()[, "Package"]]
    
    if (!identical(cran_pkg_install, "character(0)")) {
      install.packages(cran_pkg_install)
    }
  }
  
  if (!is.null(bioconductor_pkg)) {
    if ((!"BiocManager" %in% installed.packages()[, "Package"])) {
      install.packages("BiocManager")
    }
    bc_pkg_install <-
      bioconductor_pkg[!bioconductor_pkg %in% installed.packages()[, "Package"]]
    if (!identical(bc_pkg_install, "character(0)")) {
      
    }
    BiocManager::install(bc_pkg_install, update = update_bioc)
  }
  
  if (!is.null(github_pkg)) {
    if ((!"devtools" %in% installed.packages()[, "Package"])) {
      install.packages("devtools")
    }
    gh_pkg_install <-
      github_pkg[!github_pkg %in% installed.packages()[, "Package"]]
    if (!identical(gh_pkg_install, "character(0)")) {
      
    }
    devtools::install_github(gh_pkg_install)
  }
  
  invisible(sapply(c(cran_pkg, bioconductor_pkg, github_pkg), function(x)
    require(x, character.only = TRUE)))
  
  if (!is.null(def_func)) {
    purrr::walk(def_func, ~ 
                  assign(.x[1], value = eval(parse(text = .x[2])), envir = .GlobalEnv))
  }
  
}
