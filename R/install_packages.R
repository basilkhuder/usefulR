install_packages <- function(cran_pkg = NULL, bioconductor_pkg = NULL, update_bioc = FALSE){
  if(!is.null(cran_pkg)){
  cran_pkg_install <- cran_pkg[!cran_pkg %in% installed.packages()[, "Package"]]

  if(!identical(cran_pkg_install, "character(0)")) {
    install.packages(cran_pkg_install)
    }
  }
  if(!is.null(bioconductor_pkg)){
    bc_pkg_install <- bioconductor_pkg[!bioconductor_pkg %in% installed.packages()[, "Package"]]
    if(!identical(bc_pkg_install, "character(0)")) {
      if(!("BiocManager" %in% bc_pkg_install)){
        BiocManager::install(bc_pkg_install, update = update_bioc)
      }
    }
  }
  invisible(sapply(c(cran_pkg, bioconductor_pkg), require, character.only = TRUE))
}
