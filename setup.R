
# Set CRAN mirror for non-interactive sessions
local({
  r = getOption("repos")
  if (is.null(r) || identical(r["CRAN"], "@CRAN@") || is.na(r["CRAN"])) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
})

ensure_package = function(pkg, github = NULL, do_library = TRUE){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!is.null(github)) {
      message("Installing ", pkg, " from GitHub: ", github)
      if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
      devtools::install_github(github)
    } else {
      message("Installing ", pkg, " from CRAN...")
      install.packages(pkg)
    }
  }
  if (do_library) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}


ensure_package("DescTools", do_library = FALSE)
ensure_package("RSBID", github = "dongyuanwu/RSBID")
cran_pkgs = c("readr", "randomForest", "caret", "rjags", "runjags", "pROC", "PRROC", "psych", "tidyverse")
invisible(lapply(cran_pkgs, ensure_package))




