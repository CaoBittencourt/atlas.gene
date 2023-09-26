# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# # CRAN packages
# chr_pkg <- c(
#   'devtools'
# )
#
# # Git packages
# chr_git <- c(
#   'CaoBittencourt' = 'atlas.skew'
# )
#
# # genevate / install CRAN packages
# lapply(
#   chr_pkg
#   , function(pkg){
#
#     if(!require(pkg, character.only = T)){
#
#       install.packages(pkg)
#
#     }
#
#     require(pkg, character.only = T)
#
#   }
# )
#
# # genevate / install Git packages
# Map(
#   function(git, profile){
#
#     if(!require(git, character.only = T)){
#
#       install_github(
#         paste0(profile, '/', git)
#         , upgrade = F
#         , force = T
#       )
#
#     }
#
#     require(git, character.only = T)
#
#   }
#   , git = chr_git
#   , profile = names(chr_git)
# )
#
# rm(chr_pkg, chr_git)
#
# [FUNCTIONS] ---------------------------
# - Generality function ---------------------------------------------------
fun_gene_generality <- function(dbl_profile, dbl_scale_lb = 0){

  # Arguments validation
  stopifnot(
    "'dbl_profile' must be numeric." =
      is.numeric(dbl_profile)
  )

  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )

  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb

  # Drop NAs
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile

  # Apply bounded variable skewness function
  fun_skew_sdmode(
    dbl_var =
      dbl_profile
    , dbl_scale_lb =
      dbl_scale_lb
    , dbl_scale_ub =
      max(dbl_profile)
  ) -> dbl_generality

  rm(dbl_profile)

  # Output
  return(dbl_generality)

}


# # [TEST] ------------------------------------------------------------------
# # - Generality test -------------------------------------------------------
# fun_gene_generality(
#   dbl_profile =
#     rnorm(50, 50, 25) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
# )
#
# fun_gene_generality(
#   dbl_profile =
#     rnorm(50, 50, 5) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
# )
#
# fun_gene_generality(
#   dbl_profile =
#     rnorm(50, 50, 0) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
# )