#' ScorzAqol6
#' 
#' A dataset and the required information to implement an AQoL-6D scoring algorithm.
#' 
#' @include C4_ScorzProfile.R
#' @slot a_YouthvarsProfile  (an instance of the YouthvarsProfile class)
#' @slot domain_unwtd_var_nms_chr Domain unweighted variable names (a character vector)
#' @slot domain_wtd_var_nms_chr Domain weighted variable names (a character vector)
#' @slot instrument_dict_r3 Instrument dictionary (a ready4 S3)
#' @slot instrument_nm_1L_chr Instrument name (a character vector of length one)
#' @slot instrument_version_1L_chr Instrument version (a character vector of length one)
#' @slot itm_labels_chr Item labels (a character vector)
#' @slot itm_prefix_1L_chr Item prefix (a character vector of length one)
#' @slot scrg_dss_ls Scoring datasets (a list)
#' @slot total_wtd_var_nm_1L_chr Total weighted variable name (a character vector of length one)
#' @slot total_unwtd_var_nm_1L_chr Total unweighted variable name (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name ScorzAqol6-class
#' @rdname ScorzAqol6-class
#' @export ScorzAqol6
#' @exportClass ScorzAqol6
ScorzAqol6 <- methods::setClass("ScorzAqol6",
contains = "ScorzProfile",
slots = c(a_YouthvarsProfile = "YouthvarsProfile",domain_unwtd_var_nms_chr = "character",domain_wtd_var_nms_chr = "character",instrument_dict_r3 = "ready4use_dictionary",instrument_nm_1L_chr = "character",instrument_version_1L_chr = "character",itm_labels_chr = "character",itm_prefix_1L_chr = "character",scrg_dss_ls = "list",total_wtd_var_nm_1L_chr = "character",total_unwtd_var_nm_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(a_YouthvarsProfile = youthvars::YouthvarsProfile(),domain_unwtd_var_nms_chr = NA_character_,domain_wtd_var_nms_chr = NA_character_,instrument_dict_r3 = ready4use::ready4use_dictionary(),instrument_nm_1L_chr = NA_character_,instrument_version_1L_chr = NA_character_,itm_labels_chr = NA_character_,itm_prefix_1L_chr = NA_character_,scrg_dss_ls = list(list()),total_wtd_var_nm_1L_chr = NA_character_,total_unwtd_var_nm_1L_chr = NA_character_))


methods::setValidity(methods::className("ScorzAqol6"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
