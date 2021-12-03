#' ScorzAqol6Adult
#' 
#' A dataset and metadata to support implementation of a scoring algorithm for the adult version of AQoL-6D.
#' 
#' @include C4_ScorzAqol6.R
#' @slot instrument_version_1L_chr Instrument version (a character vector of length one)
#' @slot a_YouthvarsProfile  (an instance of the YouthvarsProfile class)
#' @slot domain_unwtd_var_nms_chr Domain unweighted variable names (a character vector)
#' @slot domain_wtd_var_nms_chr Domain weighted variable names (a character vector)
#' @slot instrument_dict_r3 Instrument dictionary (a ready4 S3)
#' @slot instrument_nm_1L_chr Instrument name (a character vector of length one)
#' @slot itm_labels_chr Item labels (a character vector)
#' @slot itm_prefix_1L_chr Item prefix (a character vector of length one)
#' @slot scrg_dss_ls Scoring datasets (a list)
#' @slot total_wtd_var_nm_1L_chr Total weighted variable name (a character vector of length one)
#' @slot total_unwtd_var_nm_1L_chr Total unweighted variable name (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name ScorzAqol6Adult-class
#' @rdname ScorzAqol6Adult-class
#' @export ScorzAqol6Adult
#' @exportClass ScorzAqol6Adult
ScorzAqol6Adult <- methods::setClass("ScorzAqol6Adult",
contains = "ScorzAqol6",
slots = c(instrument_version_1L_chr = "character",a_YouthvarsProfile = "YouthvarsProfile",domain_unwtd_var_nms_chr = "character",domain_wtd_var_nms_chr = "character",instrument_dict_r3 = "ready4use_dictionary",instrument_nm_1L_chr = "character",itm_labels_chr = "character",itm_prefix_1L_chr = "character",scrg_dss_ls = "list",total_wtd_var_nm_1L_chr = "character",total_unwtd_var_nm_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(instrument_version_1L_chr = 'Adult'))


methods::setValidity(methods::className("ScorzAqol6Adult"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
