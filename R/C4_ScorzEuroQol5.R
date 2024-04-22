#' ScorzEuroQol5
#' 
#' A dataset and metadata to support implementation of an EQ-5D scoring algorithm.
#' 
#' @include C4_ScorzProfile.R fn_make.R
#' @slot a_YouthvarsProfile  (an instance of the YouthvarsProfile class)
#' @slot country_1L_chr Country (a character vector of length one)
#' @slot domain_unwtd_var_nms_chr Domain unweighted variable names (a character vector)
#' @slot domain_wtd_var_nms_chr Domain weighted variable names (a character vector)
#' @slot instrument_dict_r3 Instrument dictionary (a ready4 submodule)
#' @slot instrument_nm_1L_chr Instrument name (a character vector of length one)
#' @slot instrument_short_nm_1L_chr Instrument short name (a character vector of length one)
#' @slot instrument_version_1L_chr Instrument version (a character vector of length one)
#' @slot itm_labels_chr Item labels (a character vector)
#' @slot itm_prefix_1L_chr Item prefix (a character vector of length one)
#' @slot itm_var_nms_chr Item variable names (a character vector)
#' @slot scrg_dss_ls Scoring datasets (a list)
#' @slot total_wtd_var_nm_1L_chr Total weighted variable name (a character vector of length one)
#' @slot total_unwtd_var_nm_1L_chr Total unweighted variable name (a character vector of length one)
#' @slot type_1L_chr Type (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name ScorzEuroQol5-class
#' @rdname ScorzEuroQol5-class
#' @export ScorzEuroQol5
#' @exportClass ScorzEuroQol5
ScorzEuroQol5 <- methods::setClass("ScorzEuroQol5",
contains = "ScorzProfile",
slots = c(a_YouthvarsProfile = "YouthvarsProfile",country_1L_chr = "character",domain_unwtd_var_nms_chr = "character",domain_wtd_var_nms_chr = "character",instrument_dict_r3 = "ready4use_dictionary",instrument_nm_1L_chr = "character",instrument_short_nm_1L_chr = "character",instrument_version_1L_chr = "character",itm_labels_chr = "character",itm_prefix_1L_chr = "character",itm_var_nms_chr = "character",scrg_dss_ls = "list",total_wtd_var_nm_1L_chr = "character",total_unwtd_var_nm_1L_chr = "character",type_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(a_YouthvarsProfile = youthvars::YouthvarsProfile(),country_1L_chr = NA_character_,domain_unwtd_var_nms_chr = NA_character_,domain_wtd_var_nms_chr = c("eq5dq_MO", "eq5dq_SC", "eq5dq_UA", "eq5dq_PD", "eq5dq_AD"),instrument_dict_r3 = make_eq5d_dict(),instrument_nm_1L_chr = 'EuroQol EQ-5D',instrument_short_nm_1L_chr = NA_character_,instrument_version_1L_chr = NA_character_,itm_labels_chr = c("Mobility", "Self-care", "Usual Activities","Pain-Discomfort","Anxiety-Depression"),itm_prefix_1L_chr = 'eq5dq_',itm_var_nms_chr = c("eq5dq_MO", "eq5dq_SC", "eq5dq_UA", "eq5dq_PD", "eq5dq_AD"),scrg_dss_ls = list(list()),total_wtd_var_nm_1L_chr = 'eq5d_total_w',total_unwtd_var_nm_1L_chr = 'eq5d_total_c',type_1L_chr = 'CW'))


methods::setValidity(methods::className("ScorzEuroQol5"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
