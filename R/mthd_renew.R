#' 
#' Renew (update) values
#' @name renew-ScorzAqol6Adol
#' @description renew method applied to ScorzAqol6Adol
#' @param x An object of class ScorzAqol6Adol
#' @param label_ds_1L_lgl Label dataset (a logical vector of length one), Default: T
#' @param type_1L_chr Type (a character vector of length one), Default: 'score'
#' @return x (An object of class ScorzAqol6Adol)
#' @rdname renew-methods
#' @aliases renew,ScorzAqol6Adol-method
#' @export 
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom ready4use renew.ready4use_dictionary
#' @importFrom ready4 renew
methods::setMethod("renew", "ScorzAqol6Adol", function (x, label_ds_1L_lgl = T, type_1L_chr = "score") 
{
    y <- x@a_YouthvarsProfile@a_Ready4useDyad
    y <- renew(y, type_1L_chr = "unlabel")
    if (type_1L_chr == "score") {
        if (identical(x@scrg_dss_ls, list(list()))) {
            x@scrg_dss_ls <- make_aqol6d_scrg_dss()
        }
        select_chr <- setdiff(names(y@ds_tb), x@instrument_dict_r3$var_nm_chr[!x@instrument_dict_r3$var_nm_chr %>% 
            startsWith(x@itm_prefix_1L_chr)] %>% as.vector())
        y@ds_tb <- y@ds_tb %>% dplyr::select(tidyselect::all_of(select_chr))
        y@ds_tb <- add_adol6d_scores(y@ds_tb, aqol6d_scrg_dss_ls = x@scrg_dss_ls, 
            prefix_1L_chr = x@itm_prefix_1L_chr, id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr, 
            total_aqol_var_nm_1L_chr = x@total_unwtd_var_nm_1L_chr, 
            wtd_aqol_var_nm_1L_chr = x@total_wtd_var_nm_1L_chr)
        y@dictionary_r3 <- ready4use::renew.ready4use_dictionary(y@dictionary_r3, 
            new_cases_r3 = x@instrument_dict_r3)
    }
    if (label_ds_1L_lgl) 
        y <- renew(y)
    if (type_1L_chr == "score") {
        x@a_YouthvarsProfile@a_Ready4useDyad <- y
    }
    return(x)
})
#' 
#' Renew (update) values
#' @name renew-ScorzEuroQol5
#' @description renew method applied to ScorzEuroQol5
#' @param x An object of class ScorzEuroQol5
#' @param label_ds_1L_lgl Label dataset (a logical vector of length one), Default: T
#' @param type_1L_chr Type (a character vector of length one), Default: 'score'
#' @return x (An object of class ScorzEuroQol5)
#' @rdname renew-methods
#' @aliases renew,ScorzEuroQol5-method
#' @export 
#' @importFrom dplyr rename_with mutate across filter
#' @importFrom rlang sym
#' @importFrom eq5d eq5d
#' @importFrom ready4 renew
methods::setMethod("renew", "ScorzEuroQol5", function (x, label_ds_1L_lgl = T, type_1L_chr = "score") 
{
    y <- x@a_YouthvarsProfile@a_Ready4useDyad
    y <- renew(y, type_1L_chr = "unlabel")
    if (type_1L_chr == "score") {
        y@ds_tb <- y@ds_tb %>% dplyr::rename_with(~c("MO", "SC", 
            "UA", "PD", "AD"), x@itm_var_nms_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(x@total_wtd_var_nm_1L_chr), 
            eq5d::eq5d(., country = x@country_1L_chr, version = x@instrument_version_1L_chr, 
                type = x@type_1L_chr))) %>% dplyr::rename_with(~x@itm_var_nms_chr, 
            c("MO", "SC", "UA", "PD", "AD")) %>% dplyr::mutate(`:=`(!!rlang::sym(x@total_unwtd_var_nm_1L_chr), 
            rowSums(dplyr::across(x@itm_var_nms_chr)))) %>% dplyr::filter(!is.na(!!rlang::sym(x@total_unwtd_var_nm_1L_chr)))
        instrument_dict_r3 <- x@instrument_dict_r3
        instrument_dict_r3$var_nm_chr <- c(x@itm_var_nms_chr, 
            x@total_unwtd_var_nm_1L_chr, x@total_wtd_var_nm_1L_chr)
        y@dictionary_r3 <- ready4::renew(y@dictionary_r3, new_cases_r3 = instrument_dict_r3)
    }
    if (label_ds_1L_lgl) 
        y <- renew(y)
    if (type_1L_chr == "score") {
        x@a_YouthvarsProfile@a_Ready4useDyad <- y
    }
    return(x)
})
#' 
#' Renew (update) values
#' @name renew-ScorzProfile
#' @description renew method applied to ScorzProfile
#' @param x An object of class ScorzProfile
#' @param drop_msng_1L_lgl Drop missing (a logical vector of length one), Default: F
#' @param item_type_1L_chr Item type (a character vector of length one), Default: 'numeric'
#' @param scoring_fn Scoring (a function), Default: identity
#' @param scorz_args_ls Scorz arguments (a list), Default: NULL
#' @param label_ds_1L_lgl Label dataset (a logical vector of length one), Default: T
#' @param type_1L_chr Type (a character vector of length one), Default: 'score'
#' @return x (An object of class ScorzProfile)
#' @rdname renew-methods
#' @aliases renew,ScorzProfile-method
#' @export 
#' @importFrom dplyr mutate select starts_with filter
#' @importFrom rlang sym exec
#' @importFrom ready4use renew.ready4use_dictionary
#' @importFrom ready4 renew
methods::setMethod("renew", "ScorzProfile", function (x, drop_msng_1L_lgl = F, item_type_1L_chr = "numeric", 
    scoring_fn = identity, scorz_args_ls = NULL, label_ds_1L_lgl = T, 
    type_1L_chr = "score") 
{
    if (type_1L_chr %in% c("score", "score-c", "score-w")) {
        if (type_1L_chr == "score") {
            x <- renew(x, scoring_fn = scoring_fn, scorz_args_ls = scorz_args_ls, 
                label_ds_1L_lgl = label_ds_1L_lgl, type_1L_chr = "score-w") %>% 
                renew(label_ds_1L_lgl = label_ds_1L_lgl, type_1L_chr = "score-c")
        }
        if (type_1L_chr %in% c("score-c", "score-w")) {
            y <- x@a_YouthvarsProfile@a_Ready4useDyad
            y <- renew(y, type_1L_chr = "unlabel")
        }
        if (type_1L_chr == "score-c") {
            y@ds_tb <- y@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(x@total_unwtd_var_nm_1L_chr), 
                rowSums(dplyr::select(., dplyr::starts_with(x@itm_prefix_1L_chr)))))
            if (drop_msng_1L_lgl) 
                y@ds_tb <- y@ds_tb %>% dplyr::filter(!is.na(!!rlang::sym(x@total_unwtd_var_nm_1L_chr)))
            if (!x@total_unwtd_var_nm_1L_chr %in% x@instrument_dict_r3$var_nm_chr) {
                x@instrument_dict_r3 <- ready4use::renew.ready4use_dictionary(x@instrument_dict_r3, 
                  var_nm_chr = x@total_unwtd_var_nm_1L_chr, var_ctg_chr = "multi-attribute utility instrument unweighted total score", 
                  var_desc_chr = paste0(x@instrument_nm_1L_chr, 
                    " (unweighted total)"), var_type_chr = item_type_1L_chr)
            }
        }
        if (type_1L_chr == "score-w") {
            y@ds_tb <- rlang::exec(scoring_fn, y@ds_tb, !!!scorz_args_ls)
            if (!x@total_wtd_var_nm_1L_chr %in% x@instrument_dict_r3$var_nm_chr) {
                x@instrument_dict_r3 <- ready4use::renew.ready4use_dictionary(x@instrument_dict_r3, 
                  var_nm_chr = x@total_wtd_var_nm_1L_chr, var_ctg_chr = "health utility", 
                  var_desc_chr = paste0(x@instrument_nm_1L_chr, 
                    " total score"), var_type_chr = item_type_1L_chr)
            }
        }
    }
    if (type_1L_chr %in% c("score-c", "score-w")) {
        y@dictionary_r3 <- ready4use::renew.ready4use_dictionary(y@dictionary_r3, 
            new_cases_r3 = x@instrument_dict_r3)
        if (label_ds_1L_lgl) 
            y <- renew(y)
        x@a_YouthvarsProfile@a_Ready4useDyad <- y
    }
    return(x)
})
