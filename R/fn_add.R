#' Add adolescent CHU-9D
#' @description add_adol_chu9d() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add adolescent chu-9d. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param adol_chu9d_scrg_ds PARAM_DESCRIPTION, Default: make_adol_chu9d_scrg_ds()
#' @param cleanse_1L_chr Cleanse (a character vector of length one), Default: c("all", "none", "dims", "items", "itemdims")
#' @param dim_pfx_1L_chr Dimension prefix (a character vector of length one), Default: 'chu9d_dim'
#' @param item_pfx_1L_chr Item prefix (a character vector of length one), Default: 'chu9d_item'
#' @param scale_by_1L_dbl Scale by (a double vector of length one), Default: 1.1059
#' @param unweighted_1L_chr Unweighted (a character vector of length one), Default: 'chu9d_cml'
#' @param weighted_1L_chr Weighted (a character vector of length one), Default: 'chu9d_utl'
#' @return Dataset (a tibble)
#' @rdname add_adol_chu9d
#' @export 
#' @importFrom dplyr rowwise mutate c_across starts_with ungroup case_when select
#' @importFrom purrr reduce map2_dbl
#' @importFrom stringr str_replace
#' @importFrom rlang sym
#' @importFrom tidyr all_of
#' @keywords internal
add_adol_chu9d <- function (ds_tb, adol_chu9d_scrg_ds = make_adol_chu9d_scrg_ds(), 
    cleanse_1L_chr = c("all", "none", "dims", "items", "itemdims"), 
    dim_pfx_1L_chr = "chu9d_dim", item_pfx_1L_chr = "chu9d_item", 
    scale_by_1L_dbl = 1.1059, unweighted_1L_chr = "chu9d_cml", 
    weighted_1L_chr = "chu9d_utl") 
{
    acknowledgement_1L_chr <- "Adapted from the SPSS scoring algorithm by Chen and Ratcliffe 2015"
    cleanse_1L_chr <- match.arg(cleanse_1L_chr)
    ds_tb <- ds_tb %>% dplyr::rowwise() %>% dplyr::mutate(chu9d_missing_items = sum(is.na(dplyr::c_across(dplyr::starts_with(item_pfx_1L_chr))))) %>% 
        dplyr::ungroup()
    ds_tb <- purrr::reduce(names(ds_tb)[names(ds_tb) %>% startsWith(item_pfx_1L_chr)], 
        .init = ds_tb, ~{
            new_nm_1L_chr <- stringr::str_replace(.y, item_pfx_1L_chr, 
                dim_pfx_1L_chr)
            index_1L_int <- stringr::str_replace(.y, item_pfx_1L_chr, 
                "") %>% as.integer()
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(new_nm_1L_chr), 
                !!rlang::sym(.y) %>% purrr::map2_dbl(chu9d_missing_items, 
                  ~ifelse(.y > 0, NA_integer_, adol_chu9d_scrg_ds[adol_chu9d_scrg_ds$Response == 
                    .x, index_1L_int + 1][[1]]))))
        })
    ds_tb <- ds_tb %>% dplyr::rowwise() %>% dplyr::mutate(`:=`(!!rlang::sym(unweighted_1L_chr), 
        sum(dplyr::c_across(dplyr::starts_with(item_pfx_1L_chr))))) %>% 
        dplyr::ungroup() %>% dplyr::mutate(`:=`(!!rlang::sym(unweighted_1L_chr), 
        dplyr::case_when(chu9d_missing_items > 0 ~ NA_real_, 
            T ~ !!rlang::sym(unweighted_1L_chr))))
    ds_tb <- ds_tb %>% dplyr::rowwise() %>% dplyr::mutate(chu9d_disval = sum(dplyr::c_across(dplyr::starts_with(dim_pfx_1L_chr)))) %>% 
        dplyr::ungroup()
    ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(weighted_1L_chr), 
        1 - (scale_by_1L_dbl * (1 - chu9d_disval))))
    if (cleanse_1L_chr != "none") {
        cleanse_chr <- c("chu9d_missing_items", "chu9d_disval")
        if (cleanse_1L_chr %in% c("all", "items", "itemdims")) {
            cleanse_chr <- c(cleanse_chr, names(ds_tb)[names(ds_tb) %>% 
                startsWith(item_pfx_1L_chr)])
        }
        if (cleanse_1L_chr %in% c("all", "dims", "itemdims")) {
            cleanse_chr <- c(cleanse_chr, names(ds_tb)[names(ds_tb) %>% 
                startsWith(dim_pfx_1L_chr)])
        }
        if (cleanse_1L_chr %in% c("all")) {
            cleanse_chr <- c(cleanse_chr, unweighted_1L_chr)
        }
        ds_tb <- ds_tb %>% dplyr::select(-tidyr::all_of(cleanse_chr))
    }
    return(ds_tb)
}
#' Add Assessment of Quality of Life Six Dimension (Adolescent version) scores
#' @description add_adol6d_scores() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add assessment of quality of life six dimension (adolescent version) scores. The function returns Transformed Assessment of Quality of Life (a tibble).
#' @param unscored_aqol_tb Unscored Assessment of Quality of Life (a tibble)
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param total_aqol_var_nm_1L_chr Total Assessment of Quality of Life variable name (a character vector of length one), Default: 'aqol6d_total_c'
#' @param wtd_aqol_var_nm_1L_chr Weighted Assessment of Quality of Life variable name (a character vector of length one), Default: 'aqol6d_total_w'
#' @return Transformed Assessment of Quality of Life (a tibble)
#' @rdname add_adol6d_scores
#' @export 
#' @importFrom dplyr select starts_with rename_all rename group_by mutate n ungroup arrange inner_join across filter
#' @importFrom tidyselect all_of
#' @importFrom stringr str_replace
#' @importFrom tibble as_tibble
#' @importFrom rlang sym
#' @importFrom purrr map
#' @importFrom Hmisc `label<-` label
#' @keywords internal
add_adol6d_scores <- function (unscored_aqol_tb, aqol6d_scrg_dss_ls = NULL, id_var_nm_1L_chr = "fkClientID", 
    prefix_1L_chr = "aqol6d_q", total_aqol_var_nm_1L_chr = "aqol6d_total_c", 
    wtd_aqol_var_nm_1L_chr = "aqol6d_total_w") 
{
    if (is.null(aqol6d_scrg_dss_ls)) {
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    }
    complete_ds_tb <- unscored_aqol_tb
    unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::select(tidyselect::all_of(id_var_nm_1L_chr), 
        dplyr::starts_with(unname(prefix_1L_chr)))
    old_nms_chr <- names(unscored_aqol_tb)
    names(unscored_aqol_tb) <- c("ID", paste0("Q", 1:20))
    unscored_aqol_tb <- suppressWarnings(impute_unscrd_adol_aqol6d_ds(unscored_aqol_tb))
    disvals_tb <- unscored_aqol_tb %>% add_itm_disv_to_aqol6d_itms_tb(disvalues_lup_tb = make_adol_aqol6d_disv_lup(aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls), 
        pfx_1L_chr = "Q") %>% dplyr::select(ID, dplyr::starts_with("dv_")) %>% 
        dplyr::rename_all(~stringr::str_replace(.x, "dv_", "dv"))
    scored_aqol_tb <- add_aqol6d_adol_dim_scrg_eqs(disvals_tb, 
        aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls) %>% tibble::as_tibble() %>% 
        dplyr::rename(`:=`(!!rlang::sym(id_var_nm_1L_chr), ID), 
            `:=`(!!rlang::sym(wtd_aqol_var_nm_1L_chr), uaqol))
    tbs_ls <- list(complete_ds_tb, scored_aqol_tb) %>% purrr::map(~.x %>% 
        dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>% dplyr::mutate(match_var_chr = paste0(!!rlang::sym(id_var_nm_1L_chr), 
        "_", 1:dplyr::n())) %>% dplyr::ungroup() %>% dplyr::arrange(!!rlang::sym(id_var_nm_1L_chr)))
    if ("labelled" %in% class(tbs_ls[[1]][[wtd_aqol_var_nm_1L_chr]])) {
        tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]] <- Hmisc::`label<-`(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]], 
            value = Hmisc::label(tbs_ls[[1]][[wtd_aqol_var_nm_1L_chr]]))
    }
    else {
        if ("labelled" %in% class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]])) {
            class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]]) <- setdiff(class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]]), 
                "labelled")
            attr(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]], "label") <- NULL
        }
    }
    tfd_aqol_tb <- dplyr::inner_join(tbs_ls[[1]], tbs_ls[[2]]) %>% 
        dplyr::select(-match_var_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(total_aqol_var_nm_1L_chr), 
        rowSums(dplyr::across(dplyr::starts_with(prefix_1L_chr)))))
    tfd_aqol_tb <- tfd_aqol_tb %>% dplyr::filter(!is.na(!!rlang::sym(total_aqol_var_nm_1L_chr)))
    return(tfd_aqol_tb)
}
#' Add AQoL-4D scores
#' @description add_aqol4d_scores() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add aqol-4d scores. The function returns Object (an output object of multiple potential types).
#' @param object_xx Object (an output object of multiple potential types)
#' @param country_1L_chr Country (a character vector of length one), Default: 'NA'
#' @param itm_labels_chr Item labels (a character vector), Default: character(0)
#' @param itm_prefix_1L_chr Item prefix (a character vector of length one), Default: 'aqol4d_q'
#' @param keep_all_1L_lgl Keep all (a logical vector of length one), Default: F
#' @param scrg_dss_ls Scoring datasets (a list), Default: NULL
#' @param tot_unwtd_var_nm_1L_chr Total unweighted variable name (a character vector of length one), Default: 'aqol4d_unwtd_dbl'
#' @param utl_var_nm_1L_chr Utility variable name (a character vector of length one), Default: 'aqol4d_utl_dbl'
#' @return Object (an output object of multiple potential types)
#' @rdname add_aqol4d_scores
#' @export 
#' @importFrom purrr map reduce pmap_int map_lgl pmap_dbl map_dbl map2_dbl pmap_lgl map2_lgl
#' @importFrom ready4 get_from_lup_obj
#' @importFrom stats setNames
#' @importFrom dplyr mutate select across
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
#' @importFrom assertthat assert_that
#' @keywords internal
add_aqol4d_scores <- function (object_xx, country_1L_chr = NA_character_, itm_labels_chr = character(0), 
    itm_prefix_1L_chr = "aqol4d_q", keep_all_1L_lgl = F, scrg_dss_ls = NULL, 
    tot_unwtd_var_nm_1L_chr = "aqol4d_unwtd_dbl", utl_var_nm_1L_chr = "aqol4d_utl_dbl") 
{
    if (is.null(scrg_dss_ls)) {
        scrg_dss_ls <- make_aqol4d_scrg_dss_ls()
    }
    if (identical(itm_labels_chr, character(0))) {
        itm_labels_chr = c("Self-care", "Household tasks", "Mobility", 
            "Rel's - quality", "Rel's - quantity", "Relationships - role", 
            "Vision", "Hearing", "Communication", "Sleep", "Affect", 
            "Pain")
    }
    if (is.data.frame(object_xx)) {
        ds_tb <- object_xx
        columns_chr <- names(ds_tb)
        domains_chr <- scrg_dss_ls$domain_qs_lup$Domain_chr %>% 
            unique()
        domains_ls <- domains_chr %>% purrr::map(~ready4::get_from_lup_obj(scrg_dss_ls$domain_qs_lup, 
            match_var_nm_1L_chr = "Domain_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "Question_int")) %>% stats::setNames(domains_chr)
        ds_tb <- purrr::reduce(1:4, .init = ds_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("aqol4d_mssng_", 
            names(domains_ls)[.y], "_int")), apply(X = is.na(.x %>% 
            dplyr::select(paste0(itm_prefix_1L_chr, domains_ls[[.y]]))), 
            MARGIN = 1, FUN = sum))) %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("aqol4d_impt_", 
            names(domains_ls)[.y], "_int")), .x %>% dplyr::select(paste0(itm_prefix_1L_chr, 
            domains_ls[[.y]])) %>% purrr::pmap_int(~ifelse(any(c(..1, 
            ..2, ..3) %>% purrr::map_lgl(~is.na(.x))), mean(c(..1, 
            ..2, ..3), na.rm = T) %>% round %>% as.integer, NA_integer_)))) %>% 
            dplyr::mutate(dplyr::across(paste0(itm_prefix_1L_chr, 
                domains_ls[[.y]]), ~list(.x, !!rlang::sym(paste0("aqol4d_mssng_", 
                names(domains_ls)[.y], "_int")), !!rlang::sym(paste0("aqol4d_impt_", 
                names(domains_ls)[.y], "_int"))) %>% purrr::pmap_int(~ifelse(is.na(..1) & 
                ..2 < 2, ..3, ..1)))))
        ds_tb <- purrr::reduce(1:4, .init = ds_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("aqol4d_unwtd_", 
            names(domains_ls)[.y], "_dbl")), .x %>% dplyr::select(paste0(itm_prefix_1L_chr, 
            domains_ls[[.y]])) %>% purrr::pmap_dbl(~sum(..1, 
            ..2, ..3)))))
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(tot_unwtd_var_nm_1L_chr), 
            dplyr::select(ds_tb, paste0("aqol4d_unwtd_", names(domains_ls), 
                "_dbl")) %>% purrr::pmap_dbl(~sum(..1, ..2, ..3, 
                ..4))))
        ds_tb <- purrr::reduce(1:4, .init = ds_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("aqol4d_unwtd_", 
            names(domains_ls)[.y], "_dbl")), !!rlang::sym(paste0("aqol4d_unwtd_", 
            names(domains_ls)[.y], "_dbl")) %>% purrr::map_dbl(~(1 - 
            (.x - 3)/(12 - 3)) * 100))))
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(tot_unwtd_var_nm_1L_chr), 
            !!rlang::sym(tot_unwtd_var_nm_1L_chr) %>% purrr::map_dbl(~(1 - 
                (.x - 12)/(48 - 12)) * 100)))
        ds_tb <- purrr::reduce(1:12, .init = ds_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("aqol4d_disv_q", 
            .y, "_dbl")), !!rlang::sym(paste0(itm_prefix_1L_chr, 
            .y)) %>% purrr::map2_dbl(.y, ~ifelse(is.na(.x), NA_real_, 
            ifelse(!.x %in% 1L:4L, NA_real_, ready4::get_from_lup_obj(scrg_dss_ls$item_disvalue_lup, 
                match_var_nm_1L_chr = "Question_chr", match_value_xx = paste0("Q", 
                  .y), target_var_nm_1L_chr = paste0("Answer_", 
                  .x, "_dbl"))))))))
        ds_tb <- purrr::reduce(domains_chr, .init = ds_tb, ~.x %>% 
            dplyr::mutate(`:=`(!!rlang::sym(paste0("aqol4d_disv_", 
                .y, "_dbl")), calculate_aqol4d_dim_disv(.y, ds_tb = .x, 
                scrg_dss_ls = scrg_dss_ls))))
        ds_tb <- purrr::reduce(domains_chr, .init = ds_tb, ~.x %>% 
            dplyr::mutate(`:=`(!!rlang::sym(paste0("aqol4d_dim_", 
                .y, "_dbl")), !!rlang::sym(paste0("aqol4d_disv_", 
                .y, "_dbl")) %>% purrr::map_dbl(~1 - .x))))
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(utl_var_nm_1L_chr), 
            ds_tb %>% dplyr::select(paste0("aqol4d_disv_", domains_chr, 
                "_dbl")) %>% purrr::pmap_dbl(~(scrg_dss_ls$params_lup$Value_dbl[1] * 
                ((1 - (scrg_dss_ls$params_lup$Value_dbl[2] * 
                  ..1)) * (1 - (scrg_dss_ls$params_lup$Value_dbl[3] * 
                  ..2)) * (1 - (scrg_dss_ls$params_lup$Value_dbl[4] * 
                  ..3)) * (1 - (scrg_dss_ls$params_lup$Value_dbl[5] * 
                  ..4)))) + scrg_dss_ls$params_lup$Value_dbl[6])))
        new_columns_chr <- setdiff(names(ds_tb), columns_chr)
        ds_tb <- ds_tb %>% dplyr::mutate(aqol4d_imputed_lgl = ds_tb %>% 
            dplyr::select(new_columns_chr[new_columns_chr %>% 
                startsWith("aqol4d_impt_")]) %>% purrr::pmap_lgl(~c(..1, 
            ..2, ..3, ..4) %>% purrr::map_lgl(~!is.na(.x)) %>% 
            any()))
        ds_tb <- ds_tb %>% dplyr::mutate(aqol4d_complete_lgl = !!rlang::sym(utl_var_nm_1L_chr) %>% 
            purrr::map2_lgl(aqol4d_imputed_lgl, ~!is.na(.x) && 
                !.y))
        new_columns_chr <- c(new_columns_chr, "aqol4d_imputed_lgl", 
            "aqol4d_complete_lgl")
        if (!keep_all_1L_lgl) {
            keep_chr <- c(tot_unwtd_var_nm_1L_chr, utl_var_nm_1L_chr, 
                paste0("aqol4d_unwtd_", domains_chr, "_dbl"), 
                paste0("aqol4d_dim_", domains_chr, "_dbl"), "aqol4d_imputed_lgl", 
                "aqol4d_complete_lgl")
            drop_chr <- setdiff(new_columns_chr, keep_chr)
            ds_tb <- ds_tb %>% dplyr::select(-tidyselect::all_of(drop_chr))
        }
        object_xx <- ds_tb
    }
    else {
        assertthat::assert_that(inherits(object_xx, "YouthvarsProfile"), 
            msg = "object_xx must be either a data.frame or a Youthvars_Profile ready4 module")
        Y <- object_xx
        Z <- ScorzProfile(a_YouthvarsProfile = Y, country_1L_chr = country_1L_chr, 
            domain_unwtd_var_nms_chr = c("aqol4d_unwtd_IL_dbl", 
                "aqol4d_unwtd_RL_dbl", "aqol4d_unwtd_SN_dbl", 
                "aqol4d_unwtd_MH_dbl"), domain_wtd_var_nms_chr = paste0("aqol4d_dim_", 
                make_aqol4d_domain_qs_lup()$Domain_chr %>% unique(), 
                "_dbl"), instrument_dict_r3 = make_aqol4d_dict(), 
            instrument_nm_1L_chr = "Assessment of Quality of Life (4 Dimension)", 
            instrument_short_nm_1L_chr = "AQoL-4D", itm_labels_chr = itm_labels_chr, 
            itm_prefix_1L_chr = itm_prefix_1L_chr, scrg_dss_ls = scrg_dss_ls, 
            total_wtd_var_nm_1L_chr = utl_var_nm_1L_chr, total_unwtd_var_nm_1L_chr = tot_unwtd_var_nm_1L_chr)
        Z <- renew(Z, scoring_fn = add_aqol4d_scores, scorz_args_ls = list(keep_all_1L_lgl = keep_all_1L_lgl, 
            itm_prefix_1L_chr = Z@itm_prefix_1L_chr, scrg_dss_ls = Z@scrg_dss_ls, 
            tot_unwtd_var_nm_1L_chr = Z@total_unwtd_var_nm_1L_chr, 
            utl_var_nm_1L_chr = Z@total_wtd_var_nm_1L_chr), type_1L_chr = "score-w")
        object_xx <- Z
    }
    return(object_xx)
}
#' Add Assessment of Quality of Life Six Dimension adolescent dimension scoring equations
#' @description add_aqol6d_adol_dim_scrg_eqs() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add assessment of quality of life six dimension adolescent dimension scoring equations. The function returns Unscored Assessment of Quality of Life (a tibble).
#' @param unscored_aqol_tb Unscored Assessment of Quality of Life (a tibble)
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @return Unscored Assessment of Quality of Life (a tibble)
#' @rdname add_aqol6d_adol_dim_scrg_eqs
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang parse_expr
#' @importFrom Hmisc label
#' @keywords internal
add_aqol6d_adol_dim_scrg_eqs <- function (unscored_aqol_tb, aqol6d_scrg_dss_ls = NULL) 
{
    if (is.null(aqol6d_scrg_dss_ls)) {
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    }
    adol_dim_sclg_eqs_lup <- aqol6d_scrg_dss_ls$adol_dim_sclg_eqs_lup
    for (var in adol_dim_sclg_eqs_lup$Dim_scal) {
        expression = adol_dim_sclg_eqs_lup[adol_dim_sclg_eqs_lup$Dim_scal == 
            var, ]$Equ
        unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::mutate(`:=`(!!var, 
            !!rlang::parse_expr(expression)))
        Hmisc::label(unscored_aqol_tb[, var]) = adol_dim_sclg_eqs_lup[adol_dim_sclg_eqs_lup$Dim_scal == 
            var, ]$Label
    }
    return(unscored_aqol_tb)
}
#' Add Assessment of Quality of Life Six Dimension items to Assessment of Quality of Life Six Dimension tibbles list
#' @description add_aqol6d_items_to_aqol6d_tbs_ls() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add assessment of quality of life six dimension items to assessment of quality of life six dimension tibbles list. The function returns Updated Assessment of Quality of Life Six Dimension tibbles (a list).
#' @param aqol6d_tbs_ls Assessment of Quality of Life Six Dimension tibbles (a list)
#' @param aqol_items_prpns_tbs_ls Assessment of Quality of Life items proportions tibbles (a list)
#' @param prefix_chr Prefix (a character vector)
#' @param aqol_tots_var_nms_chr Assessment of Quality of Life totals variable names (a character vector)
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param scaling_con_dbl Scaling constant (a double vector), Default: 5
#' @return Updated Assessment of Quality of Life Six Dimension tibbles (a list)
#' @rdname add_aqol6d_items_to_aqol6d_tbs_ls
#' @export 
#' @importFrom purrr map2 map reduce map_int
#' @importFrom dplyr select mutate arrange left_join starts_with everything
#' @importFrom stats na.omit
#' @importFrom simstudy defData genData
#' @importFrom rlang sym
#' @importFrom tibble rowid_to_column
#' @keywords internal
add_aqol6d_items_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol_items_prpns_tbs_ls, prefix_chr, 
    aqol_tots_var_nms_chr, aqol6d_scrg_dss_ls = NULL, id_var_nm_1L_chr = "fkClientID", 
    scaling_con_dbl = 5) 
{
    if (is.null(aqol6d_scrg_dss_ls)) {
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    }
    updated_aqol6d_tbs_ls <- purrr::map2(aqol6d_tbs_ls, aqol_items_prpns_tbs_ls, 
        ~{
            nbr_obs_1L_int <- nrow(.x) * scaling_con_dbl
            transposed_items_props_tb <- .y %>% dplyr::select(-Question) %>% 
                t()
            item_ranges_dbl_ls <- 1:ncol(transposed_items_props_tb) %>% 
                purrr::map(~c(1, length(transposed_items_props_tb[, 
                  .x] %>% stats::na.omit())))
            cat_probs_def_tbl <- purrr::reduce(1:ncol(transposed_items_props_tb), 
                .init = NULL, ~simstudy::defData(.x, varname = paste0("aqol6d_q", 
                  .y), formula = transposed_items_props_tb[, 
                  .y] %>% stats::na.omit() %>% as.vector() %>% 
                  format(digits = 10) %>% paste0(collapse = ";"), 
                  dist = "categorical"))
            items_tb <- simstudy::genData(nbr_obs_1L_int, cat_probs_def_tbl) %>% 
                dplyr::select(-id) %>% dplyr::mutate(`:=`(!!rlang::sym(unname(aqol_tots_var_nms_chr["cumulative"])), 
                rowSums(., na.rm = T))) %>% dplyr::arrange(!!rlang::sym(unname(aqol_tots_var_nms_chr["cumulative"]))) %>% 
                tibble::rowid_to_column("id")
            items_tb <- items_tb %>% dplyr::mutate(aqol6dU = calculate_adol_aqol6dU(items_tb, 
                aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls, prefix_1L_chr = prefix_chr["aqol_item"] %>% 
                  unname(), id_var_nm_1L_chr = "id"))
            .x <- .x %>% dplyr::mutate(id = purrr::map_int(aqol6d_total_w, 
                ~which.min(abs(items_tb$aqol6dU - .x)))) %>% 
                dplyr::left_join(items_tb)
            updated_tb <- .x %>% dplyr::mutate(`:=`(!!rlang::sym(unname(aqol_tots_var_nms_chr["weighted"])), 
                aqol6dU)) %>% dplyr::select(-aqol6dU, -id) %>% 
                dplyr::select(!!rlang::sym(id_var_nm_1L_chr), 
                  dplyr::starts_with(prefix_chr[["aqol_item"]]), 
                  !!rlang::sym(unname(aqol_tots_var_nms_chr["cumulative"])), 
                  !!rlang::sym(unname(aqol_tots_var_nms_chr["weighted"])), 
                  dplyr::everything())
            updated_tb
        })
    return(updated_aqol6d_tbs_ls)
}
#' Add Assessment of Quality of Life Six Dimension Health Utility to Assessment of Quality of Life Six Dimension items tibble
#' @description add_aqol6dU_to_aqol6d_items_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add assessment of quality of life six dimension health utility to assessment of quality of life six dimension items tibble. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param coefs_lup_tb Coefficients lookup table (a tibble), Default: NULL
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_aqol6dU_to_aqol6d_items_tb
#' @export 
#' @importFrom dplyr pull mutate
#' @importFrom purrr map_dbl
#' @keywords internal
add_aqol6dU_to_aqol6d_items_tb <- function (aqol6d_items_tb, coefs_lup_tb = NULL) 
{
    if (is.null(coefs_lup_tb)) {
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
        coefs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_from_8d_coefs_lup_tb
    }
    coef_dbl <- coefs_lup_tb[match(c(paste0("vD", 1:6), "Constant"), 
        coefs_lup_tb$var_name_chr), ] %>% dplyr::pull(coef_dbl)
    aqol6d_items_tb <- aqol6d_items_tb %>% dplyr::mutate(aqol6dU = coef_dbl[1] * 
        vD1 + coef_dbl[2] * vD2 + coef_dbl[3] * vD3 + coef_dbl[4] * 
        vD4 + coef_dbl[5] * vD5 + coef_dbl[6] * vD6 + coef_dbl[7]) %>% 
        dplyr::mutate(aqol6dU = aqol6dU %>% purrr::map_dbl(~ifelse(.x > 
            1, 1, .x)))
    return(aqol6d_items_tb)
}
#' Add Assessment of Quality of Life Six Dimension Health Utility to Assessment of Quality of Life Six Dimension tibbles list
#' @description add_aqol6dU_to_aqol6d_tbs_ls() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add assessment of quality of life six dimension health utility to assessment of quality of life six dimension tibbles list. The function returns Assessment of Quality of Life Six Dimension tibbles (a list).
#' @param aqol6d_tbs_ls Assessment of Quality of Life Six Dimension tibbles (a list)
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one)
#' @return Assessment of Quality of Life Six Dimension tibbles (a list)
#' @rdname add_aqol6dU_to_aqol6d_tbs_ls
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @keywords internal
add_aqol6dU_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol6d_scrg_dss_ls = NULL, prefix_1L_chr = "aqol6d_q", 
    id_var_nm_1L_chr) 
{
    if (is.null(aqol6d_scrg_dss_ls)) {
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    }
    aqol6d_tbs_ls <- aqol6d_tbs_ls %>% purrr::map(~.x %>% dplyr::mutate(aqol6dU = calculate_adol_aqol6dU(.x, 
        aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls, prefix_1L_chr = prefix_1L_chr, 
        id_var_nm_1L_chr = id_var_nm_1L_chr)))
    return(aqol6d_tbs_ls)
}
#' Add correlations and utilities to Assessment of Quality of Life Six Dimension tibbles list
#' @description add_cors_and_utls_to_aqol6d_tbs_ls() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add correlations and utilities to assessment of quality of life six dimension tibbles list. The function returns Assessment of Quality of Life Six Dimension tibbles (a list).
#' @param aqol6d_tbs_ls Assessment of Quality of Life Six Dimension tibbles (a list)
#' @param aqol_scores_pars_ls Assessment of Quality of Life scores pars (a list)
#' @param aqol_items_prpns_tbs_ls Assessment of Quality of Life items proportions tibbles (a list)
#' @param temporal_cors_ls Temporal correlations (a list)
#' @param prefix_chr Prefix (a character vector)
#' @param aqol_tots_var_nms_chr Assessment of Quality of Life totals variable names (a character vector)
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @return Assessment of Quality of Life Six Dimension tibbles (a list)
#' @rdname add_cors_and_utls_to_aqol6d_tbs_ls
#' @export 
#' @importFrom youthvars reorder_tbs_for_target_cors add_uids_to_tbs_ls
#' @keywords internal
add_cors_and_utls_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol_scores_pars_ls, aqol_items_prpns_tbs_ls, 
    temporal_cors_ls, prefix_chr, aqol_tots_var_nms_chr, aqol6d_scrg_dss_ls = NULL, 
    id_var_nm_1L_chr = "fkClientID") 
{
    if (is.null(aqol6d_scrg_dss_ls)) {
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    }
    aqol6d_tbs_ls <- youthvars::reorder_tbs_for_target_cors(aqol6d_tbs_ls, 
        cor_dbl = temporal_cors_ls[[1]], cor_var_chr = rep(names(temporal_cors_ls)[1], 
            2), id_var_to_rmv_1L_chr = "id") %>% youthvars::add_uids_to_tbs_ls(prefix_1L_chr = prefix_chr[["uid"]], 
        id_var_nm_1L_chr = id_var_nm_1L_chr)
    aqol6d_tbs_ls <- aqol6d_tbs_ls %>% add_aqol6d_items_to_aqol6d_tbs_ls(aqol_items_prpns_tbs_ls = aqol_items_prpns_tbs_ls, 
        aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls, prefix_chr = prefix_chr, 
        aqol_tots_var_nms_chr = aqol_tots_var_nms_chr, id_var_nm_1L_chr = id_var_nm_1L_chr)
    return(aqol6d_tbs_ls)
}
#' Add dimension disvalue to Assessment of Quality of Life Six Dimension items tibble
#' @description add_dim_disv_to_aqol6d_items_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add dimension disvalue to assessment of quality of life six dimension items tibble. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domains_chr Domains (a character vector)
#' @param dim_sclg_con_lup_tb Dimension scaling constant lookup table (a tibble), Default: NULL
#' @param itm_wrst_wts_lup_tb Item worst weights lookup table (a tibble), Default: NULL
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_dim_disv_to_aqol6d_items_tb
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr select mutate
#' @importFrom rlang sym exec
#' @keywords internal
add_dim_disv_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls, domains_chr, dim_sclg_con_lup_tb = NULL, 
    itm_wrst_wts_lup_tb = NULL) 
{
    if (is.null(dim_sclg_con_lup_tb) | is.null(itm_wrst_wts_lup_tb)) 
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    if (is.null(dim_sclg_con_lup_tb)) {
        dim_sclg_con_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_dim_sclg_con_lup_tb
    }
    if (is.null(itm_wrst_wts_lup_tb)) {
        itm_wrst_wts_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_itm_wrst_wts_lup_tb
    }
    aqol6d_disu_fn_ls <- make_aqol6d_fns_ls(domain_items_ls)
    kD_dbl <- make_dim_sclg_cons_dbl(domains_chr = domains_chr, 
        dim_sclg_con_lup_tb = dim_sclg_con_lup_tb)
    w_dbl_ls <- make_make_item_wrst_wts_ls_ls(domain_items_ls = domain_items_ls, 
        itm_wrst_wts_lup_tb = itm_wrst_wts_lup_tb)
    aqol6d_items_tb <- purrr::reduce(1:length(domain_items_ls), 
        .init = aqol6d_items_tb, ~{
            args_ls <- list(dvQs_tb = .x %>% dplyr::select(domain_items_ls[[.y]] %>% 
                paste0("dv_", .)), kD_1L_dbl = kD_dbl[.y], w_dbl = w_dbl_ls[[.y]])
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("dvD", 
                .y)), rlang::exec(aqol6d_disu_fn_ls[[.y]], !!!args_ls)))
        })
    return(aqol6d_items_tb)
}
#' Add dimension scores to Assessment of Quality of Life Six Dimension items tibble
#' @description add_dim_scores_to_aqol6d_items_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add dimension scores to assessment of quality of life six dimension items tibble. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_dim_scores_to_aqol6d_items_tb
#' @export 
#' @importFrom dplyr mutate across rename_with
#' @importFrom stringr str_replace
#' @keywords internal
add_dim_scores_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls) 
{
    aqol6d_items_tb <- aqol6d_items_tb %>% dplyr::mutate(dplyr::across(paste0("dvD", 
        1:length(domain_items_ls)), .fns = list(vD = ~1 - .x), 
        .names = "{fn}_{col}")) %>% dplyr::rename_with(~stringr::str_replace(., 
        "vD_dvD", "vD"))
    return(aqol6d_items_tb)
}
#' Add item totals
#' @description add_item_totals() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add item totals. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param domains_ls Domains (a list), Default: NULL
#' @param domains_prefix_1L_chr Domains prefix (a character vector of length one), Default: character(0)
#' @param domain_tfmn_fn Domain transformation (a function), Default: identity
#' @param items_prefix_1L_chr Items prefix (a character vector of length one)
#' @param total_var_nm_1L_chr Total variable name (a character vector of length one)
#' @param total_tfmn_fn Total transformation (a function), Default: identity
#' @param type_fn Type (a function), Default: as.integer
#' @return Dataset (a tibble)
#' @rdname add_item_totals
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
#' @keywords internal
add_item_totals <- function (ds_tb, domains_ls = NULL, domains_prefix_1L_chr = character(0), 
    domain_tfmn_fn = identity, items_prefix_1L_chr, total_var_nm_1L_chr, 
    total_tfmn_fn = identity, type_fn = as.integer) 
{
    vars_to_total_chr <- names(ds_tb)[names(ds_tb) %>% startsWith(items_prefix_1L_chr)]
    if (!is.null(domains_ls)) {
        suffix_1L_chr <- ifelse(identical(type_fn, as.integer), 
            "_int", ifelse(identical(type_fn, as.double), "_dbl", 
                "_num"))
        ds_tb <- purrr::reduce(1:length(domains_ls), .init = ds_tb, 
            ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(domains_prefix_1L_chr, 
                names(domains_ls)[.y], suffix_1L_chr)), .x %>% 
                dplyr::select(paste0(items_prefix_1L_chr, domains_ls[[.y]])) %>% 
                rowSums() %>% domain_tfmn_fn() %>% type_fn())))
        vars_to_total_chr <- paste0(domains_prefix_1L_chr, names(domains_ls), 
            suffix_1L_chr)
    }
    ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(total_var_nm_1L_chr), 
        ds_tb %>% dplyr::select(tidyselect::all_of(vars_to_total_chr)) %>% 
            rowSums() %>% total_tfmn_fn %>% type_fn()))
    return(ds_tb)
}
#' Add item disvalue to Assessment of Quality of Life Six Dimension items tibble
#' @description add_itm_disv_to_aqol6d_itms_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add item disvalue to assessment of quality of life six dimension items tibble. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param disvalues_lup_tb Disvalues lookup table (a tibble), Default: NULL
#' @param pfx_1L_chr Prefix (a character vector of length one)
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_itm_disv_to_aqol6d_itms_tb
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
#' @keywords internal
add_itm_disv_to_aqol6d_itms_tb <- function (aqol6d_items_tb, disvalues_lup_tb = NULL, pfx_1L_chr) 
{
    if (is.null(disvalues_lup_tb)) {
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
        disvalues_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
    }
    aqol6d_items_tb <- purrr::reduce(1:20, .init = aqol6d_items_tb, 
        ~{
            q_1L_chr <- paste0(pfx_1L_chr, .y)
            disu_dbl <- disvalues_lup_tb[.y, -1] %>% as.numeric()
            .x %>% dplyr::mutate(dplyr::across(tidyselect::all_of(q_1L_chr), 
                .fns = list(dv = ~disu_dbl[.x]), .names = "{fn}_{col}"))
        })
    return(aqol6d_items_tb)
}
#' Add labels to Assessment of Quality of Life Six Dimension tibble
#' @description add_labels_to_aqol6d_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add labels to assessment of quality of life six dimension tibble. The function returns Assessment of Quality of Life Six Dimension (a tibble).
#' @param aqol6d_tb Assessment of Quality of Life Six Dimension (a tibble)
#' @param labels_chr Labels (a character vector), Default: 'NA'
#' @return Assessment of Quality of Life Six Dimension (a tibble)
#' @rdname add_labels_to_aqol6d_tb
#' @export 
#' @importFrom Hmisc label
#' @keywords internal
add_labels_to_aqol6d_tb <- function (aqol6d_tb, labels_chr = NA_character_) 
{
    if (is.na(labels_chr)) 
        labels_chr <- c(fkClientID = "Unique client identifier", 
            round = "Data measurement round", d_age = "Age", 
            d_gender = "Gender", d_sexual_ori_s = "Sexual orientation", 
            d_studying_working = "Work and study", c_p_diag_s = " Primary diagnosis", 
            c_clinical_staging_s = "Clinical stage", c_sofas = "SOFAS", 
            s_centre = "Clinic", d_agegroup = "Age group", d_sex_birth_s = "Sex at birth", 
            d_country_bir_s = "Country of birth", d_ATSI = "Aboriginal and Torres Strait Islander", 
            d_english_home = "English spoken at home", d_english_native = "English is native language", 
            d_relation_s = "Relationship status", aqol6d_total_w = "AQoL health utility", 
            phq9_total = "PHQ9", bads_total = "BADS", gad7_total = "GAD7", 
            oasis_total = "OASIS", scared_total = "SCARED", k6_total = "K6", 
            aqol6d_total_c = "AQoL unweighted total", aqol6d_q1 = "Household tasks", 
            aqol6d_q2 = "Getting around", aqol6d_q3 = "Mobility", 
            aqol6d_q4 = "Self care", aqol6d_q5 = "Enjoy close rels", 
            aqol6d_q6 = "Family rels", aqol6d_q7 = "Community involvement", 
            aqol6d_q8 = "Despair", aqol6d_q9 = "Worry", aqol6d_q10 = "Sad", 
            aqol6d_q11 = "Agitated", aqol6d_q12 = "Energy level", 
            aqol6d_q13 = "Control", aqol6d_q14 = "Coping", aqol6d_q15 = "Frequency of pain", 
            aqol6d_q16 = "Degree of pain", aqol6d_q17 = "Pain interference", 
            aqol6d_q18 = "Vision", aqol6d_q19 = "Hearing", aqol6d_q20 = "Communication", 
            aqol6d_subtotal_c_IL = "Unweighted Independent Living", 
            aqol6d_subtotal_c_REL = "Unweighted Relationships", 
            aqol6d_subtotal_c_MH = "Unweighted Mental Health", 
            aqol6d_subtotal_c_COP = "Unweighted Coping", aqol6d_subtotal_c_P = "Unweighted Pain", 
            aqol6d_subtotal_c_SEN = "Unweighted Sense", aqol6d_subtotal_w_IL = "Independent Living", 
            aqol6d_subtotal_w_REL = "Relationships", aqol6d_subtotal_w_MH = "Mental Health", 
            aqol6d_subtotal_w_COP = "Coping", aqol6d_subtotal_w_P = "Pain", 
            aqol6d_subtotal_w_SEN = "Sense")
    Hmisc::label(aqol6d_tb) = as.list(labels_chr[match(names(aqol6d_tb), 
        names(labels_chr))])
    return(aqol6d_tb)
}
#' Add paid totals
#' @description add_paid_totals() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add paid totals. The function returns Object (an output object of multiple potential types).
#' @param ds_tb Dataset (a tibble)
#' @param ctg_var_nm_1L_chr Category variable name (a character vector of length one), Default: 'PAID_burnout_risk_lgl'
#' @param dict_ctg_1L_chr Dictionary category (a character vector of length one), Default: 'PAID'
#' @param dictionary_r3 Dictionary (a ready4 submodule), Default: ready4use::ready4use_dictionary()
#' @param items_prefix_1L_chr Items prefix (a character vector of length one), Default: 'paid_'
#' @param total_var_nm_1L_chr Total variable name (a character vector of length one), Default: 'PAID_total_dbl'
#' @param what_1L_chr What (a character vector of length one), Default: 'ds'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname add_paid_totals
#' @export 
#' @importFrom ready4use ready4use_dictionary renew.ready4use_dictionary
#' @importFrom dplyr mutate filter pull
#' @importFrom rlang sym
#' @importFrom purrr map_lgl map_chr pluck
#' @keywords internal
add_paid_totals <- function (ds_tb, ctg_var_nm_1L_chr = "PAID_burnout_risk_lgl", 
    dict_ctg_1L_chr = "PAID", dictionary_r3 = ready4use::ready4use_dictionary(), 
    items_prefix_1L_chr = "paid_", total_var_nm_1L_chr = "PAID_total_dbl", 
    what_1L_chr = "ds", ...) 
{
    ds_tb <- ds_tb %>% add_item_totals(items_prefix_1L_chr = items_prefix_1L_chr, 
        total_var_nm_1L_chr = total_var_nm_1L_chr, total_tfmn_fn = function(x) {
            x * 1.25
        }, type_fn = as.double)
    ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(ctg_var_nm_1L_chr), 
        !!rlang::sym(total_var_nm_1L_chr) %>% purrr::map_lgl(~ifelse(is.na(.x), 
            NA, .x >= 40))))
    if (what_1L_chr == "dict") {
        dictionary_r3 <- dplyr::filter(dictionary_r3, startsWith(var_nm_chr, 
            items_prefix_1L_chr))
        object_xx <- dictionary_r3 %>% ready4use::renew.ready4use_dictionary(var_nm_chr = c(total_var_nm_1L_chr, 
            ctg_var_nm_1L_chr), var_ctg_chr = dict_ctg_1L_chr, 
            var_desc_chr = c("PAID total score", "PAID burnout risk"), 
            var_type_chr = c(total_var_nm_1L_chr, ctg_var_nm_1L_chr) %>% 
                purrr::map_chr(~ds_tb %>% dplyr::pull(.x) %>% 
                  class() %>% purrr::pluck(1)))
    }
    else {
        object_xx <- ds_tb
    }
    return(object_xx)
}
#' Add PHQ-4 totals
#' @description add_phq4_totals() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add phq-4 totals. The function returns Object (an output object of multiple potential types).
#' @param ds_tb Dataset (a tibble)
#' @param ctg_var_nm_1L_chr Category variable name (a character vector of length one), Default: 'phq4_ctg_fct'
#' @param dict_ctg_1L_chr Dictionary category (a character vector of length one), Default: 'PHQ-4'
#' @param dictionary_r3 Dictionary (a ready4 submodule), Default: ready4use::ready4use_dictionary()
#' @param domains_ls Domains (a list), Default: list(anxiety = 1:2, depression = 3:4)
#' @param domains_prefix_1L_chr Domains prefix (a character vector of length one), Default: 'phq4_'
#' @param items_prefix_1L_chr Items prefix (a character vector of length one), Default: 'phq_gad_'
#' @param total_var_nm_1L_chr Total variable name (a character vector of length one), Default: 'phq4_total_int'
#' @param what_1L_chr What (a character vector of length one), Default: 'ds'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname add_phq4_totals
#' @export 
#' @importFrom ready4use ready4use_dictionary renew.ready4use_dictionary
#' @importFrom dplyr mutate filter pull
#' @importFrom rlang sym
#' @importFrom purrr map2_chr discard map_chr pluck
#' @keywords internal
add_phq4_totals <- function (ds_tb, ctg_var_nm_1L_chr = "phq4_ctg_fct", dict_ctg_1L_chr = "PHQ-4", 
    dictionary_r3 = ready4use::ready4use_dictionary(), domains_ls = list(anxiety = 1:2, 
        depression = 3:4), domains_prefix_1L_chr = "phq4_", items_prefix_1L_chr = "phq_gad_", 
    total_var_nm_1L_chr = "phq4_total_int", what_1L_chr = "ds", 
    ...) 
{
    ds_tb <- ds_tb %>% add_item_totals(domains_ls = domains_ls, 
        domains_prefix_1L_chr = domains_prefix_1L_chr, items_prefix_1L_chr = items_prefix_1L_chr, 
        total_var_nm_1L_chr = total_var_nm_1L_chr)
    ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(ctg_var_nm_1L_chr), 
        !!rlang::sym(paste0(domains_prefix_1L_chr, names(domains_ls)[1], 
            "_int")) %>% purrr::map2_chr(!!rlang::sym(paste0(domains_prefix_1L_chr, 
            names(domains_ls)[2], "_int")), ~{
            all_chr <- c(ifelse(is.na(.x), NA_character_, ifelse(.x >= 
                3, "Anxiety", "Normal range")), ifelse(is.na(.y), 
                NA_character_, ifelse(.y >= 3, "Depression", 
                  "Normal range"))) %>% unique()
            if (length(all_chr) > 1) {
                all_chr <- setdiff(all_chr %>% purrr::discard(is.na), 
                  "Normal range")
                if (length(all_chr) > 1) {
                  all_chr <- "Anxiety and depression"
                }
            }
            all_chr
        }) %>% as.factor()))
    if (what_1L_chr == "dict") {
        dictionary_r3 <- dplyr::filter(dictionary_r3, startsWith(var_nm_chr, 
            items_prefix_1L_chr))
        object_xx <- dictionary_r3 %>% ready4use::renew.ready4use_dictionary(var_nm_chr = c(paste0(domains_prefix_1L_chr, 
            names(domains_ls), "_int"), total_var_nm_1L_chr, 
            ctg_var_nm_1L_chr), var_ctg_chr = dict_ctg_1L_chr, 
            var_desc_chr = c("PHQ-4 anxiety score", "PHQ-4 depression score", 
                "PHQ-4 total score", "PHQ-4 mental health"), 
            var_type_chr = c(paste0(domains_prefix_1L_chr, names(domains_ls), 
                "_int"), total_var_nm_1L_chr, ctg_var_nm_1L_chr) %>% 
                purrr::map_chr(~ds_tb %>% dplyr::pull(.x) %>% 
                  class() %>% purrr::pluck(1)))
    }
    else {
        object_xx <- ds_tb
    }
    return(object_xx)
}
#' Add scores
#' @description add_scores() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add scores. The function returns Object (an output object of multiple potential types).
#' @param X_YouthvarsProfile PARAM_DESCRIPTION
#' @param scoring_tb Scoring (a tibble)
#' @param label_ds_1L_lgl Label dataset (a logical vector of length one), Default: T
#' @param what_1L_chr What (a character vector of length one), Default: c("merged", "list")
#' @return Object (an output object of multiple potential types)
#' @rdname add_scores
#' @export 
#' @importFrom purrr pmap reduce
#' @importFrom rlang exec
#' @importFrom stats setNames
#' @importFrom dplyr left_join bind_rows distinct filter arrange
#' @keywords internal
add_scores <- function (X_YouthvarsProfile, scoring_tb, label_ds_1L_lgl = T, 
    what_1L_chr = c("merged", "list")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    object_xx <- purrr::pmap(scoring_tb, ~{
        dict_fn <- eval(parse(text = ..10))
        dict_args_ls <- ..11
        scoring_fn <- eval(parse(text = ..5))
        if (identical(dict_args_ls, list())) 
            dict_args_ls <- NULL
        if (identical(dict_fn, scoring_fn)) {
            instrument_dict_r3 <- rlang::exec(dict_fn, Y@a_Ready4useDyad@ds_tb, 
                !!!dict_args_ls)
        }
        else {
            instrument_dict_r3 <- rlang::exec(dict_fn, !!!dict_args_ls)
        }
        scoring_args_ls <- ..6
        if (identical(scoring_args_ls, list())) 
            scoring_args_ls <- NULL
        ScorzProfile(a_YouthvarsProfile = X_YouthvarsProfile, 
            country_1L_chr = ..7, domain_unwtd_var_nms_chr = ..4, 
            domain_wtd_var_nms_chr = ..8, instrument_dict_r3 = instrument_dict_r3, 
            instrument_nm_1L_chr = ..1, instrument_short_nm_1L_chr = ..2, 
            instrument_version_1L_chr = ..15, itm_labels_chr = Y@a_Ready4useDyad@dictionary_r3 %>% 
                get_from_lup_obj(match_value_xx = ..9, match_var_nm_1L_chr = "var_ctg_chr", 
                  target_var_nm_1L_chr = "var_desc_chr"), itm_prefix_1L_chr = ..3, 
            scrg_dss_ls = ..11, total_wtd_var_nm_1L_chr = ..14, 
            total_unwtd_var_nm_1L_chr = ..13) %>% renew(scoring_fn = scoring_fn, 
            scorz_args_ls = scoring_args_ls, label_ds_1L_lgl = label_ds_1L_lgl, 
            type_1L_chr = "score-w")
    }) %>% stats::setNames(scoring_tb$short_name_chr)
    if (what_1L_chr == "merged") {
        Y <- purrr::reduce(object_xx, .init = X_YouthvarsProfile, 
            ~{
                Y_YouthvarsProfile <- .x
                Y_YouthvarsProfile@a_Ready4useDyad@ds_tb <- dplyr::left_join(.x@a_Ready4useDyad@ds_tb, 
                  .y@a_YouthvarsProfile@a_Ready4useDyad@ds_tb)
                Y_YouthvarsProfile@a_Ready4useDyad@dictionary_r3 <- dplyr::bind_rows(.y@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3, 
                  .x@a_Ready4useDyad@dictionary_r3) %>% dplyr::distinct()
                Y_YouthvarsProfile
            })
        Y@a_Ready4useDyad@dictionary_r3 <- Y@a_Ready4useDyad@dictionary_r3 %>% 
            dplyr::filter(!is.na(var_nm_chr)) %>% dplyr::arrange(var_ctg_chr, 
            var_nm_chr)
        object_xx <- Y
    }
    return(object_xx)
}
#' Add unweighted dimension totals
#' @description add_unwtd_dim_tots() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add unweighted dimension totals. The function returns Items and domains (a tibble).
#' @param items_tb Items (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domain_pfx_1L_chr Domain prefix (a character vector of length one)
#' @return Items and domains (a tibble)
#' @rdname add_unwtd_dim_tots
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @keywords internal
add_unwtd_dim_tots <- function (items_tb, domain_items_ls, domain_pfx_1L_chr) 
{
    items_and_domains_tb <- purrr::reduce(1:length(domain_items_ls), 
        .init = items_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(domain_pfx_1L_chr, 
            names(domain_items_ls)[.y])), rowSums(dplyr::select(., 
            domain_items_ls[[.y]])))))
    return(items_and_domains_tb)
}
#' Add weighted dimension totals
#' @description add_wtd_dim_tots() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add weighted dimension totals. The function returns Weighted and unweighted dimension (a tibble).
#' @param unwtd_dim_tb Unweighted dimension (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domain_unwtd_pfx_1L_chr Domain unweighted prefix (a character vector of length one)
#' @param domain_wtd_pfx_1L_chr Domain weighted prefix (a character vector of length one)
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @return Weighted and unweighted dimension (a tibble)
#' @rdname add_wtd_dim_tots
#' @export 
#' @importFrom purrr map_dbl map2_dbl discard reduce
#' @importFrom dplyr filter pull select_if mutate
#' @importFrom rlang sym
#' @keywords internal
add_wtd_dim_tots <- function (unwtd_dim_tb, domain_items_ls, domain_unwtd_pfx_1L_chr, 
    domain_wtd_pfx_1L_chr, aqol6d_scrg_dss_ls = NULL) 
{
    if (is.null(aqol6d_scrg_dss_ls)) 
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    aqol6d_adult_disv_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
    aqol6d_domain_qs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_domain_qs_lup_tb
    min_vals_dbl <- purrr::map_dbl(domain_items_ls, ~length(.x)) %>% 
        unname()
    max_vals_dbl <- purrr::map2_dbl(domain_items_ls, names(domain_items_ls), 
        ~{
            paste0("Q", aqol6d_domain_qs_lup_tb %>% dplyr::filter(Domain_chr == 
                .y) %>% dplyr::pull(Question_dbl)) %>% purrr::map_dbl(~{
                tb <- aqol6d_adult_disv_lup_tb %>% dplyr::filter(Question_chr == 
                  .x) %>% dplyr::select_if(is.numeric)
                as.numeric(as.data.frame(tb)[1, ]) %>% purrr::discard(is.na) %>% 
                  length()
            }) %>% sum()
        }) %>% unname()
    wtd_and_unwtd_dim_tb <- purrr::reduce(1:length(domain_items_ls), 
        .init = unwtd_dim_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(domain_wtd_pfx_1L_chr, 
            names(domain_items_ls)[.y])), (1 - (!!rlang::sym(paste0(domain_unwtd_pfx_1L_chr, 
            names(domain_items_ls)[.y])) - min_vals_dbl[.y])/(max_vals_dbl[.y] - 
            min_vals_dbl[.y])))))
    return(wtd_and_unwtd_dim_tb)
}
