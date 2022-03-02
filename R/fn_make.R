#' Make adolescent Assessment of Quality of Life Six Dimension disvalue lookup table
#' @description make_adol_aqol6d_disv_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make adolescent assessment of quality of life six dimension disvalue lookup table. The function returns Adolescent Assessment of Quality of Life Six Dimension disvalue (a lookup table).
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @return Adolescent Assessment of Quality of Life Six Dimension disvalue (a lookup table)
#' @rdname make_adol_aqol6d_disv_lup
#' @export 
#' @importFrom dplyr mutate case_when
#' @keywords internal
make_adol_aqol6d_disv_lup <- function (aqol6d_scrg_dss_ls = NULL) 
{
    if (is.null(aqol6d_scrg_dss_ls)) 
        aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
    aqol6d_adult_disv_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
    adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>% dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == 
        "Q18" ~ 0.622, TRUE ~ Answer_4_dbl), Answer_5_dbl = dplyr::case_when(Question_chr == 
        "Q3" ~ 0.827, TRUE ~ Answer_5_dbl), Answer_6_dbl = dplyr::case_when(Question_chr == 
        "Q1" ~ 0.073, TRUE ~ Answer_5_dbl))
    return(adol_aqol6d_disv_lup)
}
#' Make Assessment of Quality of Life Six Dimension adolescent pop tibbles list
#' @description make_aqol6d_adol_pop_tbs_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make assessment of quality of life six dimension adolescent pop tibbles list. The function returns Assessment of Quality of Life Six Dimension adolescent pop tibbles (a list).
#' @param aqol_items_prpns_tbs_ls Assessment of Quality of Life items proportions tibbles (a list)
#' @param aqol_scores_pars_ls Assessment of Quality of Life scores pars (a list)
#' @param series_names_chr Series names (a character vector)
#' @param synth_data_spine_ls Synthetic data spine (a list)
#' @param temporal_cors_ls Temporal correlations (a list)
#' @param aqol6d_scrg_dss_ls Assessment of Quality of Life Six Dimension scoring datasets (a list), Default: NULL
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param prefix_chr Prefix (a character vector), Default: c(uid = "Participant_", aqol_item = "aqol6d_q", domain_unwtd_pfx_1L_chr = "aqol6d_subtotal_c_", 
#'    domain_wtd_pfx_1L_chr = "aqol6d_subtotal_w_")
#' @return Assessment of Quality of Life Six Dimension adolescent pop tibbles (a list)
#' @rdname make_aqol6d_adol_pop_tbs_ls
#' @export 
#' @importFrom youthvars make_synth_series_tbs_ls
#' @importFrom purrr map
#' @importFrom dplyr select starts_with everything
#' @importFrom rlang sym
#' @keywords internal
make_aqol6d_adol_pop_tbs_ls <- function (aqol_items_prpns_tbs_ls, aqol_scores_pars_ls, series_names_chr, 
    synth_data_spine_ls, temporal_cors_ls, aqol6d_scrg_dss_ls = NULL, 
    id_var_nm_1L_chr = "fkClientID", prefix_chr = c(uid = "Participant_", 
        aqol_item = "aqol6d_q", domain_unwtd_pfx_1L_chr = "aqol6d_subtotal_c_", 
        domain_wtd_pfx_1L_chr = "aqol6d_subtotal_w_")) 
{
    if (is.null(aqol6d_scrg_dss_ls)) {
        aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
    }
    domain_qs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_domain_qs_lup_tb
    item_pfx_1L_chr <- prefix_chr[["aqol_item"]]
    uid_pfx_1L_chr <- prefix_chr[["uid"]]
    aqol6d_adol_pop_tbs_ls <- youthvars::make_synth_series_tbs_ls(synth_data_spine_ls, 
        series_names_chr = series_names_chr) %>% add_cors_and_utls_to_aqol6d_tbs_ls(aqol_scores_pars_ls = aqol_scores_pars_ls, 
        aqol_items_prpns_tbs_ls = aqol_items_prpns_tbs_ls, temporal_cors_ls = temporal_cors_ls, 
        prefix_chr = prefix_chr, aqol_tots_var_nms_chr = synth_data_spine_ls$aqol_tots_var_nms_chr, 
        aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls, id_var_nm_1L_chr = id_var_nm_1L_chr) %>% 
        purrr::map(~{
            domain_items_ls <- make_domain_items_ls(domain_qs_lup_tb = domain_qs_lup_tb, 
                item_pfx_1L_chr = item_pfx_1L_chr)
            domain_items_ls %>% add_unwtd_dim_tots(items_tb = .x, 
                domain_pfx_1L_chr = prefix_chr[["domain_unwtd_pfx_1L_chr"]]) %>% 
                add_wtd_dim_tots(domain_items_ls = domain_items_ls, 
                  domain_unwtd_pfx_1L_chr = prefix_chr[["domain_unwtd_pfx_1L_chr"]], 
                  domain_wtd_pfx_1L_chr = prefix_chr[["domain_wtd_pfx_1L_chr"]], 
                  aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls) %>% 
                add_labels_to_aqol6d_tb()
        }) %>% purrr::map(~.x %>% dplyr::select(!!rlang::sym(id_var_nm_1L_chr), 
        dplyr::starts_with(item_pfx_1L_chr), dplyr::starts_with(prefix_chr[["domain_unwtd_pfx_1L_chr"]]), 
        dplyr::starts_with(prefix_chr[["domain_wtd_pfx_1L_chr"]]), 
        dplyr::everything()))
    return(aqol6d_adol_pop_tbs_ls)
}
#' Make Assessment of Quality of Life Six Dimension functions list
#' @description make_aqol6d_fns_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make assessment of quality of life six dimension functions list. The function returns Assessment of Quality of Life Six Dimension disu (a list of functions).
#' @param domain_items_ls Domain items (a list)
#' @return Assessment of Quality of Life Six Dimension disu (a list of functions)
#' @rdname make_aqol6d_fns_ls
#' @export 
#' @importFrom purrr map
#' @importFrom rlang sym
#' @keywords internal
make_aqol6d_fns_ls <- function (domain_items_ls) 
{
    aqol6d_disu_fn_ls <- paste0("calculate_aqol6d_dim_", 1:length(domain_items_ls), 
        "_disv") %>% purrr::map(~rlang::sym(.x))
    return(aqol6d_disu_fn_ls)
}
#' Make Assessment of Quality of Life Six Dimension item names
#' @description make_aqol6d_item_nms() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make assessment of quality of life six dimension item names. The function returns Assessment of Quality of Life Six Dimension item names (a character vector).

#' @return Assessment of Quality of Life Six Dimension item names (a character vector)
#' @rdname make_aqol6d_item_nms
#' @export 
#' @keywords internal
make_aqol6d_item_nms <- function () 
{
    aqol6d_item_nms_chr <- c("Household tasks", "Getting around", 
        "Morbility", "Self care", "Enjoy close rel's", "Family rel's", 
        "Community involv't", "Despair", "Worry", "Sad", "Agitated", 
        "Energy level", "Control", "Coping", "Frequency of pain", 
        "Degree of pain", "Pain interference", "Vision", "Hearing", 
        "Communication")
    return(aqol6d_item_nms_chr)
}
#' Make Assessment of Quality of Life Six Dimension items tibble
#' @description make_aqol6d_items_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make assessment of quality of life six dimension items tibble. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol_tb Assessment of Quality of Life (a tibble)
#' @param old_pfx_1L_chr Old prefix (a character vector of length one)
#' @param new_pfx_1L_chr New prefix (a character vector of length one)
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname make_aqol6d_items_tb
#' @export 
#' @importFrom dplyr select starts_with rename_all
#' @importFrom stringr str_replace
#' @keywords internal
make_aqol6d_items_tb <- function (aqol_tb, old_pfx_1L_chr, new_pfx_1L_chr) 
{
    aqol6d_items_tb <- aqol_tb %>% dplyr::select(dplyr::starts_with(old_pfx_1L_chr)) %>% 
        dplyr::rename_all(~{
            stringr::str_replace(., old_pfx_1L_chr, new_pfx_1L_chr)
        })
    return(aqol6d_items_tb)
}
#' Make dimension scaling constants double vector
#' @description make_dim_sclg_cons_dbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make dimension scaling constants double vector. The function returns Dimension scaling constants (a double vector).
#' @param domains_chr Domains (a character vector)
#' @param dim_sclg_con_lup_tb Dimension scaling constant lookup table (a tibble)
#' @return Dimension scaling constants (a double vector)
#' @rdname make_dim_sclg_cons_dbl
#' @export 
#' @importFrom purrr map_dbl
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
make_dim_sclg_cons_dbl <- function (domains_chr, dim_sclg_con_lup_tb) 
{
    dim_sclg_cons_dbl <- purrr::map_dbl(domains_chr, ~ready4::get_from_lup_obj(dim_sclg_con_lup_tb, 
        match_var_nm_1L_chr = "Dimension_chr", match_value_xx = .x, 
        target_var_nm_1L_chr = "Constant_dbl", evaluate_1L_lgl = F))
    return(dim_sclg_cons_dbl)
}
#' Make domain items list
#' @description make_domain_items_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make domain items list. The function returns Domain items (a list).
#' @param domain_qs_lup_tb Domain questions lookup table (a tibble)
#' @param item_pfx_1L_chr Item prefix (a character vector of length one)
#' @return Domain items (a list)
#' @rdname make_domain_items_ls
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr filter pull
#' @importFrom stats setNames
#' @keywords internal
make_domain_items_ls <- function (domain_qs_lup_tb, item_pfx_1L_chr) 
{
    domains_chr <- domain_qs_lup_tb$Domain_chr %>% unique()
    q_nbrs_ls <- purrr::map(domains_chr, ~domain_qs_lup_tb %>% 
        dplyr::filter(Domain_chr == .x) %>% dplyr::pull(Question_dbl))
    domain_items_ls <- purrr::map(q_nbrs_ls, ~paste0(item_pfx_1L_chr, 
        .x)) %>% stats::setNames(domains_chr)
    return(domain_items_ls)
}
#' Make EQ5D dictionary
#' @description make_eq5d_dict() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make eq5d dictionary. The function returns EQ5D dictionary (a ready4 S3).

#' @return EQ5D dictionary (a ready4 S3)
#' @rdname make_eq5d_dict
#' @export 
#' @importFrom ready4use ready4use_dictionary make_pt_ready4use_dictionary
#' @keywords internal
make_eq5d_dict <- function () 
{
    eq5d_dict_r3 <- ready4use::ready4use_dictionary(ready4use::make_pt_ready4use_dictionary(var_nm_chr = paste0("eq5dq_", 
        c("MO", "SC", "UA", "PD", "AD", "total_c", "total_w")), 
        var_ctg_chr = c(rep("multi-attribute utility instrument question", 
            times = 5), "multi-attribute utility instrument unweighted total score", 
            "utility overall score (final weighted)"), var_desc_chr = c(paste0("EuroQol (EQ-5D) - ", 
            c("Mobility", "Self-care", "Usual Activities", "Pain/Discomfort", 
                "Anxiety/Depression"), " item"), "EuroQol (EQ-5D) - (unweighted total)", 
            "EuroQol (EQ-5D) - (weighted total)"), var_type_chr = c(rep("integer", 
            6), "numeric")))
    return(eq5d_dict_r3)
}
