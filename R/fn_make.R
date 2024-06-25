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
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
    aqol6d_adult_disv_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
    adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>% dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == 
        "Q18" ~ 0.622, TRUE ~ Answer_4_dbl), Answer_5_dbl = dplyr::case_when(Question_chr == 
        "Q3" ~ 0.827, TRUE ~ Answer_5_dbl), Answer_6_dbl = dplyr::case_when(Question_chr == 
        "Q1" ~ 0.073, TRUE ~ Answer_5_dbl))
    return(adol_aqol6d_disv_lup)
}
#' Make AQoL-4D dictionary
#' @description make_aqol4d_dict() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aqol-4d dictionary. The function is called for its side effects and does not return a value.
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol4d_q'
#' @param scrg_dss_ls Scoring datasets (a list), Default: NULL
#' @param tot_unwtd_var_nm_1L_chr Total unweighted variable name (a character vector of length one), Default: 'aqol4d_unwtd_dbl'
#' @param utl_var_nm_1L_chr Utility variable name (a character vector of length one), Default: 'aqol4d_utl_dbl'
#' @return an AQoL-4D (ready4 s3 class defining a data dictionary tibble.)
#' @rdname make_aqol4d_dict
#' @export 
#' @importFrom ready4use ready4use_dictionary renew.ready4use_dictionary
#' @keywords internal
make_aqol4d_dict <- function (prefix_1L_chr = "aqol4d_q", scrg_dss_ls = NULL, tot_unwtd_var_nm_1L_chr = "aqol4d_unwtd_dbl", 
    utl_var_nm_1L_chr = "aqol4d_utl_dbl") 
{
    if (is.null(scrg_dss_ls)) {
        scrg_dss_ls <- make_aqol4d_scrg_dss_ls()
    }
    domains_chr <- scrg_dss_ls$domain_qs_lup$Domain_chr %>% unique()
    aqol4d_ready4use_dictionary <- ready4use::ready4use_dictionary() %>% 
        ready4use::renew.ready4use_dictionary(var_nm_chr = c(paste0(prefix_1L_chr, 
            1:12), c("aqol4d_imputed_lgl", "aqol4d_complete_lgl"), 
            paste0("aqol4d_unwtd_", domains_chr, "_dbl"), tot_unwtd_var_nm_1L_chr, 
            paste0("aqol4d_dim_", domains_chr, "_dbl"), utl_var_nm_1L_chr), 
            var_ctg_chr = c(rep(paste0("multi-attribute utility instrument question"), 
                12), rep(paste0("multi-attribute utility instrument participation"), 
                2), rep("multi-attribute utility instrument unweighted dimension score", 
                4), "multi-attribute utility instrument unweighted total score", 
                rep("utility dimension score", 4), "utility overall score (final weighted)"), 
            var_desc_chr = c(paste0("AQoL-4D - ", c("independent living - self-care", 
                "independent living - household tasks", "independent living - mobility", 
                "relationships - quality", "relationships - quantity", 
                "relationships - role", "senses - vision", "senses - hearing", 
                "senses - communication", "mental health - sleep", 
                "mental health - affect", "mental health - pain")), 
                "AQoL-4D item responses include imputed values", 
                "Responded to all AQoL-4D questions", paste0("AQoL-4D ", 
                  c("Independent Living", "Relationships", "Senses", 
                    "Mental Health"), " domain (unweighted)"), 
                "Unweighted AQoL-4D total", paste0("AQoL-4D ", 
                  c("Independent Living", "Relationships", "Senses", 
                    "Mental Health"), " domain (weighted)"), 
                "AQol-4D utility"), var_type_chr = c(rep("integer", 
                12), rep("logical", 2), rep("numeric", 10)))
    return(aqol4d_ready4use_dictionary)
}
#' Make AQoL-4D dimension multipliers lookup table
#' @description make_aqol4d_dim_multipliers_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aqol-4d dimension multipliers lookup table. The function returns an AQoL-4D dimension multipliers (a lookup table).
#' @param domain_qs_lup Domain questions (a lookup table), Default: NULL
#' @return an AQoL-4D dimension multipliers (a lookup table)
#' @rdname make_aqol4d_dim_multipliers_lup
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_aqol4d_dim_multipliers_lup <- function (domain_qs_lup = NULL) 
{
    if (is.null(domain_qs_lup)) 
        domain_qs_lup <- make_aqol4d_domain_qs_lup()
    aqol4d_dim_multipliers_lup <- tibble::tibble(Dimension_chr = domain_qs_lup$Domain_chr %>% 
        unique(), Multiplier_dbl = c(1.0989, 1.0395, 1.6556, 
        1.292))
    return(aqol4d_dim_multipliers_lup)
}
#' Make AQoL-4D domain questions lookup table
#' @description make_aqol4d_domain_qs_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aqol-4d domain questions lookup table. The function returns an AQoL-4D domain questions (a lookup table).

#' @return an AQoL-4D domain questions (a lookup table)
#' @rdname make_aqol4d_domain_qs_lup
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_aqol4d_domain_qs_lup <- function () 
{
    aqol4d_domain_qs_lup <- tibble::tibble(Question_int = 1:12, 
        Domain_chr = c(rep("IL", 3), rep("RL", 3), rep("SN", 
            3), rep("MH", 3)), Coefficient_dbl = c(0.6097, 0.4641, 
            0.5733, 0.7023, 0.6253, 0.6638, 0.2476, 0.2054, 0.3382, 
            0.1703, 0.2554, 0.6347))
    return(aqol4d_domain_qs_lup)
}
#' Make AQoL-4D domains list
#' @description make_aqol4d_domains_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aqol-4d domains list. The function returns Domains (a list).
#' @param domain_qs_lup Domain questions (a lookup table), Default: NULL
#' @return Domains (a list)
#' @rdname make_aqol4d_domains_ls
#' @export 
#' @importFrom purrr map
#' @importFrom ready4 get_from_lup_obj
#' @importFrom stats setNames
#' @keywords internal
make_aqol4d_domains_ls <- function (domain_qs_lup = NULL) 
{
    if (is.null(domain_qs_lup)) 
        domain_qs_lup <- make_aqol4d_domain_qs_lup()
    domains_chr <- domain_qs_lup$Domain_chr %>% unique()
    domains_ls <- domains_chr %>% purrr::map(~ready4::get_from_lup_obj(domain_qs_lup, 
        match_var_nm_1L_chr = "Domain_chr", match_value_xx = .x, 
        target_var_nm_1L_chr = "Question_int")) %>% stats::setNames(domains_chr)
    return(domains_ls)
}
#' Make AQoL-4D item disvalue lookup table
#' @description make_aqol4d_item_disvalue_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aqol-4d item disvalue lookup table. The function returns Item disvalue (a lookup table).

#' @return Item disvalue (a lookup table)
#' @rdname make_aqol4d_item_disvalue_lup
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_aqol4d_item_disvalue_lup <- function () 
{
    item_disvalue_lup <- tibble::tibble(Question_chr = paste0("Q", 
        1:12), Answer_1_dbl = 0, Answer_2_dbl = c(0.154, 0.244, 
        0.326, 0.169, 0.095, 0.147, 0.145, 0.253, 0.219, 0.107, 
        0.141, 0.104), Answer_3_dbl = c(0.403, 0.343, 0.415, 
        0.396, 0.191, 0.297, 0.288, 0.478, 0.343, 0.109, 0.199, 
        0.312), Answer_4_dbl = 1)
    return(item_disvalue_lup)
}
#' Make AQoL-4D parameters lookup table
#' @description make_aqol4d_params_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aqol-4d parameters lookup table. The function returns an AQoL-4D parameters (a lookup table).
#' @param domain_qs_lup Domain questions (a lookup table), Default: NULL
#' @return an AQoL-4D parameters (a lookup table)
#' @rdname make_aqol4d_params_lup
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_aqol4d_params_lup <- function (domain_qs_lup = NULL) 
{
    if (is.null(domain_qs_lup)) 
        domain_qs_lup <- make_aqol4d_domain_qs_lup()
    aqol4d_params_lup <- tibble::tibble(Parameter_chr = c("Multiplier", 
        domain_qs_lup$Domain_chr %>% unique(), "Constant"), Value_dbl = c(1.04, 
        0.841, 0.855, 0.931, 0.997, -0.04))
    return(aqol4d_params_lup)
}
#' Make AQoL-4D scoring datasets list
#' @description make_aqol4d_scrg_dss_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aqol-4d scoring datasets list. The function returns an AQoL-4D scoring datasets (a list).

#' @return an AQoL-4D scoring datasets (a list)
#' @rdname make_aqol4d_scrg_dss_ls
#' @export 
#' @keywords internal
make_aqol4d_scrg_dss_ls <- function () 
{
    domain_qs_lup <- make_aqol4d_domain_qs_lup()
    aqol4d_scrg_dss_ls <- list(dim_multipliers_lup = make_aqol4d_dim_multipliers_lup(domain_qs_lup = domain_qs_lup), 
        item_disvalue_lup = make_aqol4d_item_disvalue_lup(), 
        domain_qs_lup = domain_qs_lup, params_lup = make_aqol4d_params_lup(domain_qs_lup = domain_qs_lup))
    return(aqol4d_scrg_dss_ls)
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
        aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
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
#' Make Assessment of Quality of Life Six Dimension scoring dictionary
#' @description make_aqol6d_scrg_dict() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make assessment of quality of life six dimension scoring dictionary. The function is called for its side effects and does not return a value.
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param scrg_dss_ls Scoring datasets (a list), Default: NULL
#' @param tot_unwtd_var_nm_1L_chr Total unweighted variable name (a character vector of length one), Default: 'aqol6d_total_c'
#' @param utl_var_nm_1L_chr Utility variable name (a character vector of length one), Default: 'aqol6d_total_w'
#' @return Assessment of Quality of Life Six Dimension (ready4 s3 class defining a data dictionary tibble.)
#' @rdname make_aqol6d_scrg_dict
#' @export 
#' @importFrom ready4use ready4use_dictionary renew.ready4use_dictionary
#' @keywords internal
make_aqol6d_scrg_dict <- function (prefix_1L_chr = "aqol6d_q", scrg_dss_ls = NULL, tot_unwtd_var_nm_1L_chr = "aqol6d_total_c", 
    utl_var_nm_1L_chr = "aqol6d_total_w") 
{
    if (is.null(scrg_dss_ls)) {
        scrg_dss_ls <- make_aqol6d_scrg_dss()
    }
    aqol6d_ready4use_dictionary <- ready4use::ready4use_dictionary() %>% 
        ready4use::renew.ready4use_dictionary(var_nm_chr = c(paste0(prefix_1L_chr, 
            1:20), tot_unwtd_var_nm_1L_chr, scrg_dss_ls$adol_dim_sclg_eqs_lup$Dim_scal[1:6], 
            scrg_dss_ls$adol_dim_sclg_eqs_lup$Dim_scal[7:12], 
            paste0("dvQ", 1:20), scrg_dss_ls$adol_dim_sclg_eqs_lup$Dim_scal[13:18], 
            utl_var_nm_1L_chr), var_ctg_chr = c(rep(paste0("multi-attribute utility instrument question"), 
            20), "multi-attribute utility instrument unweighted total score", 
            rep("utility dimension disvalue", 6), rep("utility dimension score (adult)", 
                6), rep("utility item disvalue", 20), c("utility overall score (disvalue scale)", 
                "utility overall score (life-death scale)", rep("utility overall score (adolescent disutility scale)", 
                  2), "utility overall score (instrument)", "utility overall score (instrument - rotated)"), 
            "utility overall score (final weighted)"), var_desc_chr = c(c(paste0("AQoL-6D - ", 
            c(paste0("independent living - ", c("household tasks", 
                "mobility (out of home)", "mobility (unaided)", 
                "self-care")), paste0("relationships - ", c("quality", 
                "role (family)", "role (community)")), paste0("mental health - ", 
                c("despair", "anxiety", "sadness", "calm")), 
                paste0("coping - ", c("energy", "control", "resilience")), 
                paste0("pain - ", c("frequency", "quanitity", 
                  "impact")), paste0("senses - ", c("vision", 
                  "hearing", "communication"))))), "Unweighted AQoL-4D total", 
            scrg_dss_ls$adol_dim_sclg_eqs_lup$Label[1:6], scrg_dss_ls$adol_dim_sclg_eqs_lup$Label[7:12], 
            paste0("Assessment of Quality of Life (6 Dimension) item disvalue ", 
                1:20), scrg_dss_ls$adol_dim_sclg_eqs_lup$Label[13:18], 
            "AQol-6D utility"), var_type_chr = c(rep("integer", 
            20), rep("numeric", 40)))
    return(aqol6d_ready4use_dictionary)
}
#' Make Assessment of Quality of Life Six Dimension scoring datasets
#' @description make_aqol6d_scrg_dss() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make assessment of quality of life six dimension scoring datasets. The function returns Assessment of Quality of Life Six Dimension scoring datasets (a list).

#' @return Assessment of Quality of Life Six Dimension scoring datasets (a list)
#' @rdname make_aqol6d_scrg_dss
#' @export 
#' @importFrom tibble tribble tibble
#' @keywords internal
make_aqol6d_scrg_dss <- function () 
{
    aqol6d_scrg_dss_ls <- list(aqol6d_adult_disv_lup_tb = NULL, 
        aqol6d_adult_itm_wrst_wts_lup_tb = NULL, aqol6d_adult_vldn_pop_with_STATA_scores_tb = NULL, 
        aqol6d_dim_sclg_con_lup_tb = NULL, aqol6d_domain_qs_lup_tb = NULL, 
        aqol6d_from_8d_coefs_lup_tb = NULL, adol_dim_sclg_eqs_lup = NULL)
    aqol6d_scrg_dss_ls$aqol6d_from_8d_coefs_lup_tb <- tibble::tribble(~var_name_chr, 
        ~coef_dbl, "vD1", 0.0719264, "vD2", 0.1027818, "vD3", 
        0.2519563, "vD4", 0.3201172, "vD5", 0.1288289, "vD6", 
        0.2052164, "Constant", -0.0444493)
    aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb <- tibble::tribble(~Question_chr, 
        ~Answer_1_dbl, ~Answer_2_dbl, ~Answer_3_dbl, ~Answer_4_dbl, 
        ~Answer_5_dbl, ~Answer_6_dbl, "Q1", 0, 0.073, 0.435, 
        0.82, 1, NA_real_, "Q2", 0, 0.033, 0.24, 0.471, 0.84, 
        1, "Q3", 0, 0.041, 0.251, 0.57, 0.83, 1, "Q4", 0, 0.04, 
        0.297, 0.797, 1, NA_real_, "Q5", 0, 0.074, 0.461, 0.841, 
        1, NA_real_, "Q6", 0, 0.193, 0.759, 1, NA_real_, NA_real_, 
        "Q7", 0, 0.197, 0.648, 1, NA_real_, NA_real_, "Q8", 0, 
        0.133, 0.392, 0.838, 1, NA_real_, "Q9", 0, 0.142, 0.392, 
        0.824, 1, NA_real_, "Q10", 0, 0.097, 0.33, 0.784, 1, 
        NA_real_, "Q11", 0, 0.064, 0.368, 0.837, 1, NA_real_, 
        "Q12", 0, 0.056, 0.338, 0.722, 1, NA_real_, "Q13", 0, 
        0.055, 0.382, 0.774, 1, NA_real_, "Q14", 0, 0.057, 0.423, 
        0.826, 1, NA_real_, "Q15", 0, 0.133, 0.642, 1, NA_real_, 
        NA_real_, "Q16", 0, 0.2, 0.758, 1, NA_real_, NA_real_, 
        "Q17", 0, 0.072, 0.338, 0.752, 1, NA_real_, "Q18", 0, 
        0.033, 0.223, 0.621, 0.843, 1, "Q19", 0, 0.024, 0.205, 
        0.586, 0.826, 1, "Q20", 0, 0.187, 0.695, 1, NA_real_, 
        NA_real_)
    aqol6d_scrg_dss_ls$aqol6d_domain_qs_lup_tb <- tibble::tibble(Question_dbl = 1:20, 
        Domain_chr = c(rep("IL", 4), rep("REL", 3), rep("MH", 
            4), rep("COP", 3), rep("P", 3), rep("SEN", 3)))
    aqol6d_scrg_dss_ls$aqol6d_dim_sclg_con_lup_tb <- tibble::tribble(~Dimension_chr, 
        ~Constant_dbl, "IL", -0.978, "RL", -0.923, "MH", -0.983, 
        "COP", -0.93, "P", -0.96, "SEN", -0.851)
    aqol6d_scrg_dss_ls$aqol6d_adult_itm_wrst_wts_lup_tb <- tibble::tribble(~Question_chr, 
        ~Worst_Weight_dbl, "Q1", 0.385412, "Q2", 0.593819, "Q3", 
        0.630323, "Q4", 0.794888, "Q5", 0.64303, "Q6", 0.697742, 
        "Q7", 0.508658, "Q8", 0.640377, "Q9", 0.588422, "Q10", 
        0.648748, "Q11", 0.71122, "Q12", 0.415694, "Q13", 0.636994, 
        "Q14", 0.773296, "Q15", 0.631833, "Q16", 0.767573, "Q17", 
        0.652241, "Q18", 0.580696, "Q19", 0.463022, "Q20", 0.604613)
    data("aqol6d_adult_vldn_pop_with_STATA_scores_tb", package = "scorz", 
        envir = environment())
    data("adol_dim_sclg_eqs_lup", package = "scorz", envir = environment())
    aqol6d_scrg_dss_ls$aqol6d_adult_vldn_pop_with_STATA_scores_tb <- aqol6d_adult_vldn_pop_with_STATA_scores_tb
    aqol6d_scrg_dss_ls$adol_dim_sclg_eqs_lup <- adol_dim_sclg_eqs_lup
    return(aqol6d_scrg_dss_ls)
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
#' @description make_eq5d_dict() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make eq5d dictionary. The function returns EQ5D dictionary (a ready4 submodule).

#' @return EQ5D dictionary (a ready4 submodule)
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
#' Make filtered profile
#' @description make_filtered_profile() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make filtered profile. The function is called for its side effects and does not return a value.
#' @param X_ScorzProfile PARAM_DESCRIPTION, Default: ScorzProfile()
#' @param nbr_of_digits_1L_int Number of digits (an integer vector of length one), Default: 2
#' @param profile_chr Profile (a character vector), Default: character(0)
#' @param timepoint_1L_int Timepoint (an integer vector of length one), Default: 1
#' @return X (A dataset and its associated dictionary, descriptive statistics and metadata.)
#' @rdname make_filtered_profile
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @keywords internal
make_filtered_profile <- function (X_ScorzProfile = ScorzProfile(), nbr_of_digits_1L_int = 2L, 
    profile_chr = character(0), timepoint_1L_int = 1L) 
{
    assertthat::assert_that(inherits(X_ScorzProfile, "ScorzProfile"), 
        msg = "X_ScorzProfile must be a ScorzProfile ready4 module")
    assertthat::assert_that(inherits(X_ScorzProfile@a_YouthvarsProfile, 
        "YouthvarsSeries"), msg = "X_ScorzProfile@a_YouthvarsProfile must be a Youthvars_Series ready4 module")
    assertthat::assert_that(!identical(profile_chr, character(0)), 
        msg = "profile_chr needs to be a non empty character vector")
    assertthat::assert_that(identical(setdiff(profile_chr, names(X_ScorzProfile@a_YouthvarsProfile@a_Ready4useDyad@ds_tb)), 
        character(0)), msg = "All values of profile_chr need to be column names in X_ScorzProfile@a_YouthvarsProfile@a_Ready4useDyad@ds_tb")
    Z <- X_ScorzProfile
    Y <- YouthvarsProfile(a_Ready4useDyad = Z@a_YouthvarsProfile@a_Ready4useDyad, 
        id_var_nm_1L_chr = Z@a_YouthvarsProfile@id_var_nm_1L_chr)
    Y@a_Ready4useDyad@ds_tb <- Y@a_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(Z@a_YouthvarsProfile@timepoint_var_nm_1L_chr) == 
        Z@a_YouthvarsProfile@timepoint_vals_chr[1])
    Y <- renew(Y, nbr_of_digits_1L_int = nbr_of_digits_1L_int, 
        profile_chr = profile_chr)
    X_YouthvarsProfile <- Y
    return(X_YouthvarsProfile)
}
#' Make scoring tibble
#' @description make_scoring_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make scoring tibble. The function returns Scoring (a tibble).
#' @param name_chr Name (a character vector), Default: character(0)
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param domains_unwtd_ls Domains unweighted (a list), Default: list(list())
#' @param scoring_fn_chr Scoring function (a character vector), Default: character(0)
#' @param country_chr Country (a character vector), Default: character(0)
#' @param dictionary_args_ls Dictionary arguments (a list), Default: list(list())
#' @param dictionary_fn_chr Dictionary function (a character vector), Default: character(0)
#' @param domains_wtd_ls Domains weighted (a list), Default: list(list())
#' @param dss_ls Datasets (a list), Default: list(list())
#' @param item_prefix_chr Item prefix (a character vector), Default: character(0)
#' @param match_chr Match (a character vector), Default: character(0)
#' @param scoring_args_ls Scoring arguments (a list), Default: list(list())
#' @param total_unwtd_nm_chr Total unweighted name (a character vector), Default: character(0)
#' @param total_wtd_nm_chr Total weighted name (a character vector), Default: character(0)
#' @param version_chr Version (a character vector), Default: character(0)
#' @param type_chr Type (a character vector), Default: 'dbl'
#' @return Scoring (a tibble)
#' @rdname make_scoring_tb
#' @export 
#' @importFrom purrr map
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @keywords internal
make_scoring_tb <- function (name_chr = character(0), short_name_chr = character(0), 
    domains_unwtd_ls = list(list()), scoring_fn_chr = character(0), 
    country_chr = character(0), dictionary_args_ls = list(list()), 
    dictionary_fn_chr = character(0), domains_wtd_ls = list(list()), 
    dss_ls = list(list()), item_prefix_chr = character(0), match_chr = character(0), 
    scoring_args_ls = list(list()), total_unwtd_nm_chr = character(0), 
    total_wtd_nm_chr = character(0), version_chr = character(0), 
    type_chr = "dbl") 
{
    X <- ScorzProfile()
    if (identical(country_chr, character(0)) & !identical(short_name_chr, 
        character(0))) 
        country_chr <- X@country_1L_chr
    if (identical(dictionary_fn_chr, character(0))) 
        dictionary_fn_chr <- scoring_fn_chr
    if (identical(dictionary_args_ls, list(list())) & !identical(short_name_chr, 
        character(0))) 
        dictionary_args_ls <- purrr::map(1:length(short_name_chr), 
            ~list(what_1L_chr = "dict"))
    if (identical(domains_unwtd_ls, list(list())) & !identical(short_name_chr, 
        character(0))) 
        domains_unwtd_ls <- purrr::map(1:length(short_name_chr), 
            ~X@domain_unwtd_var_nms_chr)
    if (identical(domains_wtd_ls, list(list())) & !identical(short_name_chr, 
        character(0))) 
        domains_wtd_ls <- purrr::map(1:length(short_name_chr), 
            ~X@domain_wtd_var_nms_chr)
    if (identical(dss_ls, list(list())) & !identical(short_name_chr, 
        character(0))) 
        dss_ls <- purrr::map(1:length(short_name_chr), ~X@scrg_dss_ls)
    if (identical(item_prefix_chr, character(0)) & !identical(short_name_chr, 
        character(0))) 
        item_prefix_chr <- paste0(short_name_chr %>% stringr::str_replace("-", 
            ""), "_q")
    if (identical(match_chr, character(0))) 
        match_chr <- short_name_chr
    if (identical(total_unwtd_nm_chr, character(0)) & !identical(short_name_chr, 
        character(0))) 
        total_unwtd_nm_chr <- paste0(short_name_chr %>% stringr::str_replace("-", 
            ""), "_total_", type_chr)
    if (identical(total_wtd_nm_chr, character(0)) & !identical(short_name_chr, 
        character(0))) 
        total_wtd_nm_chr <- paste0(short_name_chr %>% stringr::str_replace("-", 
            ""), "_total_", type_chr)
    if (identical(version_chr, character(0)) & !identical(short_name_chr, 
        character(0))) 
        version_chr <- X@instrument_version_1L_chr
    scoring_tb <- tibble::tibble(name_chr = name_chr, short_name_chr = short_name_chr, 
        item_prefix_chr = item_prefix_chr, domains_unwtd_ls = domains_unwtd_ls, 
        scoring_fn_chr = scoring_fn_chr, scoring_args_ls = scoring_args_ls, 
        country_chr = country_chr, domains_wtd_ls = domains_wtd_ls, 
        match_chr = match_chr, dictionary_fn_chr = dictionary_fn_chr, 
        dictionary_args_ls = dictionary_args_ls, dss_ls = dss_ls, 
        total_unwtd_var_nms_chr = total_unwtd_nm_chr, total_wtd_nm_chr = total_wtd_nm_chr, 
        version_chr = version_chr)
    return(scoring_tb)
}
