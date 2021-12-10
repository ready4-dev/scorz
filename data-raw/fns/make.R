make_adol_aqol6d_disv_lup <- function (aqol6d_scrg_dss_ls = NULL)
{
  if(is.null(aqol6d_scrg_dss_ls))
    aqol6d_scrg_dss_ls <- get_aqol6d_scrng_dss()
  aqol6d_adult_disv_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
  adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>%
    dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == "Q18" ~ 0.622,
                                                  TRUE ~ Answer_4_dbl),
                  Answer_5_dbl = dplyr::case_when(Question_chr == "Q3" ~ 0.827,
                                                  TRUE ~ Answer_5_dbl),
                  Answer_6_dbl = dplyr::case_when(Question_chr == "Q1" ~ 0.073,
                                                  TRUE ~ Answer_5_dbl))
  return(adol_aqol6d_disv_lup)
}
make_aqol6d_adol_pop_tbs_ls <- function (aqol_items_prpns_tbs_ls, aqol_scores_pars_ls, series_names_chr,
                                         synth_data_spine_ls, temporal_cors_ls,
                                         aqol6d_scrg_dss_ls = NULL,
                                         id_var_nm_1L_chr = "fkClientID",
                                         prefix_chr = c(uid = "Participant_", aqol_item = "aqol6d_q",
                                                        domain_unwtd_pfx_1L_chr = "aqol6d_subtotal_c_", domain_wtd_pfx_1L_chr = "aqol6d_subtotal_w_"))
{
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrng_dss()
  }
  domain_qs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_domain_qs_lup_tb
  item_pfx_1L_chr <- prefix_chr[["aqol_item"]]
  uid_pfx_1L_chr <- prefix_chr[["uid"]]
  aqol6d_adol_pop_tbs_ls <- make_synth_series_tbs_ls(synth_data_spine_ls,
                                                     series_names_chr = series_names_chr) %>% add_cors_and_utls_to_aqol6d_tbs_ls(aqol_scores_pars_ls = aqol_scores_pars_ls,
                                                                                                                                 aqol_items_prpns_tbs_ls = aqol_items_prpns_tbs_ls, temporal_cors_ls = temporal_cors_ls,
                                                                                                                                 prefix_chr = prefix_chr, aqol_tots_var_nms_chr = synth_data_spine_ls$aqol_tots_var_nms_chr,
                                                                                                                                 aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls,
                                                                                                                                 id_var_nm_1L_chr = id_var_nm_1L_chr) %>% purrr::map(~{
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
make_aqol6d_fns_ls <- function (domain_items_ls)
{
  aqol6d_disu_fn_ls <- paste0("calculate_aqol6d_dim_", 1:length(domain_items_ls),
                              "_disv") %>% purrr::map(~rlang::sym(.x))
  return(aqol6d_disu_fn_ls)
}
make_aqol6d_item_nms <- function(){
  aqol6d_item_nms_chr <- c("Household tasks", "Getting around",
                           "Morbility","Self care","Enjoy close rel\'s",
                           "Family rel\'s", "Community involv\'t",
                           "Despair","Worry", "Sad", "Agitated",
                           "Energy level", "Control", "Coping",
                           "Frequency of pain", "Degree of pain",
                           "Pain interference","Vision", "Hearing",
                           "Communication")
  return(aqol6d_item_nms_chr)
}
make_aqol6d_items_tb <- function (aqol_tb, old_pfx_1L_chr, new_pfx_1L_chr)
{
  aqol6d_items_tb <- aqol_tb %>% dplyr::select(dplyr::starts_with(old_pfx_1L_chr)) %>%
    dplyr::rename_all(~{
      stringr::str_replace(., old_pfx_1L_chr, new_pfx_1L_chr)
    })
  return(aqol6d_items_tb)
}
make_dim_sclg_cons_dbl <- function (domains_chr, dim_sclg_con_lup_tb)
{
  dim_sclg_cons_dbl <- purrr::map_dbl(domains_chr, ~ready4::get_from_lup_obj(dim_sclg_con_lup_tb,
                                                                             match_var_nm_1L_chr = "Dimension_chr", match_value_xx = .x,
                                                                             target_var_nm_1L_chr = "Constant_dbl", evaluate_1L_lgl = F))
  return(dim_sclg_cons_dbl)
}
make_domain_items_ls <- function (domain_qs_lup_tb, item_pfx_1L_chr)
{
  domains_chr <- domain_qs_lup_tb$Domain_chr %>% unique()
  q_nbrs_ls <- purrr::map(domains_chr, ~domain_qs_lup_tb %>%
                            dplyr::filter(Domain_chr == .x) %>% dplyr::pull(Question_dbl))
  domain_items_ls <- purrr::map(q_nbrs_ls, ~paste0(item_pfx_1L_chr,
                                                   .x)) %>% stats::setNames(domains_chr)
  return(domain_items_ls)
}
make_eq5d_dict <- function(){
  eq5d_dict_r3 <- ready4use::ready4use_dictionary(ready4use::make_pt_ready4use_dictionary(var_nm_chr = paste0("eq5dq_",1:5),
                                                                          var_ctg_chr = "multi-attribute utility instrument question",
                                                                          var_desc_chr = paste0("EuroQol EQ-5D item question",1:5),
                                                                          var_type_chr = "integer"))
  return(eq5d_dict_r3)
}
