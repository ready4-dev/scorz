calculate_adol_aqol6dU <- function (unscored_aqol_tb,
                                    aqol6d_scrg_dss_ls = NULL,
                                    prefix_1L_chr = "aqol6d_q",
                                    id_var_nm_1L_chr = "fkClientID",
                                    wtd_aqol_var_nm_1L_chr = "aqol6d_total_w")
{
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
  }
  scored_aqol_tb <- add_adol6d_scores(unscored_aqol_tb,
                                      aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls,
                                      prefix_1L_chr = prefix_1L_chr,
                                      id_var_nm_1L_chr = id_var_nm_1L_chr, wtd_aqol_var_nm_1L_chr = wtd_aqol_var_nm_1L_chr)
  adol_aqol6d_dbl <- scored_aqol_tb %>% dplyr::pull(!!rlang::sym(wtd_aqol_var_nm_1L_chr))
  return(adol_aqol6d_dbl)
}
calculate_adult_aqol6dU <- function (aqol6d_items_tb,
                                     prefix_1L_chr,
                                     aqol6d_scrg_dss_ls = NULL)
{
  if(is.null(aqol6d_scrg_dss_ls))
    aqol6d_scrg_dss_ls <- make_aqol6d_scrg_dss()
  coefs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_from_8d_coefs_lup_tb
  dim_sclg_con_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_dim_sclg_con_lup_tb
  disvalues_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
  domain_qs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_domain_qs_lup_tb
  itm_wrst_wts_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_itm_wrst_wts_lup_tb
  domains_chr <- dim_sclg_con_lup_tb$Dimension_chr
  item_pfx_1L_chr <- hutils::longest_prefix(disvalues_lup_tb$Question_chr)
  domain_items_ls <- make_domain_items_ls(domain_qs_lup_tb = domain_qs_lup_tb,
                                          item_pfx_1L_chr = item_pfx_1L_chr)
  aqol6d_items_tb <- aqol6d_items_tb %>% make_aqol6d_items_tb(old_pfx_1L_chr = prefix_1L_chr,
                                                              new_pfx_1L_chr = item_pfx_1L_chr) %>% impute_adult_aqol6d_items_tb(domain_items_ls = domain_items_ls) %>%
    add_itm_disv_to_aqol6d_itms_tb(disvalues_lup_tb = disvalues_lup_tb,
                                   pfx_1L_chr = item_pfx_1L_chr) %>% add_dim_disv_to_aqol6d_items_tb(domain_items_ls = domain_items_ls,
                                                                                                     domains_chr = domains_chr, dim_sclg_con_lup_tb = dim_sclg_con_lup_tb,
                                                                                                     itm_wrst_wts_lup_tb = itm_wrst_wts_lup_tb) %>% add_dim_scores_to_aqol6d_items_tb(domain_items_ls = domain_items_ls) %>%
    add_aqol6dU_to_aqol6d_items_tb(coefs_lup_tb = coefs_lup_tb)
  aqol6dU_dbl <- aqol6d_items_tb$aqol6dU
  return(aqol6dU_dbl)
}
calculate_aqol4d_dim_disv <- function(domain_1L_chr,
                                      ds_tb,
                                      scrg_dss_ls = NULL) {
  if(is.null(scrg_dss_ls)){
    scrg_dss_ls <- make_aqol4d_scrg_dss_ls()
  }
  domains_chr <- scrg_dss_ls$domain_qs_lup$Domain_chr %>% unique()
  domains_ls <- domains_chr %>% purrr::map(~ready4::get_from_lup_obj(scrg_dss_ls$domain_qs_lup, match_var_nm_1L_chr = "Domain_chr", match_value_xx = .x, target_var_nm_1L_chr = "Question_int")) %>%
    stats::setNames(domains_chr)
  qs_int <- domains_ls[[domain_1L_chr]]
  coefficients_dbl <- qs_int %>% purrr::map_dbl(~ ready4::get_from_lup_obj(scrg_dss_ls$domain_qs_lup, match_var_nm_1L_chr = "Question_int", match_value_xx = .x, target_var_nm_1L_chr = "Coefficient_dbl"))
  multiplier_1L_dbl <- ready4::get_from_lup_obj(scrg_dss_ls$dim_multipliers_lup, match_var_nm_1L_chr = "Dimension_chr", match_value_xx = domain_1L_chr, target_var_nm_1L_chr = "Multiplier_dbl")
  aqol4d_dim_disv_dbl <- ds_tb %>% dplyr::select(paste0("aqol4d_disv_q",qs_int,"_dbl")) %>% purrr::pmap_dbl(~ (multiplier_1L_dbl*(1-(1-coefficients_dbl[1]*..1)*(1-coefficients_dbl[2]*..2)*(1-coefficients_dbl[3]*..3))))
  return(aqol4d_dim_disv_dbl)
}


calculate_aqol6d_dim_1_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl)
{
  dvD1_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
    (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) *
                       (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl *
                                                                    w_dbl[3] * ..3)) * (1 + (kD_1L_dbl * w_dbl[4] * ..4)) -
                       1)
  })
  return(dvD1_dbl)
}
calculate_aqol6d_dim_2_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl)
{
  dvD2_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
    (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) *
                       (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl *
                                                                    w_dbl[3] * ..3)) - 1)
  })
  return(dvD2_dbl)
}
calculate_aqol6d_dim_3_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl)
{
  dvD3_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
    (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) *
                       (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl *
                                                                    w_dbl[3] * ..3)) * (1 + (kD_1L_dbl * w_dbl[4] * 1 *
                                                                                               ..4)) - 1)
  })
  return(dvD3_dbl)
}
calculate_aqol6d_dim_4_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl)
{
  dvD4_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
    (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) *
                       (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl *
                                                                    w_dbl[3] * ..3)) - 1)
  })
  return(dvD4_dbl)
}
calculate_aqol6d_dim_5_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl)
{
  dvD5_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
    (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) *
                       (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl *
                                                                    w_dbl[3] * ..3)) - 1)
  })
  return(dvD5_dbl)
}
calculate_aqol6d_dim_6_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl)
{
  dvD6_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
    (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) *
                       (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl *
                                                                    w_dbl[3] * ..3)) - 1)
  })
  return(dvD6_dbl)
}
