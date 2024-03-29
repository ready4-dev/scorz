library(ready4use)
# data(adol_dim_scalg_eqs_lup, package = "youthvars")
# data(aqol6d_adult_disv_lup_tb, package = "youthvars")
# data(aqol6d_adult_itm_wrst_wts_lup_tb,package = "youthvars")
# data(aqol6d_adult_vldn_pop_with_STATA_scores_tb, package = "youthvars")
# data(aqol6d_dim_sclg_con_lup_tb, package = "youthvars")
# data(aqol6d_domain_qs_lup_tb, package = "youthvars")
# data(aqol6d_from_8d_coefs_lup_tb, package = "youthvars")
#
# aqol6d_scrg_dss_ls <- list(adol_dim_scalg_eqs_lup = adol_dim_scalg_eqs_lup,
#                             aqol6d_adult_disv_lup_tb = aqol6d_adult_disv_lup_tb,
#                             aqol6d_adult_itm_wrst_wts_lup_tb = aqol6d_adult_itm_wrst_wts_lup_tb,
#                             aqol6d_adult_vldn_pop_with_STATA_scores_tb = aqol6d_adult_vldn_pop_with_STATA_scores_tb,
#                             aqol6d_dim_sclg_con_lup_tb = aqol6d_dim_sclg_con_lup_tb,
#                             aqol6d_domain_qs_lup_tb = aqol6d_domain_qs_lup_tb,
#                             aqol6d_from_8d_coefs_lup_tb = aqol6d_from_8d_coefs_lup_tb)
aqol6d_scrg_dss_ls <- fns_env_ls$fns_env$get_aqol6d_scrg_dss()
aqol6d_scrg_dss_ls$adol_dim_sclg_eqs_lup <- aqol6d_scrg_dss_ls$adol_dim_scalg_eqs_lup
aqol6d_scrg_dss_ls$adol_dim_scalg_eqs_lup <- NULL

# data(aqol_scrg_dict_r3, package = "youthvars")
# z <- Ready4usePointer(b_Ready4useRepos = Ready4useRepos(dv_nm_1L_chr = "fakes",
#                                                         dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
#                                                         dv_server_1L_chr = "dataverse.harvard.edu",
#                                                         gh_repo_1L_chr = "ready4-dev/scorz",
#                                                         gh_tag_1L_chr = "Documentation_0.0"))
# y <- Ready4useRecord(a_Ready4usePointer = z,
#                      b_Ready4useIngest = Ready4useIngest(objects_ls = list(aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls#aqol_scrg_dict_r3 = aqol_scrg_dict_r3#
#                                                                            ),
#                                                          descriptions_chr = c("Datasets used for scoring AQoL-6D (adolescent version)"#"Data dictionary for AQoL-6D scoring variables"#
#                                                                               )))
# y <- share(y, # NEED TO PROMPT FOR PERMISSION
#            type_1L_chr = "prefer_gh")
a <- ingest(Ready4useRepos(gh_repo_1L_chr = "ready4-dev/scorz",
                           gh_tag_1L_chr = "Documentation_0.0"),
            fls_to_ingest_chr = c("aqol6d_scrg_dss_ls"))
b <- procure(procureSlot(a, "b_Ready4useIngest"),
        fl_nm_1L_chr = "aqol6d_scrg_dss_ls")

