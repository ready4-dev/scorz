library(youthvars)
library(ready4use)
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Derive Summary scores from questionnaire item responses" %>% tools::toTitleCase(),
                                 pkg_desc_1L_chr = "Tools to summarise questionnaire responses from youth mental health collections in a single index measure.
                            The main motivation for this package is to facilitate automated scoring, using published publicly available scoring algorithms, of measures relevant to mental health systems models.
  This development version of the scorz package has been made available as part of the process of testing and documenting the package.
                            If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                 authors_prsn = c(utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton@orygen.org.au", role = c("aut", "cre"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                  utils::person(given = "Caroline",family = "Gao",email = "caroline.gao@orygen.org.au", role = c("aut"),comment = c(ORCID = "0000-0002-0987-2759")),
                                                  utils::person("Orygen", role = c("cph", "fnd")),
                                                  utils::person("Headspace", role = c( "fnd")),
                                                  utils::person("National Health and Medical Research Council", role = c( "fnd"))),
                                 urls_chr = c("https://ready4-dev.github.io/scorz/",
                                              "https://github.com/ready4-dev/scorz",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(depends_chr = "ready4",
                                                                       suggests_chr = "rmarkdown",
                                                                       imports_chr = "knitrBootstrap"),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(#user_manual_fns_chr = c()
                           ),##
                           dev_pkgs_chr = c("ready4",
                                            "ready4use","ready4show",
                                            "youthvars"),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/scorz-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "authoring",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5722708.svg)](https://doi.org/10.5281/zenodo.5722708)"
  )
y <- dplyr::bind_rows(ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                   name_stub_chr = "Profile",
                                                                   slots_ls = list("a_YouthvarsProfile",
                                                                                   "domain_unwtd_var_nms_chr",
                                                                                   "domain_wtd_var_nms_chr",
                                                                                   "instrument_dict_r3",
                                                                                   "instrument_nm_1L_chr",
                                                                                   "instrument_version_1L_chr",
                                                                                   "itm_labels_chr",
                                                                                   "itm_prefix_1L_chr",
                                                                                   "scrg_dss_ls",
                                                                                   "total_wtd_var_nm_1L_chr",
                                                                                   "total_unwtd_var_nm_1L_chr"
                                                                   ) %>% list(),
                                                                   pt_ls = list("YouthvarsProfile",
                                                                                "character",
                                                                                "character",
                                                                                "ready4use_dictionary",
                                                                                "character",
                                                                                "character",
                                                                                "character",
                                                                                "character",
                                                                                "list",
                                                                                "character",
                                                                                "character") %>% list(),
                                                                   class_desc_chr= "A dataset to be scored, its associated metadata and details of the scoring instrument.",
                                                                   parent_class_chr = "Ready4Module"),
                      ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                   name_stub_chr = "Aqol6",
                                                                   slots_ls = list("a_YouthvarsProfile",
                                                                                   "domain_unwtd_var_nms_chr",
                                                                                   "domain_wtd_var_nms_chr",
                                                                                   "instrument_dict_r3",
                                                                                   "instrument_nm_1L_chr",
                                                                                   "instrument_version_1L_chr",
                                                                                   "itm_labels_chr",
                                                                                   "itm_prefix_1L_chr",
                                                                                   "scrg_dss_ls",
                                                                                   "total_wtd_var_nm_1L_chr",
                                                                                   "total_unwtd_var_nm_1L_chr"
                                                                   ) %>% list(),
                                                                   pt_ls = list("YouthvarsProfile",
                                                                                "character",
                                                                                "character",
                                                                                "ready4use_dictionary",
                                                                                "character",
                                                                                "character",
                                                                                "character",
                                                                                "character",
                                                                                "list",
                                                                                "character",
                                                                                "character") %>% list(),
                                                                   vals_ls = list(list(domain_wtd_var_nms_chr = "paste0(\"vD\",1:6)",
                                                                                       instrument_dict_r3 = "get_aqol6d_scrg_dict()",
                                                                                       instrument_nm_1L_chr = "'Assessment of Quality of Life (6 Dimension)'",
                                                                                       #instrument_version_1L_chr = NA_character_,
                                                                                       itm_labels_chr = "make_aqol6d_item_nms()",
                                                                                       itm_prefix_1L_chr =  "'aqol6d_q'",
                                                                                       scrg_dss_ls = "get_aqol6d_scrg_dss()",
                                                                                       total_wtd_var_nm_1L_chr = "'aqol6d_total_w'",
                                                                                       total_unwtd_var_nm_1L_chr = "'aqol6d_total_c'"
                                                                                       )),
                                                                   class_desc_chr = "A dataset and metadata to support implementation of an AQoL-6D scoring algorithm.",
                                                                   parent_class_chr = "ScorzProfile",#"Ready4Module"#"
                                                                   inc_clss_ls = list("ScorzProfile")
                                                                   ),
                      ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                   name_stub_chr = "Aqol6Adol",
                                                                   slots_ls = list("instrument_version_1L_chr") %>% list(),
                                                                   pt_ls = list("character") %>% list(),
                                                                   vals_ls = list(list(instrument_version_1L_chr = "'Adolescent'")),
                                                                   class_desc_chr = "A dataset and metadata to support implementation of a scoring algorithm for the adolescent version of AQoL-6D.",
                                                                   parent_class_chr = "ScorzAqol6",
                                                                   inc_clss_ls = list("ScorzAqol6")
                                                                   ),
                      ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                   name_stub_chr = "Aqol6Adult",
                                                                   slots_ls = list("instrument_version_1L_chr") %>% list(),
                                                                   pt_ls = list("character") %>% list(),
                                                                   vals_ls = list(list(instrument_version_1L_chr = "'Adult'")),
                                                                   class_desc_chr = "A dataset and metadata to support implementation of a scoring algorithm for the adult version of AQoL-6D.",
                                                                   parent_class_chr = "ScorzAqol6",
                                                                   inc_clss_ls = list("ScorzAqol6")
                      )
                      ) %>%
  ready4class::ready4class_constructor()
z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y#,
                                             # pkg_ds_ls_ls = datasets_ls
) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
# devtools::build_vignettes()