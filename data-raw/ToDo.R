####
library(youthvars)
ScorzProfile <- methods::setClass("ScorzProfile", #scorz
                                  contains = "Ready4Module",
                                  slots = c(a_YouthvarsProfile = "YouthvarsProfile",
                                            domain_unwtd_var_nms_chr = "character",
                                            domain_wtd_var_nms_chr = "character",
                                            instrument_dict_r3 = "ready4use_dictionary",
                                            instrument_nm_1L_chr = "character",
                                            itm_labels_chr = "character",
                                            itm_prefix_1L_chr =  "character",
                                            scrg_dss_ls = "list",
                                            total_wtd_var_nm_1L_chr = "character",
                                            total_unwtd_var_nm_1L_chr = "character"),
                                  prototype = list(a_YouthvarsProfile = YouthvarsProfile(),
                                                   domain_unwtd_var_nms_chr = NA_character_,
                                                   domain_wtd_var_nms_chr = NA_character_,
                                                   instrument_dict_r3 = ready4use::ready4use_dictionary(),
                                                   instrument_nm_1L_chr = NA_character_,
                                                   itm_labels_chr = NA_character_,
                                                   itm_prefix_1L_chr =  NA_character_,
                                                   scrg_dss_ls = list(),
                                                   total_wtd_var_nm_1L_chr = NA_character_,
                                                   total_unwtd_var_nm_1L_chr = NA_character_))
ScorzAqol6 <- methods::setClass("ScorzAqol6",
                                contains = "ScorzProfile",
                                slots = c(a_YouthvarsProfile = "YouthvarsProfile",
                                          domain_unwtd_var_nms_chr = "character",
                                          domain_wtd_var_nms_chr = "character",
                                          instrument_dict_r3 = "ready4use_dictionary",
                                          instrument_nm_1L_chr = "character",
                                          itm_labels_chr = "character",
                                          itm_prefix_1L_chr =  "character",
                                          scrg_dss_ls = "list",
                                          total_wtd_var_nm_1L_chr = "character",
                                          total_unwtd_var_nm_1L_chr = "character"),
                                prototype = list(a_YouthvarsProfile = YouthvarsProfile(),
                                                 domain_unwtd_var_nms_chr = NA_character_,
                                                 domain_wtd_var_nms_chr = paste0("vD",1:6),
                                                 instrument_dict_r3 = youthvars::aqol_scrg_dict_r3,
                                                 instrument_nm_1L_chr = "Assessment of Quality of Life (6 Dimension)",
                                                 itm_labels_chr = c("Household tasks", "Getting around",
                                                                    "Morbility","Self care","Enjoy close rel\'s",
                                                                    "Family rel\'s", "Community involv\'t",
                                                                    "Despair","Worry", "Sad", "Agitated",
                                                                    "Energy level", "Control", "Coping",
                                                                    "Frequency of pain", "Degree of pain",
                                                                    "Pain interference","Vision", "Hearing",
                                                                    "Communication"),
                                                 itm_prefix_1L_chr =  "aqol6d_q",
                                                 scrg_dss_ls = list(),
                                                 total_wtd_var_nm_1L_chr = "aqol6d_total_w",
                                                 total_unwtd_var_nm_1L_chr = NA_character_))
ScorzAqol6Adol <- methods::setClass("ScorzAqol6Adol",
                                    contains = "ScorzAqol6",
                                    prototype = list(instrument_nm_1L_chr = "Assessment of Quality of Life (6 Dimension, Adolescent Version)"))

depict_ScorzProfile <- function(x,
                                heights_int = NA_integer_,
                                plot_rows_cols_pair_int = NA_integer_,
                                type_1L_chr = "item_by_time",
                                y_label_1L_chr = "",
                                var_idcs_int = NA_integer_,
                                ...){
  if(endsWith(type_1L_chr,"by_time") & "timepoint_var_nm_1L_chr" %in% slotNames(x@a_YouthvarsProfile)){
    if(type_1L_chr == "comp_item_by_time"){
      if(is.na(heights_int[1]))
        heights_int <- c(20L, 1L)
      if(is.na(plot_rows_cols_pair_int[1]))
        plot_rows_cols_pair_int <- c(5L,4L)
      plt <- make_itm_resp_plts(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                col_nms_chr = names(dplyr::select(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                                                  starts_with(x@itm_prefix_1L_chr))),
                                lbl_nms_chr = x@itm_labels_chr,
                                plot_rows_cols_pair_int = plot_rows_cols_pair_int,
                                heights_int = heights_int,
                                round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr,# CONDITIONAL
                                y_label_1L_chr = y_label_1L_chr,
                                ...)

    }
    if(type_1L_chr == "comp_domain_by_time"){
      if(is.na(heights_int[1]))
        heights_int <- c(10L, 1L)
      if(is.na(plot_rows_cols_pair_int[1]))
        plot_rows_cols_pair_int <- c(3L,2L)
      plt <- make_sub_tot_plts(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                               col_nms_chr = x@domain_wtd_var_nms_chr,
                               plot_rows_cols_pair_int = plot_rows_cols_pair_int,
                               round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr,
                               heights_int = heights_int,
                               y_label_1L_chr = y_label_1L_chr,
                               ...)
    }
    if(type_1L_chr %in% c("domain_by_time","item_by_time","total_by_time")){
      if(type_1L_chr == "item_by_time"){
        var_nms_chr <- names(dplyr::select(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                           starts_with(x@itm_prefix_1L_chr)))
      }
      if(type_1L_chr == "domain_by_time"){
        var_nms_chr <- x@domain_wtd_var_nms_chr
      }
      if(type_1L_chr == "total_by_time"){
        var_nms_chr <- c(x@total_wtd_var_nm_1L_chr,x@total_unwtd_var_nm_1L_chr) %>%
          purrr::discard(is.na)
      }
      if(is.na(var_idcs_int[1]))
        var_idcs_int <- 1:length(var_nms_chr)
      var_nms_chr[var_idcs_int] %>%
        purrr::map(~ depict(x@a_YouthvarsProfile,
                            type_1L_chr = "by_time",
                            var_nms_chr = .x))
    }
  }
}
methods::setMethod("depict",
                   methods::className("ScorzProfile"#, package = "ready4use"
                   ),
                   depict_ScorzProfile)
renew_ScorzAqol6Adol <- function(x,
                                 type_1L_chr = "score"){
  if(type_1L_chr == "score"){ ## PICK UP HERE
    scored_data_tb <- add_adol6d_scores(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                        prefix_1L_chr =  x@itm_prefix_1L_chr,
                                        id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr,
                                        wtd_aqol_var_nm_1L_chr = x@total_wtd_var_nm_1L_chr)
    dictionary_r3 <- ready4::renew(x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3,
                                   new_ready4_dict_r3 = x@instrument_dict_r3)
    scored_data_tb <- scored_data_tb %>%
      ready4use::add_labels_from_dictionary(dictionary_r3)
    x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb <- scored_data_tb
    x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3 <- dictionary_r3
  }
  return(x)
}
methods::setMethod("renew",
                   methods::className("ScorzAqol6Adol"#, package = "ready4use"
                   ),
                   renew_ScorzAqol6Adol)

###
x <- ready4use::Ready4useRepos(dv_nm_1L_chr = "fakes",
                               dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
                               dv_server_1L_chr = "dataverse.harvard.edu") %>%
  ingest(fls_to_ingest_chr = "ymh_clinical_dyad_r4") # MAKE DEFAULT PROCURE METHOD IF RETURN FILE ==T
x <- procure(procureSlot(x,"b_Ready4useIngest"), # MAKE DEFAULT PROCURE METHOD FOR RECORD IF FL NM PROVIDED
             fl_nm_1L_chr = "ymh_clinical_dyad_r4")
exhibit(x)
y <- youthvars::YouthvarsProfile(a_Ready4useDyad = x,
                                 id_var_nm_1L_chr = "fkClientID")
z <- ScorzAqol6Adol(a_YouthvarsProfile = y)
x <- procureSlot(procureSlot(z,"a_YouthvarsProfile"),
                 "a_Ready4useDyad")
exhibit(x)
depict(z, type_1L_chr = "item_by_time")
depict(z, type_1L_chr = "item_by_time", var_idcs_int = c(2L))
depict(z, type_1L_chr = "domain_by_time")
depict(z, type_1L_chr = "domain_by_time", var_idcs_int = c(1L))
depict(z, type_1L_chr = "total_by_time")
depict(z, type_1L_chr = "comp_item_by_time")
depict(z, type_1L_chr = "comp_domain_by_time")
##
## MIGRATE TO SPECIFIC
ScorzModelSpec <- methods::setClass("ScorzModelSpec", #youthvars
                                    contains = "Ready4Module",
                                    slots = c(a_ScorzProfile = "ScorzProfile",
                                              candidate_predrs_chr = "character",
                                              depnt_var_nm_1L_chr = "character",
                                              depnt_var_max_val_1L_dbl = "numeric"),
                                    prototype =  list(a_ScorzProfile = ScorzProfile(),
                                                      candidate_predrs_chr = NA_character_,
                                                      depnt_var_nm_1L_chr = NA_character_,
                                                      depnt_var_max_val_1L_dbl = Inf))
exhibit_ScorzModelSpec <- function(x,
                                   captions_chr = character(0),
                                   method_chr = c("pearson", "spearman"),
                                   mkdn_tbl_refs_chr = NULL,
                                   output_type_1L_chr = "HTML",
                                   type_1L_chr = "correlation",
                                   timepoints_int = NA_integer_){
  if(type_1L_chr == "correlation"){
    if(is.na(timepoints_int)){
      if("timepoint_vals_chr" %in% slotNames(x@a_ScorzProfile@a_YouthvarsProfile)){
        timepoints_int <- 1:length(x@a_ScorzProfile@a_YouthvarsProfile@timepoint_vals_chr) %>% as.integer()
      }else{
        timepoints_int <- 1
      }
    }
    if(identical(character(0), captions_chr)){
      captions_chr <- paste0("Correlations at ",
                             x@a_ScorzProfile@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int])
    }
    1:length(timepoints_int) %>%
      purrr::map(~
                   transform_ds_for_tstng(x@a_ScorzProfile@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                          depnt_var_nm_1L_chr = x@depnt_var_nm_1L_chr,
                                          depnt_var_max_val_1L_dbl = x@depnt_var_max_val_1L_dbl,
                                          candidate_predrs_chr = x@candidate_predrs_chr,
                                          round_var_nm_1L_chr = ifelse("timepoint_var_nm_1L_chr" %in% slotNames(x@a_ScorzProfile@a_YouthvarsProfile),
                                                                       x@a_ScorzProfile@a_YouthvarsProfile@timepoint_var_nm_1L_chr,
                                                                       NA_character_),
                                          round_val_1L_chr = ifelse("timepoint_vals_chr" %in% slotNames(x@a_ScorzProfile@a_YouthvarsProfile),
                                                                    x@a_ScorzProfile@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int[.x]],
                                                                    NA_character_))  %>%
                   make_corstars_tbl_xx(caption_1L_chr = captions_chr[.x],
                                        mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr[.x],
                                        method_chr = method_chr,
                                        result_chr = output_type_1L_chr
                   ))
  }
}
methods::setMethod("exhibit",
                   methods::className("ScorzModelSpec"#, package = "ready4use"
                   ),
                   exhibit_ScorzModelSpec)

#
