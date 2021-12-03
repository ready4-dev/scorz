renew_ScorzAqol6Adol <- function(x,
                                 label_ds_1L_lgl = T,
                                 type_1L_chr = "score"){
  if(type_1L_chr == "score"){ ## PICK UP HERE
    if(identical(x@scrg_dss_ls,list(list()))){
      x@scrg_dss_ls <- get_aqol6d_scrg_dss()
    }
    y <- x@a_YouthvarsProfile@a_Ready4useDyad
    y <- renew(y, type_1L_chr = "unlabel")
    select_chr <- setdiff(names(y@ds_tb),
                          x@instrument_dict_r3$var_nm_chr[!x@instrument_dict_r3$var_nm_chr %>%
                                                            startsWith(x@itm_prefix_1L_chr)] %>%
                            as.vector())
    y@ds_tb <- y@ds_tb %>%
      dplyr::select(tidyselect::all_of(select_chr))
    y@ds_tb <- add_adol6d_scores(y@ds_tb, # Make an enhanceSlot method - then generalise renew mthd to parent class
                                 aqol6d_scrg_dss_ls = x@scrg_dss_ls,
                                 prefix_1L_chr =  x@itm_prefix_1L_chr,
                                 id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr,
                                 total_aqol_var_nm_1L_chr = x@total_unwtd_var_nm_1L_chr,
                                 wtd_aqol_var_nm_1L_chr = x@total_wtd_var_nm_1L_chr)
    y@dictionary_r3 <- ready4::renew(y@dictionary_r3,
                                     new_ready4_dict_r3 = x@instrument_dict_r3)
    if(label_ds_1L_lgl)
      y <- renew(y)
    x@a_YouthvarsProfile@a_Ready4useDyad <- y
  }
  return(x)
}
