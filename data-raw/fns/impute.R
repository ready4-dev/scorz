impute_unscrd_adol_aqol6d_ds <- function (unscrd_aqol_ds_tb)
{
  unscrd_aqol_ds_tb <- unscrd_aqol_ds_tb %>% dplyr::mutate(missing = rowSums(is.na(dplyr::select(.,
                                                                                                 paste0("Q", c(1:10))))))
  aqol_cases_to_imp_tb <- unscrd_aqol_ds_tb %>% dplyr::filter(missing <
                                                                10) %>% dplyr::select(-missing)
  aqol_cases_not_to_imp_tb <- unscrd_aqol_ds_tb %>% dplyr::filter(missing >=
                                                                    10) %>% dplyr::select(-missing)
  imputed_aqol_tb <- mice::mice(aqol_cases_to_imp_tb, m = 1,
                                maxit = 50, meth = "pmm", seed = 1234, printFlag = F)
  aqol_cases_to_imp_tb <- mice::complete(imputed_aqol_tb, "long") %>%
    dplyr::select(-.imp, -.id)
  imputed_unscrd_aqol_ds_tb_tb <- data.frame(rbind(aqol_cases_to_imp_tb,
                                                   aqol_cases_not_to_imp_tb))
  return(imputed_unscrd_aqol_ds_tb_tb)
}
