#' Impute unscored adolescent Assessment of Quality of Life Six Dimension dataset
#' @description impute_unscrd_adol_aqol6d_ds() is an Impute function that imputes data. Specifically, this function implements an algorithm to impute unscored adolescent assessment of quality of life six dimension dataset. The function returns Imputed unscored Assessment of Quality of Life dataset tibble (a tibble).
#' @param unscrd_aqol_ds_tb Unscored Assessment of Quality of Life dataset (a tibble)
#' @return Imputed unscored Assessment of Quality of Life dataset tibble (a tibble)
#' @rdname impute_unscrd_adol_aqol6d_ds
#' @export 
#' @importFrom dplyr mutate select filter
#' @importFrom mice mice complete
#' @keywords internal
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
