#' 
#' Depict (plot) features of model module data
#' @name depict-ScorzProfile
#' @description depict method applied to ScorzProfile
#' @param x An object of class ScorzProfile
#' @param heights_int Heights (an integer vector), Default: NA
#' @param plot_rows_cols_pair_int Plot rows columns pair (an integer vector), Default: NA
#' @param tfmn_1L_chr Transformation (a character vector of length one), Default: 'title'
#' @param type_1L_chr Type (a character vector of length one), Default: 'item_by_time'
#' @param y_label_1L_chr Y label (a character vector of length one), Default: ''
#' @param var_idcs_int Variable idcs (an integer vector), Default: NA
#' @param ... Additional arguments
#' @return Plot (an output object of multiple potential types)
#' @rdname depict-methods
#' @aliases depict,ScorzProfile-method
#' @export 
#' @importFrom ready4 renew depict
#' @importFrom youthvars make_itm_resp_plts make_sub_tot_plts
#' @importFrom dplyr select
#' @importFrom purrr discard map
methods::setMethod("depict", "ScorzProfile", function (x, heights_int = NA_integer_, plot_rows_cols_pair_int = NA_integer_, 
    tfmn_1L_chr = "title", type_1L_chr = "item_by_time", y_label_1L_chr = "", 
    var_idcs_int = NA_integer_, ...) 
{
    y <- procureSlot(procureSlot(x, "a_YouthvarsProfile"), "a_Ready4useDyad") %>% 
        ready4::renew(tfmn_1L_chr = tfmn_1L_chr) %>% ready4::renew(tfmn_1L_chr = tfmn_1L_chr, 
        type_1L_chr = "case")
    if (endsWith(type_1L_chr, "by_time") & "timepoint_var_nm_1L_chr" %in% 
        slotNames(x@a_YouthvarsProfile)) {
        if (type_1L_chr == "comp_item_by_time") {
            if (is.na(heights_int[1])) 
                heights_int <- c(20L, 1L)
            if (is.na(plot_rows_cols_pair_int[1])) 
                plot_rows_cols_pair_int <- c(5L, 4L)
            plt_xx <- youthvars::make_itm_resp_plts(y@ds_tb, 
                col_nms_chr = names(dplyr::select(y@ds_tb, starts_with(x@itm_prefix_1L_chr))), 
                lbl_nms_chr = x@itm_labels_chr, plot_rows_cols_pair_int = plot_rows_cols_pair_int, 
                heights_int = heights_int, round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr, 
                y_label_1L_chr = y_label_1L_chr, ...)
        }
        if (type_1L_chr == "comp_domain_by_time") {
            if (is.na(heights_int[1])) 
                heights_int <- c(10L, 1L)
            if (is.na(plot_rows_cols_pair_int[1])) 
                plot_rows_cols_pair_int <- c(3L, 2L)
            plt_xx <- youthvars::make_sub_tot_plts(y@ds_tb, col_nms_chr = x@domain_wtd_var_nms_chr, 
                plot_rows_cols_pair_int = plot_rows_cols_pair_int, 
                round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr, 
                heights_int = heights_int, y_label_1L_chr = y_label_1L_chr, 
                ...)
        }
        if (type_1L_chr %in% c("domain_by_time", "item_by_time", 
            "total_by_time")) {
            if (type_1L_chr == "item_by_time") {
                var_nms_chr <- names(dplyr::select(y@ds_tb, starts_with(x@itm_prefix_1L_chr)))
            }
            if (type_1L_chr == "domain_by_time") {
                var_nms_chr <- x@domain_wtd_var_nms_chr
            }
            if (type_1L_chr == "total_by_time") {
                var_nms_chr <- c(x@total_wtd_var_nm_1L_chr, x@total_unwtd_var_nm_1L_chr) %>% 
                  purrr::discard(is.na)
            }
            if (is.na(var_idcs_int[1])) 
                var_idcs_int <- 1:length(var_nms_chr)
            x@a_YouthvarsProfile@a_Ready4useDyad <- y
            plt_xx <- var_nms_chr[var_idcs_int] %>% purrr::map(~depict(procureSlot(x, 
                "a_YouthvarsProfile"), type_1L_chr = "by_time", 
                var_nms_chr = .x))
            if (length(var_idcs_int) == 1) 
                plt_xx <- plt_xx[[1]]
        }
    }
    plt_xx
    return(plt_xx)
})
