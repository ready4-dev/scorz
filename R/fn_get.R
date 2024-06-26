#' Get Assessment of Quality of Life Six Dimension scoring dictionary
#' @description get_aqol6d_scrg_dict() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get assessment of quality of life six dimension scoring dictionary. The function is called for its side effects and does not return a value.

#' @return No return value, called for side effects.
#' @rdname get_aqol6d_scrg_dict
#' @export 
#' @importFrom ready4 ingest procureSlot procure
#' @importFrom ready4use Ready4useRepos
#' @keywords internal
get_aqol6d_scrg_dict <- function () 
{
    ready4::ingest(ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/scorz", 
        gh_tag_1L_chr = "Documentation_0.0"), fls_to_ingest_chr = c("aqol_scrg_dict_r3")) %>% 
        ready4::procureSlot("b_Ready4useIngest") %>% ready4::procure(fl_nm_1L_chr = "aqol_scrg_dict_r3")
}
#' Get Assessment of Quality of Life Six Dimension scoring datasets
#' @description get_aqol6d_scrg_dss() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get assessment of quality of life six dimension scoring datasets. The function returns Assessment of Quality of Life Six Dimension scoring datasets (a list).

#' @return Assessment of Quality of Life Six Dimension scoring datasets (a list)
#' @rdname get_aqol6d_scrg_dss
#' @export 
#' @importFrom ready4 ingest procureSlot procure
#' @importFrom ready4use Ready4useRepos
#' @keywords internal
get_aqol6d_scrg_dss <- function () 
{
    aqol6d_scrg_dss_ls <- ready4::ingest(ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/scorz", 
        gh_tag_1L_chr = "Documentation_0.0"), fls_to_ingest_chr = c("aqol6d_scrg_dss_ls")) %>% 
        ready4::procureSlot("b_Ready4useIngest") %>% ready4::procure(fl_nm_1L_chr = "aqol6d_scrg_dss_ls")
    return(aqol6d_scrg_dss_ls)
}
