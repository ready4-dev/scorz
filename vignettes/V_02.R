## ----warning=FALSE, message=FALSE---------------------------------------------
library(scorz)

## -----------------------------------------------------------------------------
X <- ready4use::Ready4useRepos(dv_nm_1L_chr = "fakes",
                               dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
                               dv_server_1L_chr = "dataverse.harvard.edu") %>%
  ingest(fls_to_ingest_chr = "ymh_eq5d_dyad_r4",
         metadata_1L_lgl = F) 
X <- youthvars::YouthvarsSeries(a_Ready4useDyad = X,
                                id_var_nm_1L_chr = "uid",
                                timepoint_var_nm_1L_chr = "Timepoint",
                                timepoint_vals_chr = unique(X@ds_tb$Timepoint))

## -----------------------------------------------------------------------------
Y <- ScorzEuroQol5(a_YouthvarsProfile = X,
                   country_1L_chr = "UK",
                   instrument_version_1L_chr = "5L",
                   itm_var_nms_chr = c("eq5dq_MO", "eq5dq_SC", "eq5dq_UA", "eq5dq_PD", "eq5dq_AD"))

## ----results='hide', warning=F, message=FALSE---------------------------------
Y <- renew(Y)

## -----------------------------------------------------------------------------
exhibit(Y,
        display_1L_chr = "head")

## ----adfig, fig.cap="EQ-5D Anxiety / Depression Dimension scores by time", out.width = "600px"----
depict(Y, type_1L_chr = "domain_by_time", var_idcs_int = 5L)

## ----totalsfig, fig.cap="EQ-5D total weighted utility scores by time", out.width = "600px"----
depict(Y, type_1L_chr = "total_by_time", var_idcs_int = 1L)

## ----domainsfig, fig.cap="EQ-5D weighted domain scores by time", out.width = "600px", results='hide', fig.keep='all'----
depict(Y, type_1L_chr = "comp_domain_by_time")

## ----eval=F-------------------------------------------------------------------
#  Z <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/scorz", # Replace with details of your repo.
#                                 gh_tag_1L_chr = "Documentation_0.0") # You must have write permissions.
#  Z <- share(Z,
#             obj_to_share_xx = Y,
#             fl_nm_1L_chr = "ymh_ScorzEuroQol5")

