## ----warning=FALSE, message=FALSE---------------------------------------------
library(scorz)

## -----------------------------------------------------------------------------
x <- ready4use::Ready4useRepos(dv_nm_1L_chr = "fakes",
                               dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
                               dv_server_1L_chr = "dataverse.harvard.edu") %>%
  ingest(fls_to_ingest_chr = "ymh_clinical_dyad_r4") %>%
  procureSlot("b_Ready4useIngest") %>%
  procure(fl_nm_1L_chr = "ymh_clinical_dyad_r4")


## -----------------------------------------------------------------------------
exhibit(x)

## -----------------------------------------------------------------------------
y <- youthvars::YouthvarsSeries(a_Ready4useDyad = x,
                                id_var_nm_1L_chr = "fkClientID",
                                timepoint_var_nm_1L_chr = "round",
                                timepoint_vals_chr = levels(x@ds_tb$round))

## -----------------------------------------------------------------------------
z <- ScorzAqol6Adol(a_YouthvarsProfile = y)#,itm_prefix_1L_chr = "aqol6d_q"

## -----------------------------------------------------------------------------
z@itm_prefix_1L_chr

## -----------------------------------------------------------------------------
# Not run
# z <- renewSlot(z, slot_nm_1L_chr = "itm_prefix_1L_chr", new_val_xx = "new_prefix")

## ----results='hide', warning=F, message=FALSE---------------------------------
z <- renew(z)

## -----------------------------------------------------------------------------
x <- procureSlot(procureSlot(z,"a_YouthvarsProfile"),
                 "a_Ready4useDyad")

## -----------------------------------------------------------------------------
exhibit(x)

## -----------------------------------------------------------------------------
depict(z, type_1L_chr = "item_by_time")

## -----------------------------------------------------------------------------
depict(z, type_1L_chr = "item_by_time", var_idcs_int = c(2L))

## -----------------------------------------------------------------------------
depict(z, type_1L_chr = "domain_by_time")


## -----------------------------------------------------------------------------
depict(z, type_1L_chr = "domain_by_time", var_idcs_int = c(1L))

## -----------------------------------------------------------------------------
depict(z, type_1L_chr = "total_by_time")


