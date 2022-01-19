## ----warning=FALSE, message=FALSE---------------------------------------------
library(scorz)

## -----------------------------------------------------------------------------
X <- ready4use::Ready4useRepos(dv_nm_1L_chr = "fakes",
                               dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
                               dv_server_1L_chr = "dataverse.harvard.edu") %>%
  ingest(fls_to_ingest_chr = "ymh_clinical_dyad_r4",
         metadata_1L_lgl = F) 

## -----------------------------------------------------------------------------
X <- X %>%
  renew(type_1L_chr = "label")

## -----------------------------------------------------------------------------
exhibit(X,
        display_1L_chr = "head")

## -----------------------------------------------------------------------------
X <- youthvars::YouthvarsSeries(a_Ready4useDyad = X,
                                id_var_nm_1L_chr = "fkClientID",
                                timepoint_var_nm_1L_chr = "round",
                                timepoint_vals_chr = levels(X@ds_tb$round))

## -----------------------------------------------------------------------------
Y <- ScorzAqol6Adol(a_YouthvarsProfile = X)

## -----------------------------------------------------------------------------
procureSlot(Y,
            slot_nm_1L_chr = "itm_prefix_1L_chr")


## -----------------------------------------------------------------------------
# Not run
# Y <- renewSlot(Y, slot_nm_1L_chr = "itm_prefix_1L_chr", new_val_xx = "new_prefix")

