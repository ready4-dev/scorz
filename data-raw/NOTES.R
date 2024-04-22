#
library(ready4use)
X <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
abbreviations_lup <- Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>% ready4fun::renew.ready4fun_abbreviations(short_name_chr = c("aqol4d", "phq4"),
                                                                                               long_name_chr = c("AQoL-4D","PHQ-4"),
                                                                                               plural_lgl = c(F,F))

Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(abbreviations_lup = abbreviations_lup)),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")
