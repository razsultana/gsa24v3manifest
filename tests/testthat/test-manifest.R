manifest <- load_manifest()

test_that("Number of probes", {
  expect_equal(nrow(manifest), 654027L)
})


test_that("Number of columns", {
  expect_equal(ncol(manifest), 21L)
})

if (FALSE) {
  
  manifest
  
  manifest %>% pull(TopGenomicSeqSBE) %>% nchar() %>% table()
  manifest %>% pull(TopGenomicSeqSBE_Left) %>% nchar() %>% table()
  manifest %>% pull(TopGenomicSeqSBE_Right) %>% nchar() %>% table()
  
  manifest %>% count(nchar(TopGenomicSeqSBE), SNP) %>% print(n = Inf)
  manifest %>% filter(nchar(TopGenomicSeqSBE) == 7, SNP == "[D/I]")
  
  manifest %>% count(SNP, IlmnStrand) %>% print(n = Inf)
  
  
  manifest %>% count(SNPType)
  manifest %>% count(SNP, SNPType)
  
  manifest %>% count(SNPType, ProbeType)
  
  manifest %>% filter(SNPType == "AMB", ProbeType == "II")
  
  manifest %>% filter(SNPType == "AMB", ProbeType == "II") %>% pull(Name)
  
  
  Name %in% c("rs28362918", "rs28897688")
  
  manifest
  manifest %>% count(Exclude)
  manifest %>% count(Exclude, SNPType, ProbeType)

}
