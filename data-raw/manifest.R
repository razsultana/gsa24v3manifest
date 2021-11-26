stopifnot(.Platform$OS.type == "unix") # command line tools used later

# ======================================================
# InfiniumOmni5-4v1-2_A1_b144_rsids.txt
# ======================================================
# From <https://support.illumina.com/array/array_kits/humanomni5-4-beadchip-kit/downloads.html> 
#   -> "Infinium Omni5-4 v1.2 Support Files" 
#   -> "Infinium Omni5-4 v1.2 Loci Name to rsID Conversion File"
# `InfiniumOmni5-4v1-2_A1_b144_rsids.txt`
# Direct link:
rsid_url <- "https://support.illumina.com/content/dam/illumina-support/documents/downloads/productfiles/humanomni5-4/v1-2/infinium-omni5-4-v1-2-a1-b144-rsids.zip"
rsid_tmpfile <- tempfile()
download.file(url = rsid_url, destfile = rsid_tmpfile)

rsid_dir <- tempdir()
unzip(zipfile = rsid_tmpfile, exdir = rsid_dir)

#rsid_dir <- "/tmp/RtmpZEwja4"
rsid_path <- file.path(rsid_dir, "InfiniumOmni5-4v1-2_A1_b144_rsids.txt")
stopifnot(file.exists(rsid_path))

stopifnot(isTRUE(all.equal(file.size(rsid_path), 95486222L)))

file.remove(rsid_tmpfile)

# ======================================================
# InfiniumOmni5-4v1-2_A1_MappingComment.txt
# ======================================================
# From <https://support.illumina.com/array/array_kits/humanomni5-4-beadchip-kit/downloads.html> 
#   -> "Infinium Omni5-4 v1.2 Support Files" 
#   -> "Infinium Omni5-4 v1.2 Mapping Comments File"
# `InfiniumOmni5-4v1-2_A1_MappingComment.txt`
# Direct link:
mappingcomments_url <- "https://support.illumina.com/content/dam/illumina-support/documents/downloads/productfiles/humanomni5-4/v1-2/infinium-omni5-4-v1-2-a1-mapping-comment.zip"
mappingcomments_tmpfile <- tempfile()
download.file(url = mappingcomments_url, destfile = mappingcomments_tmpfile)

mappingcomments_dir <- tempdir()
unzip(zipfile = mappingcomments_tmpfile, exdir = mappingcomments_dir)

#mappingcomments_dir
mappingcomments_path <- file.path(mappingcomments_dir, "InfiniumOmni5-4v1-2_A1_MappingComment.txt")
stopifnot(file.exists(mappingcomments_path))

stopifnot(isTRUE(all.equal(file.size(mappingcomments_path), 52903704L)))

file.remove(mappingcomments_tmpfile)


# ======================================================
# InfiniumOmni5-4v1-2_A2.csv
# ======================================================
# From <https://support.illumina.com/array/array_kits/humanomni5-4-beadchip-kit/downloads.html> 
#   -> "Infinium Omni5-4 v1.2 Product Files" 
#   -> "Infinium Omni5-4 v1.2 Manifest File (CSV Format - GRCh38)"
# `InfiniumOmni5-4v1-2_A2.csv`


manifest_url <- "ftp://webdata2:webdata2@ussd-ftp.illumina.com/downloads/productfiles/infinium-omni5-4/v1-2/infinium-omni5-4-v1-2-a2-manifest-file-csv.zip"
manifest_tmpfile <- tempfile()

old_opts <- options(timeout = 1000)
download.file(url = manifest_url, destfile = manifest_tmpfile)
options(old_opts)

manifest_dir <- tempdir()
unzip(zipfile = manifest_tmpfile, exdir = manifest_dir)

#manifest_dir <- "/tmp/RtmpZEwja4"
manifest_path <- file.path(manifest_dir, "InfiniumOmni5-4v1-2_A2.csv")
stopifnot(file.exists(manifest_path))

stopifnot(isTRUE(all.equal(file.size(manifest_path), 1862274220L)))

#file.remove(manifest_tmpfile)


# Remove '[Controls]' section
cmd <- paste0('cd ', manifest_dir, ' && csplit --prefix=MANIFEST InfiniumOmni5-4v1-2_A2.csv /\\\\[Controls\\\\]/')
system(cmd)
stopifnot(file.exists(file.path(manifest_dir, "MANIFEST00")))
stopifnot(file.exists(file.path(manifest_dir, "MANIFEST01")))
stopifnot(!file.exists(file.path(manifest_dir, "MANIFEST02")))

manifest_probes_path <- file.path(manifest_dir, "MANIFEST00")
stopifnot(isTRUE(all.equal(file.size(manifest_probes_path), 1862271459L)))

# Remove header:
cmd <- paste0("cd ", manifest_dir, " && sed -i '1,7d' MANIFEST00")
system(cmd)
stopifnot(isTRUE(all.equal(file.size(manifest_probes_path), 1862271292L)))

# Controls:
#sed -i '1d' xx01
#echo 'IlmnID,Name,IlmnStrand,SNP,AddressA_ID,AlleleA_ProbeSeq,AddressB_ID,AlleleB_ProbeSeq,GenomeBuild,Chr,MapInfo,Ploidy,Species,Source,SourceVersion,SourceStrand,SourceSeq,TopGenomicSeq,BeadSetID,Exp_Clusters,RefStrand' | cat - xx01 > temp && mv temp xx01 


###########################
# 

if (FALSE) {
  rsid_dir <- "/tmp/RtmpZEwja4"
  rsid_path <- file.path(rsid_dir, "InfiniumOmni5-4v1-2_A1_b144_rsids.txt")
  
  manifest_dir <- rsid_dir
  manifest_probes_path <- file.path(manifest_dir, "MANIFEST00")
}

library(dplyr)
library(tidyr)
library(tibble)
library(data.table)
library(here)

manifest_raw <- data.table::fread(manifest_probes_path, sep = ",")

manifest <- manifest_raw %>% 
  as_tibble() %>% 
  mutate(TopGenomicSeqSBE = gsub("^.*(.{1}\\[.*\\].{1}).*$", "\\1", TopGenomicSeq),
         TopGenomicSeqSBE_Left = gsub("^(.{1})\\[.*$", "\\1", TopGenomicSeqSBE),
         TopGenomicSeqSBE_Right = gsub("^.*\\](.{1})*$", "\\1", TopGenomicSeqSBE)) %>% 
  select(IlmnID, Name, SNP, IlmnStrand, SourceStrand, 
         TopGenomicSeqSBE, TopGenomicSeqSBE_Left, TopGenomicSeqSBE_Right, 
         AddressA_ID, AddressB_ID, 
         GenomeBuild, Chr, MapInfo, BeadSetID, Exp_Clusters, RefStrand) %>% 
  mutate(AddressA_ID = as.character(AddressA_ID),
         AddressB_ID = as.character(AddressB_ID)) %>% 
  mutate(ProbeType = ifelse(is.na(AddressB_ID), "II", "I")) %>% 
  mutate(SNPType = case_when(
    SNP == "[A/T]" ~ "AMB",
    SNP == "[T/A]" ~ "AMB",
    SNP == "[G/C]" ~ "AMB",
    SNP == "[C/G]" ~ "AMB",
    SNP == "[D/I]" ~ "INDEL",
    SNP == "[I/D]" ~ "INDEL",
    TRUE ~ "UNAMB"
  )) %>% 
  mutate(Exclude = case_when(
    Name %in% c("rs28362918", "rs28897688") ~ TRUE,
    TRUE ~ FALSE
  ))

rsno <- data.table::fread(rsid_path, header = TRUE, sep = "\t") %>%
  as_tibble()

stopifnot(nrow(anti_join(manifest, rsno, by = "Name")) == 0L)
stopifnot(nrow(anti_join(rsno, manifest, by = "Name")) == 0L)
stopifnot(nrow(manifest) == 4327108L)

manifest <- manifest %>% 
  inner_join(rsno, by = "Name")

stopifnot(nrow(manifest) == 4327108L)

###########

mappingcomments <- data.table::fread(mappingcomments_path, header = TRUE, sep = "\t") %>%
  as_tibble()

stopifnot(nrow(anti_join(manifest, mappingcomments, by = "Name")) == 0L)
stopifnot(nrow(anti_join(mappingcomments, manifest, by = "Name")) == 0L)
stopifnot(nrow(manifest) == 4327108L)

manifest <- manifest %>% 
  inner_join(mappingcomments, by = "Name")

stopifnot(nrow(manifest) == 4327108L)


#manifest %>% filter(!grepl("^[0-9]+$", Name)) %>% 
#  select(IlmnID, Name, RsID)

manifest <- manifest %>% 
  select(Name, IlmnID, RsID, Chr, MapInfo, AddressA_ID, AddressB_ID, GenomeBuild, 
         SNP, everything()) 
class(manifest) <- "data.frame"

stopifnot(length(unique(manifest$Name)) == nrow(manifest))

#manifest_I <- subset(manifest, ProbeType == "I")

#usethis::use_data(manifest, overwrite = TRUE, version = 3, compress = "bzip2")

#cat(paste0(paste0("#'   \\item{", colnames(manifest), "}{}\n"), collapse = ""))

#subset(manifest, Exclude == TRUE)

dest <- here("inst", "extdata", "manifest.csv")

chunks <- 20
#chunk_idx <- rep(seq_len(chunks), length.out = nrow(manifest))
chunk_idx <- gl(n = chunks, k = ceiling(nrow(manifest)/chunks), length = nrow(manifest))
head(chunk_idx)
tail(chunk_idx)
table(chunk_idx)

stopifnot(length(chunk_idx) == nrow(manifest))

manifest_chunks <- split(manifest, chunk_idx)

k <- nchar(as.character(chunks)) + 1L

unlink(here("inst", "extdata"), recursive = TRUE)
dir.create(here("inst", "extdata"))
for (i in seq_along(manifest_chunks)) {
  dest <- here("inst", "extdata", paste0("manifest-", sprintf(paste0("%0", k, "d"), i), ".csv"))
  data.table::fwrite(x = manifest_chunks[[i]], file = dest, sep = ";")
}

#data.table::fwrite(x = manifest, file = dest, sep = ";")
