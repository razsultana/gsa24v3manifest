stopifnot(.Platform$OS.type == "unix") # command line tools used later

# ======================================================
# GSA-24v3-0_A1_b151_rsids.txt
# ======================================================
# From <https://sapac.support.illumina.com/array/array_kits/infinium-global-screening-array/downloads.html> 
#   -> "Infinium Global Screening Array v3.0 Support Files" 
#   -> "Infinium Global Screening Array v3.0 Loci Name to rsID Conversion File"
# `GSA-24v3-0_A1_b151_rsids.txt`
# Direct link:
rsid_url <- "https://sapac.support.illumina.com/content/dam/illumina-support/documents/downloads/productfiles/global-screening-array-24/v3-0/infinium-global-screening-array-24-v3-0-a1-b151-rsids.zip"
rsid_tmpfile <- tempfile()
download.file(url = rsid_url, destfile = rsid_tmpfile)

rsid_dir <- tempdir()
unzip(zipfile = rsid_tmpfile, exdir = rsid_dir)

#rsid_dir <- "/tmp/RtmpZEwja4"
rsid_path <- file.path(rsid_dir, "GSA-24v3-0_A1_b151_rsids.txt")
stopifnot(file.exists(rsid_path))

stopifnot(isTRUE(all.equal(file.size(rsid_path), 15480783L)))

file.remove(rsid_tmpfile)

# ======================================================
# GSA-24v3-0_A1_MappingComment.txt
# ======================================================
# From <https://sapac.support.illumina.com/array/array_kits/infinium-global-screening-array/downloads.html> 
#   -> "Infinium Global Screening Array v3.0 Support Files" 
#   -> "Infinium Global Screening Array v3.0 Mapping Comments"
# `GSA-24v3-0_A1_MappingComment.txt`
# Direct link:
mappingcomments_url <- "https://sapac.support.illumina.com/content/dam/illumina-support/documents/downloads/productfiles/global-screening-array-24/v3-0/infinium-global-screening-array-24-v3-0-a1-mapping-comment.zip"
mappingcomments_tmpfile <- tempfile()
download.file(url = mappingcomments_url, destfile = mappingcomments_tmpfile)

mappingcomments_dir <- tempdir()
unzip(zipfile = mappingcomments_tmpfile, exdir = mappingcomments_dir)

#mappingcomments_dir
mappingcomments_path <- file.path(mappingcomments_dir, "GSA-24v3-0_A1_MappingComment.txt")
stopifnot(file.exists(mappingcomments_path))

stopifnot(isTRUE(all.equal(file.size(mappingcomments_path), 9084079L)))

file.remove(mappingcomments_tmpfile)


# ======================================================
# GSA-24v3-0_A2.csv
# ======================================================
# From <https://sapac.support.illumina.com/array/array_kits/infinium-global-screening-array/downloads.html> 
#   -> "Infinium Global Screening Array v2.0 Product Files" 
#   -> "Infinium Global Screening Array v2.0 Manifest File (CSV Format - GRCh38)"
# `GSA-24v3-0_A2.csv`


manifest_url <- "https://sapac.support.illumina.com/content/dam/illumina-support/documents/downloads/productfiles/global-screening-array-24/v3-0/GSA-24v3-0-A2-manifest-file-csv.zip"
manifest_tmpfile <- tempfile()

old_opts <- options(timeout = 1000)
download.file(url = manifest_url, destfile = manifest_tmpfile)
options(old_opts)

manifest_dir <- tempdir()
unzip(zipfile = manifest_tmpfile, exdir = manifest_dir)

#manifest_dir <- "/tmp/RtmpZEwja4"
manifest_path <- file.path(manifest_dir, "GSA-24v3-0_A2.csv")
stopifnot(file.exists(manifest_path))

stopifnot(isTRUE(all.equal(file.size(manifest_path), 284225383L)))

file.remove(manifest_tmpfile)


# Remove '[Controls]' section
cmd <- paste0('cd ', manifest_dir, ' && csplit -f MANIFEST GSA-24v3-0_A2.csv /\\\\[Controls\\\\]/')
system(cmd)
stopifnot(file.exists(file.path(manifest_dir, "MANIFEST00")))
stopifnot(file.exists(file.path(manifest_dir, "MANIFEST01")))
stopifnot(!file.exists(file.path(manifest_dir, "MANIFEST02")))

manifest_probes_path <- file.path(manifest_dir, "MANIFEST00")
stopifnot(isTRUE(all.equal(file.size(manifest_probes_path), 284223634L)))

# Remove header:
cmd <- paste0("cd ", manifest_dir, " && sed -i '.orig' '1,7d' MANIFEST00")
system(cmd)
stopifnot(isTRUE(all.equal(file.size(manifest_probes_path), 284223483L)))

# ======================================================
# GSA-24v3-0_A1.csv
# ======================================================
# From <https://sapac.support.illumina.com/array/array_kits/infinium-global-screening-array/downloads.html> 
#   -> "Infinium Global Screening Array v2.0 Product Files" 
#   -> "Infinium Global Screening Array v2.0 Manifest File (CSV Format - GRCh37)"
# `GSA-24v3-0_A1.csv`


manifest_url <- "https://sapac.support.illumina.com/content/dam/illumina-support/documents/downloads/productfiles/global-screening-array-24/v3-0/GSA-24v3-0-A1-manifest-file-csv.zip"
manifest_tmpfile <- tempfile()

old_opts <- options(timeout = 1000)
download.file(url = manifest_url, destfile = manifest_tmpfile)
options(old_opts)

unzip(zipfile = manifest_tmpfile, exdir = manifest_dir)

#manifest_dir <- "/tmp/RtmpZEwja4"
manifest_path <- file.path(manifest_dir, "GSA-24v3-0_A1.csv")
stopifnot(file.exists(manifest_path))

stopifnot(isTRUE(all.equal(file.size(manifest_path), 284228929L)))

file.remove(manifest_tmpfile)


# Remove '[Controls]' section
cmd <- paste0('cd ', manifest_dir, ' && csplit -f MANIFEST1 GSA-24v3-0_A1.csv /\\\\[Controls\\\\]/')
system(cmd)
stopifnot(file.exists(file.path(manifest_dir, "MANIFEST100")))
stopifnot(file.exists(file.path(manifest_dir, "MANIFEST101")))
stopifnot(!file.exists(file.path(manifest_dir, "MANIFEST102")))

manifest_probes_path1 <- file.path(manifest_dir, "MANIFEST100")
stopifnot(isTRUE(all.equal(file.size(manifest_probes_path1), 284227180L)))

# Remove header:
cmd <- paste0("cd ", manifest_dir, " && sed -i '.orig' '1,7d' MANIFEST100")
system(cmd)
stopifnot(isTRUE(all.equal(file.size(manifest_probes_path1), 284227029L)))


# Controls:
#sed -i '1d' xx01
#echo 'IlmnID,Name,IlmnStrand,SNP,AddressA_ID,AlleleA_ProbeSeq,AddressB_ID,AlleleB_ProbeSeq,GenomeBuild,Chr,MapInfo,Ploidy,Species,Source,SourceVersion,SourceStrand,SourceSeq,TopGenomicSeq,BeadSetID,Exp_Clusters,RefStrand' | cat - xx01 > temp && mv temp xx01 


###########################
# 

if (FALSE) {
  rsid_dir <- "/tmp/RtmpZEwja4"
  rsid_path <- file.path(rsid_dir, "GSA-24v3-0_A1_b151_rsids.txt")
  
  manifest_dir <- rsid_dir
  manifest_probes_path <- file.path(manifest_dir, "MANIFEST00")
}

library(multidplyr)
library(tidyverse)
library(data.table)
library(here)

manifest_raw <- data.table::fread(manifest_probes_path, sep = ",")
manifest_raw1 <- data.table::fread(manifest_probes_path1, sep = ",") %>% 
  as_tibble() %>% dplyr::select(IlmnID, MapInfo) %>% dplyr::rename(`Pos_hg19` = `MapInfo`)

manifest <- manifest_raw %>% 
  as_tibble() %>% 
  mutate(TopGenomicSeqSBE = gsub("^.*(.{1}\\[.*\\].{1}).*$", "\\1", TopGenomicSeq),
         TopGenomicSeqSBE_Left = gsub("^(.{1})\\[.*$", "\\1", TopGenomicSeqSBE),
         TopGenomicSeqSBE_Right = gsub("^.*\\](.{1})*$", "\\1", TopGenomicSeqSBE)) %>% 
  select(IlmnID, Name, SNP, IlmnStrand, SourceStrand, 
         TopGenomicSeqSBE, TopGenomicSeqSBE_Left, TopGenomicSeqSBE_Right, 
         AddressA_ID, AddressB_ID, 
         GenomeBuild, Chr, MapInfo, BeadSetID, Exp_Clusters, RefStrand) %>% 
  rename(`Pos_hg38` = `MapInfo`) %>%
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
  inner_join(manifest_raw1, by=join_by(IlmnID)) %>%
  mutate(Exclude = case_when(
    Name %in% c("rs28362918", "rs28897688") ~ TRUE,
    TRUE ~ FALSE
  ))

rsno <- data.table::fread(rsid_path, header = TRUE, sep = "\t") %>%
  as_tibble()

stopifnot(nrow(anti_join(manifest, rsno, by = "Name")) == 0L)
stopifnot(nrow(anti_join(rsno, manifest, by = "Name")) == 0L)
stopifnot(nrow(manifest) == 654027L)

manifest <- manifest %>% 
  inner_join(rsno, by = "Name")

stopifnot(nrow(manifest) == 654027L)

###########

mappingcomments <- data.table::fread(mappingcomments_path, header = TRUE, sep = "\t") %>%
  as_tibble()

stopifnot(nrow(anti_join(manifest, mappingcomments, by = "Name")) == 0L)
stopifnot(nrow(anti_join(mappingcomments, manifest, by = "Name")) == 0L)
stopifnot(nrow(manifest) == 654027L)

manifest <- manifest %>% 
  inner_join(mappingcomments, by = "Name")

stopifnot(nrow(manifest) == 654027L)


#manifest %>% filter(!grepl("^[0-9]+$", Name)) %>% 
#  select(IlmnID, Name, RsID)

manifest <- manifest %>% 
  select(Name, IlmnID, RsID, Chr, Pos_hg38, Pos_hg19, AddressA_ID, AddressB_ID, GenomeBuild, 
         SNP, everything()) 
class(manifest) <- "data.frame"

stopifnot(length(unique(manifest$Name)) == nrow(manifest))
library(BSgenome.Hsapiens.UCSC.hg19)
hg19 <- BSgenome.Hsapiens.UCSC.hg19
library(BSgenome.Hsapiens.UCSC.hg38)
hg38 <- BSgenome.Hsapiens.UCSC.hg38

hg38_sequence <- function(chr, pos) { 
  chr <- gsub('XY','X', gsub('MT', 'M', chr))
  chr <- paste0('chr', chr)
  if (chr=='chr0' | seqlengths(hg38)[chr] < pos)  
    return(NA) 
  else 
    return(as(hg38[[chr]][pos], 'character'))
  }

hg19_sequence <- function(chr, pos) {
  chr <- gsub('XY','X', gsub('MT', 'M', chr))
  chr <- paste0('chr', chr)
  if (chr=='chr0' | seqlengths(hg19)[chr] < pos) 
    return(NA) 
  else 
    return(as(hg19[[chr]][pos], 'character'))
  }

cl <- new_cluster(8)
cluster_copy(cl, 'hg38_sequence')
cluster_copy(cl, 'hg38')
cluster_copy(cl, 'hg19_sequence')
cluster_copy(cl, 'hg19')
#cluster_call(cl, ls())
cluster_library(cl, 'BSgenome.Hsapiens.UCSC.hg19')
cluster_library(cl, 'BSgenome.Hsapiens.UCSC.hg38')
cluster_library(cl, 'tidyverse')
#cluster_call(cl, search())
manifest1 <- manifest %>% group_by(Chr) %>% partition(cl)

manifest <- manifest1 %>%   
  mutate(Ref_hg38 = map2_chr(Chr, Pos_hg38, ~hg38_sequence(.x, .y)), Ref_hg19 = map2_chr(Chr, Pos_hg19, ~hg19_sequence(.x, .y))) %>% 
  collect() %>% ungroup()

rm(manifest1)
rm(cl)

#manifest_I <- subset(manifest, ProbeType == "I")

#usethis::use_data(manifest, overwrite = TRUE, version = 3, compress = "bzip2")

#cat(paste0(paste0("#'   \\item{", colnames(manifest), "}{}\n"), collapse = ""))

#subset(manifest, Exclude == TRUE)

dest <- here("inst", "extdata", "manifest.csv")

chunks <- 4
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

