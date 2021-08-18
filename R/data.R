#' Load manifest information for Illumina Infinium Omni5-4 SNP bead chips
#' 
#' This dataset is a combination of the following two datasets 
#' from <https://support.illumina.com/array/array_kits/humanomni5-4-beadchip-kit/downloads.html>:
#' 
#' 1. `InfiniumOmni5-4v1-2_A2.csv`
#'     - `Infinium Omni5-4 v1.2 Product Files -> Infinium Omni5-4 v1.2 Manifest File (CSV Format - GRCh38)`
#' 2. `InfiniumOmni5-4v1-2_A1_b144_rsids.txt`
#'     - `Infinium Omni5-4 v1.2 Support Files -> Infinium Omni5-4 v1.2 Loci Name to rsID Conversion File`
#' 
#' Note that the following probes have `Exclude = FALSE` (all others have `Exclude = TRUE`) 
#' because they are ambiguous (SNP is `C`/`G`), but they are a Type-II probe (i.e. do not have an `AddressB_ID`), 
#' so the bases cannot be distinguish as both `C` and `G` are detected by the green channel:
#' 
#' * Name `rs28362918` (Illumina ID `rs28362918-131_T_R_1908372611`)
#' * Name `rs28897688` (Illumina ID `rs28897688-131_T_R_1908379219`)
#' 
#'
#' @format A data frame with 4,327,108 rows (probes) and 18 variables:
#' \describe{
#'   \item{Name}{Name of probe}
#'   \item{IlmnID}{Illumina ID of probe}
#'   \item{RsID}{rs number of probe}
#'   \item{Chr}{Chromosome}
#'   \item{AddressA_ID}{Address A of probe}
#'   \item{AddressB_ID}{Address B of probe}
#'   \item{SNP}{The possible bases of the SNP}
#'   \item{IlmnStrand}{`TOP`/`BOT`/`PLUS`/`MINUS`}
#'   \item{SourceStrand}{`TOP`/`BOT`/`PLUS`/`MINUS`}
#'   \item{TopGenomicSeqSBE}{The content of the `.[.].` part of `TopGenomicSeq` (not included), including the flanking left and right parts}
#'   \item{TopGenomicSeqSBE_Left}{The left flanking of `TopGenomicSeqSBE`}
#'   \item{TopGenomicSeqSBE_Right}{The right flanking of `TopGenomicSeqSBE`}
#'   \item{BeadSetID}{Bead set ID}
#'   \item{Exp_Clusters}{3 or 2}
#'   \item{RefStrand}{`+`/`-`}
#'   \item{ProbeType}{`II` (only address A) or `I` (address A and B)}
#'   \item{SNPType}{Signals whether `SNP` has alleles on same color channel (ambiguous) or not (unambiguous), or potentially is an INDEL (insertion/deletion): `AMB`iguous for AT-SNPs (`[A/T]` or `[T/A]`) and CG-SNPs; `INDEL` for INDELs; `UNAMB`iguous for the rest (e.g. AC-SNPs)}
#'   \item{Exclude}{Some probes should be excluded (cf. above), this indicate those}
#' }
#' @source \url{https://support.illumina.com/array/array_kits/humanomni5-4-beadchip-kit/downloads.html}
#' 
#' @importFrom data.table fread rbindlist
load_manifest <- function() { 
  dir <- system.file("extdata", package = "omni54manifest")
  fls <- list.files(path = dir, pattern = "^manifest-.*\\.csv$", full.names = TRUE)
  
  result <- vector("list", length(fls))
  
  for (i in seq_along(fls)) {
    result[[i]] <- data.table::fread(input = fls[i], sep = ";")
  }
  
  manifest <- data.table::rbindlist(result, use.names = TRUE)
  data.table::setkey(x = manifest, Name)
  
  return(manifest)
}

