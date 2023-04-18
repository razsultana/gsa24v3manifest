#' Load manifest information for Illumina Infinium Global Screening Array v3.0 SNP beadchip
#' 
#' This dataset is a combination of the following three datasets 
#' from https://sapac.support.illumina.com/array/array_kits/infinium-global-screening-array/downloads.html>>:
#' 
#' 1. `GSA-24v3-0_A2.csv`
#'     - `Infinium Global Screening Array v2.0 Product Files -> Infinium Global Screening Array v2.0 Manifest File (CSV Format - GRCh38)`
#' 2. `GSA-24v3-0_A1_b151_rsids.txt`
#'     - `Infinium Global Screening Array v3.0 Support Files -> Infinium Global Screening Array v3.0 Loci Name to rsID Conversion File`
#' 3. `GSA-24v3-0_A1_MappingComment.txt`
#'     - `Infinium Global Screening Array v3.0 Support Files -> Infinium Global Screening Array v3.0 Mapping Comments`
#' 
#'
#' @format A data frame with 654,027 rows (probes) and 21 variables:
#' \describe{
#'   \item{Name}{Name of probe}
#'   \item{IlmnID}{Illumina ID of probe}
#'   \item{RsID}{rs number of probe}
#'   \item{Chr}{Chromosome}
#'   \item{MapInfo}{Map information}
#'   \item{AddressA_ID}{Address A of probe}
#'   \item{AddressB_ID}{Address B of probe}
#'   \item{GenomeBuild}{Genome build, e.g. 38}
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
#'   \item{MappingComment}{Mapping comment - ambiguous mapping, PAR regions etc.}
#' }
#' @source \url{https://sapac.support.illumina.com/array/array_kits/infinium-global-screening-array/downloads.html>}
#' 
#' @importFrom data.table fread rbindlist
#' @importFrom parallel mclapply
#' @export
load_manifest <- function() { 
  dir <- system.file("extdata", package = "gsa24v3manifest")
  fls <- list.files(path = dir, pattern = "^manifest-.*\\.csv$", full.names = TRUE)
  
  result <- parallel::mclapply(seq_along(fls), function(i) {
    data.table::fread(input = fls[i], sep = ";", showProgress = FALSE)
  })
  
  #result <- vector("list", length(fls))
  #for (i in seq_along(fls)) {
  #  result[[i]] <- data.table::fread(input = fls[i], sep = ";", showProgress = FALSE)
  #}
  
  manifest <- data.table::rbindlist(result, use.names = TRUE)

  return(manifest)
}

