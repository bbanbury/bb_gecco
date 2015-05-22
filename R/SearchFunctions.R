##  ---------------------------------  ##
##                                     ##
##        Data Search Functions        ##
##              BBanbury               ##
##              22 May 15              ##
##                                     ##
##  ---------------------------------  ##



##  ---------------------------------  ##
##                                     ##
##         Assistant Functions         ##
##                                     ##
##  ---------------------------------  ##


#' List of file directories for gecco data 
#' 
#' This function will add the specific path plus root directory  
#' @param whichData Specifies which data you need, and will point to a file/directory
#' @export
#' @return Returns a path to the directory. THis function is where we can store where data lives in directories, 
#' so that when we change where data lives, we only have to change it once.  
#' @seealso \link{MakePathtoNewcomb_P} \link{MakePathtoPeters_U}
#' @examples
#' DataLocation("hapmap_snp_names")
DataLocation <- function(whichData){
  # GWAS
  if(whichData == "hapmap_snp_names")  return(MakePathtoPeters_U("/GECCO_Working/barb_working/HapMap_data/snp_names/"))
  if(whichData == "hapmap")  return(MakePathtoPeters_U("/GECCO_Data/"))
  if(whichData == "hapmap_SNPinfo")  return(MakePathtoPeters_U("GECCO_Working/barb_working/HapMap_data/"))
  if(whichData == "rs_to_gigs_file")  return(MakePathtoPeters_U("/GECCO_Working/barb_working/rs_names_to_gigs_positions.csv"))
  if(whichData == "gigs_sample") return(MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V1/sample_files/gigs_sample.Rdata"))
  if(whichData == "GIGSv2")  return(MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V2/"))
  if(whichData == "GIGS_snp_names")  return(MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V2/snp_files/*.csv"))
  if(whichData == "gigs_pca")  return(MakePathtoPeters_U('GECCO_Working/keithworking/t275-work/pca.Rdata'))
  # Epi / Survival
  if(whichData == "harmonized-epi")  return(MakePathtoPeters_U("/Data Harmonization/Post-harmonization/Data/", server="cs"))
  if(whichData == "Jihyoun-dals")  return(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/DALS-surv-dat-04152014.csv"))
  if(whichData == "Jihyoun-cps2")  return(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/CPS2-surv-dat-04152014.csv"))
  if(whichData == "pooledSurv")  return(MakePathtoPeters_U("/GECCO_Working/mpassareworking/Survival/Combined\ Survival\ Update\ Has\ Surv\ Pooled.csv"))
  if(whichData == "listOfEpiVars")  return(MakePathtoPeters_U("/GECCO_Working/barb_working/listOfVariables.Rdata"))
  if(whichData == "separate-epi")  return(MakePathtoPeters_U("/GECCO_Working/mpassareworking/Survival/Data/"))
  if(whichData == "separate-isacc")  return(MakePathtoNewcomb_P("/Molecular Correlates_ISACC/Survival data harmonization/Harmonized data/", "cs"))
  #software or other
  if(whichData == "software")  return(MakePathtoPeters_U("/PetersGrp/GECCO_Software/"))
  if(whichData == "PriorityPruner")  return(MakePathtoPeters_U("/PetersGrp/GECCO_Software/bin/PriorityPruner.jar"))
  if(whichData == "wgs_tfam")  return(MakePathtoPeters_U("/GECCO_Working/chuckworking/wgs/ld_data/whi-wgs_snps_unfiltered/whi_wgs.tfam"))
  if(whichData == "wgs_tped")  return(MakePathtoPeters_U("/GECCO_Working/chuckworking/wgs/ld_data/whi-wgs_snps_unfiltered/"))
  if(whichData == "1kgp_tfam")  return(MakePathtoPeters_U("/GECCO_Working/chuckworking/1kgp/1kgp_phase1v3_founder.tfam"))
  if(whichData == "1kgp_tped")  return(MakePathtoPeters_U("/GECCO_Working/chuckworking/1kgp/"))
  #results
  if(whichData == "hapmap_marginal")  return(MakePathtoPeters_U("Results_Database/Version 4/Results - Meta of Marginal/gecco_version3_SNPinfo_23studies.Rdata", "cs"))
  if(whichData == "gigsv1_marginal")  return(MakePathtoPeters_U("Results_Database/GIGS_V1/Marginal/*Rdata", "cs"))
  if(whichData == "gigsv2_marginal")  return(MakePathtoPeters_U("Results_Database/GIGS_V2/Marginal/*Rdata", "cs"))
  else stop("Can Not Find Data")
}


#' Make a directory path 
#' 
#' This function will add the root directory path to Peters_U folder.  
#' @param directory The endpoint directory  
#' @param server Which server to use, either "home" or "cs"
#' @export
#' @return Returns a path to the directory. This function will auto detect which host a user is on, and create a path depending on the host. 
#' @seealso \link{MakePathtoNewcomb_P}
#' @examples
#' MakePathtoPeters_U("/GECCO_Working", server="cs")
MakePathtoPeters_U <- function(directory, server="home"){
  if(system("hostname", intern=TRUE) == "DHCP169022.FHCRC.ORG"){
    bp <- "/Volumes/bbanbury/Peters_U/"
    if(server == "cs")
      bp <- sub("bbanbury", "researcher", bp)
  }
  if(length(grep("rhino", system("hostname", intern=TRUE))) > 0){
    bp <- "/shared/silo_researcher/Peters_U/"
    if(server == "cs")
      bp <- sub("silo", "cs", bp)
  }
  directory <- sub("^/", "", directory) # remove if it starts with a "/"
  return(paste0(bp, directory))
}


#' Make a directory path 
#' 
#' This function will add the root directory path to Newcomb_P folder.  
#' @param directory The endpoint directory  
#' @param server Which server to use, either "home" or "cs"
#' @export
#' @return Returns a path to the directory. This function will auto detect which host a user is on, and create a path depending on the host. 
#' @seealso \link{MakePathtoPeters_U}
#' @examples
#' MakePathtoNewcomb_P("/Molecular Correlates_ISACC", server="cs")
MakePathtoNewcomb_P <- function(directory, server="cs"){
  if(system("hostname", intern=TRUE) == "DHCP169022.FHCRC.ORG"){
    bp <- "/Volumes/bbanbury/Newcomb_P/"
    if(server == "cs")
      bp <- sub("bbanbury", "researcher", bp)
  }
  if(length(grep("rhino", system("hostname", intern=TRUE))) > 0){
    bp <- "/shared/silo_researcher/Newcomb_P/"
    if(server == "cs")
      bp <- sub("silo", "cs", bp)
  }
  directory <- sub("^/", "", directory) # remove if it starts with a "/"
  return(paste0(bp, directory))
}


#' Change Study Names
#' 
#' This function will change the study names from those in the ISACC paper proposal to the numerical name. 
#' @param study One of: ASTERISK, OFCCR, PMH-CCFR
#' @export
#' @return Returns a new study name
#' @examples
#' ChangeStudyNames("ASTERISK")
#' sapply(c("ASTERISK", "OFCCR", "PMH-CCFR"), ChangeStudyNames)
ChangeStudyNames <- function(study){
  if(study == "ASTERISK") return("111fccs")
  else if(study == "OFCCR") return("102arctic")
  else if(study == "PMH-CCFR") return("117hrtccr")
  else return(study)
}


#' print.snp_names
#'
#' Compact Display of SNP Names 
#'
#' @param x an object in the class "snp_names"
#' @param ... Additional arguments passed to print
#' @export
#' @return This function prints a short summary of the SNP names to console, so it doesn't freeze up the machine if the list is huge. 
#' @seealso \link{find_rs_hapmap} \link{FindSNPposition_hapmap}
#' @examples
#' load(paste0(DataLocation("hapmap_snp_names"), "snp_names_113nhs_omni1.Rdata"))
#' print(snp_names)
print.snp_names <- function(x, ...){
  writeLines("SNP names data contains:", ...)
  for(i in sequence(length(x))){
    writeLines(paste("     ", names(x)[i], length(x[[i]])))
  }
}


#' print.snps_in_gene_regions
#'
#' Compact Display of SNPs in gene regions
#'
#' This function prints a short summary of the SNP names to console, so it doesn't freeze up the machine if the list is huge. 
#'
#' @param x an object in the class "snps_in_gene_regions"
#' @param ... Additional arguments passed to print
#' @export
#' @seealso \link{Use_snp_finder.py} \link{FindSNPpositions_gigs}
#' @examples
#' Use_snp_finder.py("MYC")
print.snps_in_gene_regions <- function(x, ...){
  writeLines(paste("This is a matrix of snps in", length(unique(x[,2])), "gene regions."), ...)
  for(g in unique(x[,2])){
    writeLines(paste("     -- ", g, "has", length(which(x[,2] == g)), "snps"), ...)
  }
}


#' print.snp_location_info
#'
#' Compact Display of SNPs locations
#'
#' This function prints a short summary of the SNP locations to console, so it doesn't freeze up the machine if the list is huge. 
#'
#' @param x an object in the class "snp_location_info"
#' @param ... Additional arguments passed to print
#' @export
#' @seealso \link{FindSNPposition_hapmap} \link{FindSNPpositions_gigs} \link{MakeSNPDetailsTable_GIGS}
#' @examples
#' FindSNPposition_hapmap("rs2965667")
print.snp_location_info <- function(x, ...){
  writeLines(paste("This is a matrix of", dim(x)[1], "snps", "and all their locality information"), ...)
  writeLines(paste("Includes data from:"))
  writeLines(paste0("     -- ", length(unique(x[,1])), " studies {", paste(unique(x[,1]), collapse=", "), "}"))
  writeLines(paste0("     -- ", length(unique(x[,2])), " batches {", paste(unique(x[,2]), collapse=", "), "}"))
  chromos <- sapply(unique(x[,4]), GetChromo, USE.NAMES=FALSE)
  writeLines(paste0("     -- ", length(unique(chromos)), " chromosomes {", paste(unique(chromos), collapse=", "), "}"))
}


#' Use snp_finder.py program
#'
#' Use Chuck's snp_finder.py program
#'
#' This function is just a wrapper for Chucks snp_finder py script.  Pass it the same variables and it returns a matrix of matches.
#'
#' @param gene Gene in which to find snps
#' @param upstream Distance upstream in bp
#' @param downstream Distance downstream in bp
#' @param snp_list Name of snp list to use. Options include: "gigs", "exome_pooled_20130624", "hapmap_imputed_20120208", or "combined_exome_1kgp"
#' @param buildver Which human genome build to use, either "hg18" or "hg19" (use hg18 for hapmap imputed)
#' @param include_ncrna Boolean, whether to include non-coding RNAs in the search (such as miRNA). Defaults to FALSE, however, if you do not find anything with that then try to run using TRUE to expand the search. 
#' @param report.call Option to report the system call so you can run Chucks program via terminal.
#' @param chatty Option to print progress to screen
#' @export
#' @return This function uses system calls, so it will not work on a Windows machine. If the program works and finds snps, then it will return a matrix in the class "snps_in_gene_regions ", that has two columns, one with the snp location and the other with the gene. 
#' @seealso \link{print.snps_in_gene_regions} 
#' @examples
#' Use_snp_finder.py("MYC", 500, 500)
Use_snp_finder.py <- function(gene, upstream=0, downstream=0, snp_list="gigs", buildver="hg19", include_ncrna=FALSE, report.call=FALSE, chatty=TRUE){
  allsnps <- NULL
  for(g in gene){
    if(chatty)
      print(paste("working on", g))
    path <- DataLocation("software")
    com <- paste0(path, "bin/snp_finder.py --db ", path, "snp.db --gene ", g, " -u ", upstream, " -d ", downstream, " -b ", buildver, " --snp_list ", snp_list)
    if(include_ncrna)
      com <- paste0(com, " --include_ncrna")
    if(report.call)
      print(com)
    snps <- system(com, intern=TRUE)
    snps <- snps[grep("^chr", snps)]
    if(length(snps) > 0){
      snps <- sapply(snps, strsplit, USE.NAMES=FALSE, split="\\t")
      snps <- matrix(unlist(sapply(snps, strsplit, USE.NAMES=FALSE, split="\\t")), ncol=2, byrow=TRUE)
      allsnps <- rbind(allsnps, snps)
    }
    else
      warning(paste(g, "not found in database, consider running with include_ncrna==TRUE"))
  }
  allsnps[,1] <- sapply(allsnps[,1], sub, pattern="chr", replacement="", USE.NAMES=FALSE)
  colnames(allsnps) <- c("position", "gene")
  class(allsnps) <- "snps_in_gene_regions"
  return(allsnps)
}


#' Get Chromosome
#'
#' Get Chromosome Number
#'
#' @return This function returns a chromosome from a filename 
#'
#' @param ncdfFileName An ncdf file name with chromosome number at the end
#' @export
#' @seealso \link{GetStudy} \link{GetBatch} \link{GetLastFileNameInPath}
#' @examples
#' GetChromo("101ccfr_usc2_merged_dosage_5.nc")
GetChromo <- function(ncdfFileName){
  ncdfFileName  <- GetLastFileNameInPath(ncdfFileName)
  dd <- strsplit(ncdfFileName, "[_.]")[[1]][length(strsplit(ncdfFileName, "[_.]")[[1]]) - 1]
  return(gsub("\\D+", "", dd))
}


#' Get Study
#'
#' Get Study out of ncdf file name
#'
#' @return This function returns a study from a filename 
#'
#' @param ncdfFileName An ncdf file name with study name number at the beginning
#' @export
#' @seealso \link{GetChromo} \link{GetBatch} \link{GetLastFileNameInPath}
#' @examples
#' GetStudy("101ccfr_usc2_merged_dosage_5.nc")
GetStudy <- function(ncdfFileName){
  ncdfFileName  <- GetLastFileNameInPath(ncdfFileName)
  return(strsplit(ncdfFileName, "[_.]")[[1]][1])
}


#' Get Batch
#'
#' Get Batch out of ncdf file name
#'
#' This function returns a batch from a filename 
#'
#' @param ncdfFileName An ncdf file name with batch name number 
#' @export
#' @seealso \link{GetChromo} \link{GetStudy} \link{GetLastFileNameInPath}
#' @examples
#' GetBatch("101ccfr_usc2_merged_dosage_5.nc")
GetBatch <- function(ncdfFileName){
  ncdfFileName  <- GetLastFileNameInPath(ncdfFileName)
  return(strsplit(ncdfFileName, "[_.]")[[1]][2])
}


#' Get File Name without Path
#'
#' Get a filename without the whole path
#'
#' This function strips all path information from a file, and returns just the name of the file.
#'
#' @param fileWithPath An ncdf file name with or without a whole path
#' @export
#' @seealso \link{GetChromo} \link{GetStudy} \link{GetLastFileNameInPath}
#' @examples
#' GetLastFileNameInPath("/directory/to/remove/101ccfr_usc2_merged_dosage_5.nc")
GetLastFileNameInPath <- function(fileWithPath){
  return(strsplit(fileWithPath, "/", fixed=TRUE)[[1]][length(strsplit(fileWithPath, "/", fixed=TRUE)[[1]])])
}


#' Change Alleles from Numeric to Letters
#'
#' Change Allele Coding from 1,2,3,4 to A,C,T,G
#'
#' This function changes numeric allele designation to letters
#'
#' @param num A number 1:4
#' @export
#' @seealso \link{MakePriorityPrunerInputFile} 
#' @examples
#' ChangeAlleles("1")
ChangeAlleles <- function(num){
  num <- as.numeric(num)
  if(num == 1)  return("A")
  if(num == 2)  return("C")
  if(num == 3)  return("G")
  if(num == 4)  return("T")
  else return("NA")
}


#' deFactorize
#'
#' Auto Remove the factoring
#'
#' This function changes any factor class to character
#'
#' @param table Any table (class data.frame or matrix)
#' @export
deFactorize <- function(table){
  for(i in sequence(dim(table)[2])){
    if(is.factor(table[,i]))
      table[,i] <- as.character(table[,i])
  }
  return(table)
}


#' MakeSplitPos
#'
#' Internal function for splitting a snp name in gigs to its position information
#'
#' Changes snp names from 1:123 to a data frame with name, chromosome, and position data
#'
#' @param gigs_numbers SNP names (ex, 1:132423)
#' @export
#' @seealso \link{GetMarginal} 
#' @examples
#' ChangeAlleles("1")
MakeSplitPos <- function(gigs_numbers){
# vector or single gigs_number (ex, 1:234)
  if(any(is.na(gigs_numbers)))
    gigs_numbers <- gigs_numbers[-which(is.na(gigs_numbers))]
  gigs_numbers <- as.character(gigs_numbers)
  splitpos <- matrix(unlist(sapply(gigs_numbers, strsplit, split=":")), ncol=2, byrow=TRUE)
  splitpos <- cbind(gigs_numbers, splitpos)
  colnames(splitpos) <- c("SNP_Name", "chr", "pos")
  return(deFactorize(splitpos))
}


#' GetMarginal Results
#'
#' Get Marginal Analysis results
#'
#' Returns a table of results, with snp as rows
#'
#' @param snps A gigs SNP name, chr:pos (ex, "1:1234")
#' @param datasource Either "gigsv1" or "gigsv2" for now, can add hapmap later
#' @param chatty Option to print progress to screen
#' @export
#' @seealso \link{MakePriorityPrunerInputFile} 
#' @examples
#' GetMarginal(c("2:204570092", "3:30680370", "1:169602109"))
GetMarginal <- function(snps, datasource="gigsv2", chatty=TRUE){
  snps <- MakeSplitPos(snps)
  res <- NULL
  if(datasource == "gigsv1")
    files <- DataLocation("gigsv1_marginal")
  if(datasource == "gigsv2")
    files <- DataLocation("gigsv2_marginal")
  files <- system(paste("ls ", files), intern=TRUE)
  filesnames <- sapply(files, GetChromo, USE.NAMES=FALSE)
  files <- files[which(filesnames %in% unique(snps[,2]))]
  for(i in files){
    if(chatty)
      print(paste("Working on", GetLastFileNameInPath(i)))
    res.chr <- NULL
    load(i)
    sub_snps <- snps[which(snps[,2] == GetChromo(i)),1]
    marg <- t(res.chr[,which(colnames(res.chr) %in% sub_snps)])[,1:4]
    if(class(marg) == "numeric" | class(marg) == "character"){
      marg <- matrix(marg, nrow=1)
      rownames(marg) <- sub_snps[[1]]
    }
    res <- rbind(res, cbind(rownames(marg), marg))
  }
  rownames(res) <- NULL
  colnames(res) <- c("SNP_Name", "Estimate", "Std. Error", "p", "var")
  return(res)
}


#' Make PriorityPruner Input File
#'
#' Auto create an input file for the program PriorityPruner from a vector of snp names
#'
#' This function returns a table with the required columns for the program PriorityPruner. These include: snp_name, chr, pos, a1, a2, P, forceSelect, and designScore.
#'
#' @param snplist A SNP name, either rs number or chr:pos (ex, "rs1234" or "1:1234")
#' @param pvals Where to draw pvalues (or other values between 0 and 1). PriorityPruner will use these as a prioritization metric. This is used for prioritizing the selection of SNPs, where lower numbers are prioritized.
#' @param forceSelect For now, this doesn't really work other than to assign all values the same 0 status. Flag indicating if the SNP should be selected (kept) regardless of its LD with other selected SNPs or other filtering criteria specified, such as MAF or design score (1=true, 0=false).
#' @param designScore For now, this doesn't really work other than to assign all values the same 1 status. Flag indicating if the SNP should be selected (kept) regardless of its LD with other selected SNPs or other filtering criteria specified, such as MAF or design score (1=true, 0=false).
#' @param chatty Option to print progress to screen
#' @param save.file Option to save results to files. This will save a separate file for each chromosome in the snplist. 
#' @export
#' @seealso \link{MakeSNPDetailsTable_GIGS} \link{GetMarginal} \link{Run_PriorityPruner}
#' @examples
#' MakePriorityPrunerInputFile(c("rs3181096", "rs3863057", "rs6666554"), save.file=FALSE)
MakePriorityPrunerInputFile <- function(snplist, pvals="gigsv2", forceSelect=NULL, designScore=NULL, chatty=TRUE, save.file=TRUE){
  if(pvals != "gigsv1" & pvals != "gigsv2") #could use any pvals really, not just out of marginal
    stop("need to add more than gigs pvals")
  if(pvals == "gigsv1" | pvals == "gigsv2"){
    gigsposition <- snplist
    if(any(grep("rs", snplist)))
      gigsposition[grep("rs", snplist)] <- find_rs_to_gigs(snplist[grep("rs", snplist)], chatty)[,2]
    if(any(grep("chr", snplist)))
      gigsposition[grep("chr", snplist)] <- gsub("chr", "", gigsposition[grep("chr", snplist)])
    if(class(gigsposition) == "character")
      gigsposition <- cbind(snplist, gigsposition)
    if(any(is.na(gigsposition)))
        gigsposition <- gigsposition[-which(is.na(gigsposition), arr.ind=TRUE)[,1],]
    if(any(grep(":I|:D", gigsposition[,2])))
      gigsposition[grep(":I|:D", gigsposition[,2]),2] <- gsub(":I|:D", "", gigsposition[grep(":I|:D", gigsposition[,2]),2])
    splitpos <- MakeSplitPos(gigsposition[,2])
    res <- cbind(gigsposition[,1], splitpos)
    res2 <- MakeSNPDetailsTable_GIGS(res[,2], chatty)
    res3 <- deFactorize(merge(res[,c(1,2,3,4)], res2[,c(1,2,3)], by="SNP_Name")) 
    colnames(res3) <- c("name", "rs_name", "chr", "pos", "a1", "a2")
    res3$a1 <- sapply(res3$a1, ChangeAlleles)
    res3$a2 <- sapply(res3$a2, ChangeAlleles)
    margs <- GetMarginal(res3$name, pvals, chatty=chatty)[,c(1,4)]
    res4 <- deFactorize(merge(res3, margs, by.x="name", by.y="SNP_Name"))
  }
  if(is.null(forceSelect))
    forceSelect <- rep(0, dim(res4)[1]) # these will need else statements if someone passes a mismatched vector. 
  if(is.null(designScore))
    designScore <- rep(1, dim(res4)[1])
  d <- data.frame(res4[,-2], forceSelect=forceSelect, designScore=designScore, stringsAsFactors=FALSE)
  if(save.file){
    for(i in unique(d[,2])){
      sub_d <- d[which(d[,2] == i), ]
      write.table(sub_d, file=paste0("snp_table_chr", i, ".txt"), row.names=FALSE, sep=" ", quote=FALSE)
    }
  }
  return(d)
}
# snplist <- read.csv(MakePathtoPeters_U('GECCO_Working/Floraworking/Shared_results/3003_snplist.csv'), stringsAsFactor=F)[,1]


#' Run PriorityPruner On Gizmo
#'
#' Run PriorityPruner on Gizmo Cluster
#'
#' This function will batch PriorityPruner jobs to Gizmo cluster from a rhino node
#'
#' @param snp_table_file A snp_table file that was created either by hand or using the MakePriorityPrunerInputFile function
#' @param datasource This is where we draw the tfam/tped files for PriorityPruner. At this point, it can either be gigsv2 or 1kgp data
#' @param r2 numeric, Fixed LD R2 cut off from 0-1
#' @param report.call Option to report the system call so you can run PriorityPruner program via terminal
#' @export
#' @seealso \link{MakePriorityPrunerInputFile} \link{Read_PriorityPruner_Results}
#' @examples
#' \dontrun{Run_PriorityPruner("snp_table_chr1")}
Run_PriorityPruner <- function(snp_table_file, datasource="gigsv2", r2=0.5, report.call=FALSE){
  if(length(grep("rhino", system("hostname", intern=TRUE))) < 1)
    stop("Must be on a rhino machine")
  call <- paste0("java -Xms256m -Xmx2048m -jar ", DataLocation("PriorityPruner"))
  if(datasource == "gigsv2"){
    all_files <- system(paste("ls", DataLocation("wgs_tped")), intern=TRUE)
    tped_file <- all_files[grep(paste0("chr", gsub("\\D+", "", snp_table_file), ".tped"), all_files)]
    call2 <- paste0(call, " --tfam ", DataLocation("wgs_tfam"), " --tped ", DataLocation("wgs_tped"), tped_file)
  }
  if(datasource == "1kgp"){
    all_files <- system(paste("ls", DataLocation("1kgp_tped")), intern=TRUE)
    tped_file <- all_files[grep(paste0("chr", gsub("\\D+", "", snp_table_file), ".tped"), all_files)]
    call2 <- paste0(call, " --tfam ", DataLocation("1kgp_tfam"), " --tped ", DataLocation("1kgp_tped"), tped_file)
  }
  if(datasource != "gigsv2" & datasource != "1kgp")
    stop("gotta be gigsv2 for now")
  call3 <- paste0(call2, " --snp_table ", snp_table_file, " --r2 ", r2, " --out ld_prune_", gsub("\\D+", "", snp_table_file))
  if(report.call)
    cat(call3)
  system(paste0("sbatch --wrap='", call3, "'"))
}


#' Retrieve PriorityPruner Results
#'
#' Read in results from PriorityPruner 
#'
#' This function will read in and combine all the results from a PriorityPruner analysis
#'
#' @param results_files A vector of *.results files that are output from PriorityPruner
#' @export
#' @seealso \link{MakePriorityPrunerInputFile} \link{Run_PriorityPruner}
#' @examples
#' \dontrun{Read_PriorityPruner_Results(list.files(pattern="\\.results"))}
Read_PriorityPruner_Results <- function(results_files){
  res <- NULL
  for(i in results_files){
    res <- rbind(res,  read.table(i, stringsAsFactors=FALSE, skip=1))
  }
  res <- deFactorize(res)
  colnames(res) <- read.table(i, stringsAsFactors=FALSE)[1,]
  return(res)
}



##  ---------------------------------  ##
##                                     ##
##           Hapmap Functions          ##
##                                     ##
##  ---------------------------------  ##



#' Find RS Numbers from HapMap
#'
#' Find RS numbers from HapMap study-specific dataset
#'
#' Returns table with study, batch, rs_number, ncdf file, and the position
#'
#' @param rs_number Vector or single SNP name
#' @param snp_names Which hapmap study file you want to use, these are found in the directory DataLocation("hapmap_snp_names"). 
#' @export
#' @seealso \link{FindSNPposition_hapmap} 
#' @examples
#' load(paste0(DataLocation("hapmap_snp_names"), "snp_names_101ccfr_usc.Rdata"))
#' find_rs_hapmap(c("rs2736100", "rs401681", "rs10069690"), snp_names)
find_rs_hapmap <- function(rs_number, snp_names){
  if(class(snp_names) != "snp_names")
    stop("looking for an snp names file")
  if(any(rs_number %in% unlist(snp_names))){
    locs <- NULL
    for(i in sequence(length(snp_names))){
      if(any(rs_number %in% snp_names[[i]])){
        for(tat in sequence(length(which(rs_number %in% snp_names[[i]])))){
          which_rs <- rs_number[which(rs_number %in% snp_names[[i]])]
          loc <- c(GetStudy(names(snp_names)[i]), GetBatch(names(snp_names)[i]), which_rs[tat], names(snp_names)[i], which(snp_names[[i]] %in% which_rs[tat]))
          locs <- rbind(locs, loc)
        }
      }
    }
  }
  locs <- as.matrix(locs)
  colnames(locs) <- c("study", "batch", "rs_num", "ncdf file", "position")
  return(locs)
}


#' Find RS Numbers from HapMap
#'
#' Find RS numbers from HapMap accross various studies
#'
#' Returns table in the class "snp_location_info", with study, batch, rs_number, ncdf file, and the position. The special class makes it so that it does not print all results to the screen unless you explicitly want to.  Use [] to display the whole table. 
#'
#' @param rs_number Vector or single SNP name
#' @param studies Which hapmap study files you want to include, these are found in the directory DataLocation("hapmap_snp_names"). If NULL, it will include all. 
#' @param chatty Option to print progress to screen
#' @export
#' @seealso \link{find_rs_hapmap} 
#' @examples
#' FindSNPposition_hapmap(c("rs2736100", "rs401681", "rs10069690"), studies="101ccfr")
#' FindSNPposition_hapmap(c("rs2736100", "rs401681", "rs10069690"))
FindSNPposition_hapmap <- function(rs_number, studies=NULL, chatty=TRUE){
  directory <- DataLocation("hapmap_snp_names")
  res <- NULL
  if(is.null(studies))
    studies <- c("101ccfr", "109colo23", "112mec", "114phs", "115vital", "108dachs", 
                "103dals", "111fccs", "110hpfs", "113nhs", "102arctic", "117hrtccr", 
                "104plco", "105whi")
  for(i in studies){
    if(chatty)
      print(paste("Working on", i))
    snps_names_file <- system(paste0("ls ", directory, "*", i, "*"), intern=TRUE)
    for(j in snps_names_file){
      nafile <- GetLastFileNameInPath(j)
      snp_names <- NULL
      load(j)
      if("snp_names" %in% ls()){
        rs_positions <- find_rs_hapmap(rs_number, snp_names)
        res <- rbind(res, rs_positions)
      }
    }
  }
  class(res) <- "snp_location_info"
  return(res)
}


#' Create a Dosage Dataset One Study
#'
#' Create dataset with Dosage Information for one hapmap study
#'
#' Somewhat of an internal function that returns a dataset with individuals as rows and rs_positions as columns for just one hapmap study (ex,"101ccfr" will return both "usc" and "usc2" batches). Internal, because it is hard to give the directory. If you don't want to have to figure that out, then use the function \link{CreateDosageDataAcrossStudies}
#'
#' @param rs_positions An object of the class "snp_location_info" that comes out of the FindSNPpositions* functions
#' @param directory File path addition to find the nc files
#' @export
#' @seealso \link{FindSNPposition_hapmap} \link{CreateDosageDataAcrossStudies}
#' @examples
#' tmp <- FindSNPposition_hapmap(c("rs2736100", "rs401681", "rs10069690"), studies="102arctic")
#' dir <- "102arctic/102arctic_minusccfr/mach/"
#' CreateDosageDataPerStudy(tmp, directory=paste0(DataLocation("hapmap"), dir))
CreateDosageDataPerStudy <- function(rs_positions, directory=NULL){
  files <- unique(rs_positions[,4])
  probs <- NULL
  probsnamevector <- NULL
  for(whichFile in files){
  print(whichFile)
    nc <- nc_open(paste0(directory, whichFile))
    samples <- ncvar_get(nc, "Sample_ID", start=c(1, 1), count=c(-1,-1))
    study <- rep(GetStudy(whichFile), length(samples))
    batch <- rep(GetBatch(whichFile), length(samples))
    sub_rs_positions <- rs_positions[which(rs_positions[,4] == whichFile),]  
    if(class(sub_rs_positions) == "character"){
      sub_rs_positions <- matrix(sub_rs_positions, nrow=1)
    }
    ind <- 0
    for(i in sub_rs_positions[,5]){
      ind <- ind + 1
      lo <- as.numeric(sub_rs_positions[,5][ind])
      if(ncvar_get(nc, "SNP_Name", start=c(1, lo), count=c(-1,1)) == sub_rs_positions[ind,3]){  #here to check that rs number matches correctly
        dosage <- ncvar_get(nc, "Dosage", start=c(lo, 1), count=c(1,-1))
        #  probAA <- ncvar_get(nc, "Prob_AA", start=c(lo, 1), count=c(1,-1))
        #  probAB <- ncvar_get(nc, "Prob_AB", start=c(lo, 1), count=c(1,-1))
        probs <- cbind(probs, dosage)
        probsnamevector <- c(probsnamevector, paste0(sub_rs_positions[ind,3]))
      }
    res <- cbind(study, batch, samples, probs)
    colnames(res) <- c("study", "batch", "netcdf_ID", probsnamevector)
    }
  }
  return(res)
}


#' Create a Dosage Dataset from HapMap
#'
#' Create dataset with Dosage Information for one or many hapmap studies
#'
#' This function uses the information in rs_positions (class snp_location_info) to gather a 
#' dosage dataset.  There can be one study or many studies. Returns a dataset with individuals 
#' as rows and rs_positions as columns. 
#'
#' @param rs_positions An object of the class "snp_location_info" that comes out of the FindSNPpositions* functions
#' @param saveToRdata Optional file name to save Rdata to. Will not save if left NULL. 
#' @export
#' @seealso \link{FindSNPposition_hapmap} \link{CreateDosageDataPerStudy}
#' @examples
#' tmp <- FindSNPposition_hapmap(c("rs2736100", "rs401681", "rs10069690"), studies="101ccfr")
#' CreateDosageDataAcrossStudies(tmp)
CreateDosageDataAcrossStudies <- function(rs_positions, saveToRdata=NULL){
  nostudies <- unique(paste(rs_positions[,1], rs_positions[,2], sep="_"))
  ddall <- data.frame(matrix(nrow=0, ncol=length(unique(rs_positions[,3]))+3))
  colnames(ddall) <- c("study", "batch", "netcdf_ID", unique(rs_positions[,3]))
  for(i in nostudies){
    dir <- paste0(DataLocation("hapmap"), GetStudy(i), "/", paste(GetStudy(i), GetBatch(i), sep="_"), "/mach/")
    red_rs_positions <- rs_positions[grep(paste0(i, "_"), rs_positions[,4]),]
    dd <- CreateDosageDataPerStudy(red_rs_positions, dir)
    if(any(!colnames(ddall) %in% colnames(dd))){
      colsToAdd <- which(!colnames(ddall) %in% colnames(dd))  #add NAs columns
      NAcols <- data.frame(matrix(nrow=dim(dd), ncol=length(colsToAdd)))
      colnames(NAcols) <- colnames(ddall[colsToAdd])
      dd <- cbind(dd, NAcols)
    }
    ddall <- rbind(ddall, dd)   ## NEED to join columns, because they are different
  }
  if(!is.null(saveToRdata)){
    save(ddall, file=saveToRdata)
  }
  return(ddall)
}


#' GetCountAndBaselineAlleles
#'
#' Get Count and Baseline Allele Information
#'
#' This function pulls the count and baseline allele information per locus from the legend file. Returns a 
#' matrix with rs number, position, allele 1 (count), and allele 2 (baseline)
#'
#' @param rs_numbers A vector of rs numbers
#' @export
#' @seealso \link{CreateSNPDetailsTable} \link{CreateDosageDataPerStudy}
#' @examples
#' GetCountAndBaselineAlleles(c("rs2736100", "rs401681", "rs10069690"))
GetCountAndBaselineAlleles <- function(rs_numbers){
  res <- matrix(nrow=length(rs_numbers), ncol=4)
  colnames(res) <- c("rs_number", "position", "Allele1", "Allele2")
  res[,1] <- rs_numbers
  p <- paste0(DataLocation("hapmap_SNPinfo"), "legend-augmented.nc")
  nc <- nc_open(p)
  nameVector <- ncvar_get(nc, "SNP_Name", start=c(1,1), count=c(-1,-1))
  locations <- which(nameVector %in% rs_numbers)
  res[,2] <- locations
  for(i in sequence(dim(res)[1])){
    res[i,3] <- ncvar_get(nc, "Allele1", start=c(locations[i]), count=c(1))
    res[i,4] <- ncvar_get(nc, "Allele2", start=c(locations[i]), count=c(1))
  }  
  return(res)
}


#' CreateSNPDetailsTable
#'
#' Create a SNP Table with details on hapmap position and alleles
#'
#' For a vector of snps, this function will pull all of the information we want to release with hapmap data.  
#' This will create a table to use in the data release. Results will include: snp name (rs number), chromosome,
#' position, the number of studies genotypes, the number of studies imputed, the count allele, the baseline allele,
#' the mean R2, and the R2 range (min-max), the mean CAF, and the CAF range (min-max). 
#'
#' @param rs_numbers A vector of rs numbers
#' @param studies A vector of studies to be included in the count
#' @param chatty Option to print progress to screen
#' @export
#' @seealso \link{GetCountAndBaselineAlleles} 
#' @examples
#' studies <- c("101ccfr", "102arctic", "103dals")
#' CreateSNPDetailsTable(c("rs2736100", "rs401681", "rs10069690"), studies)
CreateSNPDetailsTable <- function(rs_numbers, studies, chatty=TRUE){
  snpsall <- Imputed.matrix <- R2.matrix <- CAF.matrix <- NULL
  dataNeeded <- c("snpsall", "Imputed.matrix", "R2.matrix", "CAF.matrix")
  for(i in sequence(length(dataNeeded))){
      if(chatty)
        print(paste("loading", dataNeeded[i]))
      load(paste0(DataLocation("hapmap_SNPinfo"), dataNeeded[i], ".Rdata"))
    }
  nc <- nc_open(paste0(DataLocation("hapmap_SNPinfo"), "legend-augmented.nc"))
  dets <- matrix(nrow=length(rs_numbers), ncol=9)
  rownames(dets) <- rs_numbers
  colnames(dets) <- c("Chr", "NumStudGeno", "NumStudImpute", "Count", "Baseline", "MeanR2", "RangeR2", "MeanCAF", "RangeCAF")
  FlorasPos <- which(snpsall[,1] %in% rs_numbers)
  colsToInclude <- NULL
  for(i in colnames(Imputed.matrix)){  #which studies to include
    if(strsplit(i, ".", fixed=TRUE)[[1]][1] %in% studies)
      colsToInclude[i] <- TRUE
    else
      colsToInclude[i] <- FALSE
  }
  FlorasData <- cbind(snpsall[FlorasPos,], Imputed.matrix[FlorasPos, colsToInclude], R2.matrix[FlorasPos, colsToInclude], CAF.matrix[FlorasPos, colsToInclude])
  alleles <- GetCountAndBaselineAlleles(rs_numbers)
  for(i in sequence(dim(dets)[1])){
    h <- which(FlorasData[,1] == rownames(dets)[i])
    dets[i,1] <- FlorasData[h,3]  # chromo
    dets[i,2] <- length(which(FlorasData[h, grep("Imputed", colnames(FlorasData))] == 0))  # genotyped
    dets[i,3] <- length(which(FlorasData[h, grep("Imputed", colnames(FlorasData))] == 1))  # imputed
    dets[i,4] <- alleles[which(alleles[,1] == rownames(dets)[i]), 3]
    dets[i,5] <- alleles[which(alleles[,1] == rownames(dets)[i]), 4]
    whichImputed <- which(FlorasData[h, grep("Imputed", colnames(FlorasData))] == 1)
    if(length(whichImputed) > 0){
      cols <- grep("R2", colnames(FlorasData))[whichImputed]
      dets[i,6] <- round(mean(unlist(FlorasData[h, cols]), na.rm=TRUE), digits=3)  #mean R2 for imputed only
      dets[i,7] <- paste(round(min(FlorasData[h, cols]), digits=3), round(max(FlorasData[h, cols]), digits=3), sep="-")
    }
    cols <- grep("RAF", colnames(FlorasData))
    dets[i,8] <- round(mean(unlist(FlorasData[h, cols]), na.rm=TRUE), digits=3)
    dets[i,9] <- paste(round(min(FlorasData[h, cols], na.rm=TRUE), digits=3), round(max(FlorasData[h, cols], na.rm=TRUE), digits=3), sep="-")
  }
  return(data.frame(dets))
}





##  ---------------------------------  ##
##                                     ##
##            GIGS Functions           ##
##                                     ##
##  ---------------------------------  ##


#' Find RS numbers in GIGSv1 Data
#'
#' Find RS numbers in the GIGSv1 dataset (not available for GIGSv2 yet)
#'
#' This function allows you to search an RS number for its corresponding GIGs name. Will return a table with two columns, rs number, and gigs number.  
#'
#' @param rs_number A vector of rs numbers
#' @param chatty Option to print progress to screen
#' @export
#' @seealso \link{MakePriorityPrunerInputFile} 
#' @examples
#' find_rs_to_gigs(c("rs2736100", "rs401681", "rs10069690"))
find_rs_to_gigs <- function(rs_number, chatty=TRUE){
  rs_to_gigs_file <- DataLocation("rs_to_gigs_file")
  res <- matrix(nrow=length(rs_number), ncol=2)
  colnames(res) <- c("rs_number", "gigs_number")
  for(i in sequence(length(rs_number))){
    if(chatty)
      print(paste("Working on", rs_number[i]))
    tmp <- system(paste0("grep '", rs_number[i], ",' ", rs_to_gigs_file), intern=TRUE)
    if(length(tmp) == 0)
      res[i,] <- rep(NA, 2)
    if(length(tmp) == 1)
      res[i,] <- strsplit(tmp, split=",")[[1]]
    if(length(tmp) > 1)
      stop("something went wonky, grep returning multiple")  
  }
  return(res)
}


#' Find position in gigsv2
#'
#' Find a snp position in the gigsv2 dataset
#'
#' This function will use a gigs snp name (ie, 2:123) and find where that is located in the gigs dataset.
#' This function only searches a single chromosome, so can also use \link{FindSNPpositions_gigs} if you 
#' need to do more than one. Returns a matrix with study (always gigs), gene(always NA), the snp name, 
#' the ncdf file it is located, and the position within the file. 
#'
#' @param snp_name A vector of snp names in gigs format (ie, 1:1234)
#' @param chromosome which chromosome to search. Could be written smarter so that it auto finds this from the position name.  Duh. 
#' @export
#' @seealso \link{FindSNPpositions_gigs} 
#' @examples
#' Find_position_gigs_single_chromo(c("5:1286516", "5:1322087", "5:1279790"), 5)
Find_position_gigs_single_chromo <- function(snp_name, chromosome){
  locs <- matrix(nrow=length(snp_name), ncol=5)
  colnames(locs) <- c("study", "gene", "gigs_name", "ncdf file", "position")
  file <- paste0("gigs_v2_chr", chromosome, ".nc")
  nc <- nc_open(paste0(DataLocation("GIGSv2"), file))
  snpnames <- ncvar_get(nc, "SNP_Name", start=c(1, 1), count=c(-1,-1))
  tocollect <- NULL
  for(i in snp_name){
    if(length(which(snpnames %in% i)) == 0)
      tocollect <- c(tocollect, NA)
    tocollect <- c(tocollect, which(snpnames %in% i))
  }
  locs[,1] <- rep("gigs", length(snp_name))
  locs[,2] <- rep(NA, length(snp_name))
  locs[,3] <- snp_name
  locs[,4] <- rep(file, length(snp_name))
  locs[,5] <- tocollect
  return(locs)
}


#' Find position in gigsv2
#'
#' Find a snp position in the gigsv2 dataset
#'
#' This function will use a gigs snp name (ie, 2:123) and find where that is located in the gigs dataset.
#' This function only searches over a single or many chromosomes. Returns a matrix with study (always gigs
#' for gigs data), gene (if available), the snp name, the ncdf file it is located, and the position 
#' within the file. If the snp_name you pass has gene information, then this will be preserved in the 
#' output.  Otherwise it will be NA.  
#'
#' @param snp_name A vector of snp names in gigs format (ie, 1:1234) or a data frame in the class "snps_in_gene_regions"
#' @param chatty Option to print progress to screen
#' @export
#' @seealso \link{Find_position_gigs_single_chromo} \link{MakeSNPDetailsTable_GIGS} \link{Use_snp_finder.py}
#' @examples
#' FindSNPpositions_gigs(c("5:1286516", "5:1322087", "5:1279790"))
FindSNPpositions_gigs <- function(snp_name, chatty=TRUE){
  if(length(grep("rs", snp_name)) > 0)
    stop("you are trying to find rs numbers in gigs")
  if(class(snp_name) == "character"){
    snp_name <- cbind(snp_name, rep("", length(snp_name)))
    snps <- snp_name
  }
  res <- NULL
  if(class(snp_name) == "snps_in_gene_regions" | class(snp_name) == "matrix")
    snps <- snp_name[,1]
  splitpos <- MakeSplitPos(snps)
  splitpos <- cbind(splitpos, snp_name[,2])
  chromosomes <- unique(splitpos[,2])
  for(chr in chromosomes){
    if(chatty) 
      print(paste("working on chromosome", chr))
    sub_splitpos <- splitpos[which(splitpos[,2] == chr),]
    if(class(sub_splitpos) == "character")
      sub_splitpos <- matrix(sub_splitpos, nrow=1)
    noGenesonChromo <- unique(sub_splitpos[which(sub_splitpos[,2] == chr), 4])
    for(numberGenes in noGenesonChromo){
      rowsforgene <- which(sub_splitpos[,4] == numberGenes)
      positions <- sub_splitpos[rowsforgene, 1]
      res2 <- Find_position_gigs_single_chromo(positions, chr)
      res2[,2] <- rep(numberGenes, dim(res2)[1])
      # print(res2)
      res <- rbind(res, res2)
    }
  }
  class(res) <- "snp_location_info"
  return(res)
}


#' Create Dosage Dataset in GIGSv2
#'
#' Create a new Dosage Dataset from GIGSv2
#'
#' This function will create a gigs dosage dataset, with individuals as rows and snps as columns. If times 
#' is selected, then the output wll be a list where the first element is the dosage data, and the second 
#' element is the computational time. 
#'
#' @param gigs_positions An object of the class "snp_location_info" that comes out of the FindSNPpositions* functions
#' @param chatty Option to print progress to screen
#' @param report.times Option to return computational time with dataset as a second object in the list
#' @export
#' @seealso \link{CreateDosageDataAcrossStudies} \link{MakeSNPDetailsTable_GIGS} \link{Use_snp_finder.py}
#' @examples
#' tmp <- FindSNPpositions_gigs(c("5:1286516", "5:1322087", "5:1279790"))
#' CreateDosageDataFromGigs(tmp)
CreateDosageDataFromGigs <- function(gigs_positions, chatty=TRUE, report.times=FALSE){
  nochromos <- unique(gigs_positions[,4])
  ddall <- data.frame(matrix(nrow=0, ncol=length(unique(gigs_positions[,3]))+3))
  colnames(ddall) <- c("study", "batch", "netcdf_ID", unique(gigs_positions[,3]))
  probs <- NULL
  probsnamevector <- NULL
  times <- NULL
  for(i in nochromos){
    sub_gigs_positions <- gigs_positions[which(gigs_positions[,4] == i),]  
    if(class(sub_gigs_positions) == "character"){
      sub_gigs_positions <- matrix(sub_gigs_positions, nrow=1)
    }
    nc <- nc_open(paste0(DataLocation("GIGSv2"), i))
    samples <- ncvar_get(nc, "Sample_ID", start=c(1, 1), count=c(-1,-1))
    snpname <- ncvar_get(nc, "SNP_Name", start=c(1,1), count=c(-1,-1))
    study <- rep(GetStudy(i), length(samples))
    batch <- rep(GetBatch(i), length(samples))
    ind <- 0
    if(chatty)
      cat(paste0("\nworking on ", i, " which has ", length(sub_gigs_positions[,5]), " snps: "))
    for(snpi in sub_gigs_positions[,3]){
      starttime <- proc.time()[[3]]
      ind <- ind + 1
      lo <- which(snpname == snpi)
      if(length(lo == 1)){
        if(chatty)
          cat(paste(ind), " ")
        dosage <- 2*ncvar_get(nc, "Prob_AA", start=c(lo,1), count=c(1, -1)) + ncvar_get(nc, "Prob_AB", start=c(lo,1), count=c(1,-1))
        probs <- cbind(probs, dosage)
        probsnamevector <- c(probsnamevector, paste0(sub_gigs_positions[ind,2], "_", sub_gigs_positions[ind,3]))
      }
      res <- cbind(study, batch, samples, probs)
      colnames(res) <- c("study", "batch", "netcdf_ID", probsnamevector)
      endtime <- proc.time()[[3]] - starttime
      times <- c(times, endtime)
    }
  }
  if(report.times)
    return(list(res=res, times=times))
  return(res)
}


#' MakeSNPDetailsTable_GIGS
#'
#' Create a SNP Table with details on gigs position and alleles
#'
#' For a vector of snps, this function will pull all of the information we want to release with hapmap data.  
#' This will create a table to use in the data release. Results will include: snp name, the count allele, 
#' the baseline allele, R2, and CAF. 
#'
#' @param snps A vector of snps (ie, c("1:123", "2:234"))
#' @param chatty Option to print progress to screen
#' @export
#' @seealso \link{CreateSNPDetailsTable} 
#' @examples
#' MakeSNPDetailsTable_GIGS(c("5:1286516", "5:1322087", "5:1279790"))
MakeSNPDetailsTable_GIGS <- function(snps, chatty=TRUE){
  if(class(snps) != "snp_location_info")
    snps <- FindSNPpositions_gigs(snps, chatty)
  snps <- cbind(snps, sapply(snps[,4], GetChromo, USE.NAMES=FALSE))
  res <- matrix(nrow=0, ncol=7)
  files <- DataLocation("GIGS_snp_names")
  files <- system(paste("ls ", files), intern=TRUE)
  filesnames <- sapply(files, GetChromo, USE.NAMES=FALSE)
  files <- files[which(filesnames %in% unique(snps[,6]))]
  for(i in files){
    if(chatty)
      print(paste("working on", i))
    chr <- GetChromo(i)
    sub_snps <- snps[which(snps[,6] == chr),]
    if(class(sub_snps) == "character")
      sub_snps <- matrix(sub_snps, nrow=1)
    tmp <- read.csv(i)
    dd <- tmp[which(tmp[,1] %in% sub_snps[,3]),]
    res <- rbind(res, dd)
  }
  res <- deFactorize(res)
  return(res)
}







##  ---------------------------------  ##
##                                     ##
##      Epi Data Search Functions      ##
##                                     ##
##  ---------------------------------  ##


#' WhichEpiFilesToInclude
#'
#' Sift through available Epi Files 
#'
#' This function was written before chatting with Yi about the various datasets. So use with caution.
#'
#' @param studies A vector of studies to be included in the count
#' @param files Argument to choose files with the extension 1/2. This is a super sucky function, because it ignores the other file types. 
#' @return Will return a list with two vectors. The first ($files) is a vector of file names, and the second ($sst) is a vector of the studies
#' @export
#' @seealso \link{MakeDataAvailabilityTable} \link{CreateEpiDatasetPerStudy}
#' @examples
#' WhichEpiFilesToInclude(c("101ccfr", "108dachs"))
WhichEpiFilesToInclude <- function(studies, files="1or2"){
  epifiles <- list.files(path=DataLocation("harmonized-epi"))
  if(files == "1or2"){
    epifiles <- epifiles[-grep("0.csv", epifiles)]  #not ready
    epifiles <- epifiles[-grep("e.csv", epifiles)]  # exome chip
    epifiles <- epifiles[-grep("osflag.csv", epifiles)]  #
    epifiles <- epifiles[-grep("seq.csv", epifiles)]  # exome chip
    epifiles <- epifiles[-grep("ad.csv", epifiles)]  # adenoma
    epifiles <- epifiles[-grep("hf.csv", epifiles)]  # 
    epifiles <- epifiles[-grep("_r.csv", epifiles)]  # 
    epifiles <- epifiles[grep("\\d+", epifiles)]  #remove if no study number
    epifilesToInclude <- NULL
    sst <- NULL
    for(i in epifiles){  #which studies to include
      studyname <- GetLastFileNameInPath(i)
      studyname <- strsplit(studyname, ".", fixed=TRUE)[[1]][1]
      if(sub("\\d*$", "", studyname) %in% sub("\\d*$", "", studies)){
        sst <- c(sst, studyname)
        epifilesToInclude[i] <- TRUE
      }
      else
        epifilesToInclude[i] <- FALSE
    }
  return(list(files=epifiles[epifilesToInclude], sst=sst))
  }
  if(files == "specific"){
    csvfilenames <- paste0(studies, ".csv")
    epifilelast <-  matrix(unlist(sapply(epifiles, strsplit, split="/")), ncol=length(strsplit(epifiles[1], "/")[[1]]), byrow=TRUE)[,8]
    epifiles <- epifiles[epifilelast %in% csvfilenames]
  }
  return(list(files=epifiles, sst=csvfilenames))
}


#' GiveStudiesNumbers
#'
#' Give Studies their numbers 
#'
#' This function will add the study number to the name. This was done to match epi datasets to gwas datasets (which have them included already)
#'
#' @param study A single study (ex: "whi", or "nhs")
#' @return Will return a new study name
#' @export
#' @seealso \link{MakeDataAvailabilityTable} \link{CreateSurvivalDataset}
#' @examples
#' GiveStudiesNumbers("whi")
GiveStudiesNumbers <- function(study){
  s <- sub("\\d*$", "", study)
  if(s == "whi") return(paste0("105", study))
  if(s == "vital") return(paste0("115", study))
  if(s == "plco") return(paste0("104", study))
  if(s == "hpfs") return(paste0("110", study))
  if(s == "phs") return(paste0("114", study))
  if(s == "nhs") return(paste0("113", study))
  if(s == "dals") return(paste0("103", study))
  else return("ADD A NEW ONE")
}


#' ConvertTrueFalse
#'
#' Convert True/False to other variables
#'
#' This function will convert TRUE and FALSE to something esle.  Even though you can add 0 (for example, TRUE+0)
#' to get 1 and 0, this function works by specifying any T/F variables. 
#'
#' @param TrueorFalse Boolean, either TRUE or FALSE
#' @param True Conversion for TRUE variable
#' @param False Conversion for FALSE variable
#' @return Will return conversion
#' @export
#' @seealso \link{MakeDataAvailabilityTable} 
#' @examples
#' ConvertTrueFalse(TRUE)
#' ConvertTrueFalse(TRUE, True="ppp", False="qqq")
ConvertTrueFalse <- function(TrueorFalse, True="1", False="0"){
  if(TrueorFalse) return(True)
  else return(False)
}


#' GrepForEpiVars
#'
#' Grep for Available Epi Variables
#'
#' This function will search the list of variables file for matched terms. Matches are returned and you can choose 
#' which variables to include this way.  
#'
#' @param grepTerms Which keywords to search (ex: "smoke", "BMI")
#' @return Will return a vector of matching terms
#' @export
#' @seealso \link{MakeDataAvailabilityTable} 
#' @examples
#' GrepForEpiVars("smoke")
#' GrepForEpiVars(c("BMI", "race"))
GrepForEpiVars <- function(grepTerms){
#  careful with some terms, like "sex", "case" it will pull in other variables
  listOfVariables <- NULL
  load(DataLocation("listOfEpiVars"))
  variablesOfInterest <- NULL
  vars <- NULL
  for(i in grepTerms){
    tog <- grep(tolower(i), listOfVariables[,3], ignore.case=TRUE)
    variablesOfInterest <- cbind(rep(i, length(tog)), as.character(listOfVariables[tog, 2]))
    vars <- rbind(vars, variablesOfInterest)
  }
  if("sex" %in% vars[,1]){
    ws <- which(vars[,1] == "sex")    
    ws2 <- which(vars[,2] == "sex")
    vars <- vars[-ws[which(!ws %in% ws2)],]
  }
  if(any(duplicated(vars[,2])))
    vars <- data.frame(vars[-which(duplicated(vars[,2])),], stringsAsFactors=FALSE)
  colnames(vars) <- c("keyword", "variable name")
  return(vars)
}  


#' MakeDataAvailabilityTable
#'
#' Make Data Availability Table for Epi Variables
#'
#' This function will return a table with the variables of interest as columns  
#'
#' @param epivars Which covariates to include
#' @param studies Which studies to pull, if left NULL it will use all
#' @param includeSurvival Boolean, Do you want to include survival variables
#' @param survvars Which survival variables to include
#' @return Will return a vector of matching terms
#' @export
#' @seealso \link{MakeDataAvailabilityTable} 
#' @examples
#' MakeDataAvailabilityTable("smoke")
#' MakeDataAvailabilityTable(c("smoke", "race", "BMI"))
MakeDataAvailabilityTable <- function(epivars=NULL, studies=NULL, includeSurvival=FALSE, survvars=NULL){
# could also do the number of individuals with data rather than any data
  if(is.null(studies))
    studies <- c("101ccfr", "109colo23", "112mec", "114phs", "115vital", "108dachs", 
                "103dals", "111fccs", "110hpfs", "113nhs", "102arctic", "117hrtccr", 
                "104plco", "105whi")
  if(includeSurvival & is.null(survvars))
    stop("You need to add which survival variates")
  listOfVariables <- NULL
  load(DataLocation("listOfEpiVars"))
  if(includeSurvival)
      epivars <- c(epivars, survvars)
  epi1 <- DataLocation("harmonized-epi")
  epifiles <- WhichEpiFilesToInclude(studies, files="1or2")
  m <- matrix(nrow=length(epivars), ncol=length(epifiles$files))
  rownames(m) <- epivars
  colnames(m) <- epifiles$sst
  for(j in sequence(length(epifiles$files))){
    tmp <- read.csv(paste0(epi1, epifiles$files[j], collapse="/"))
    m[,j] <- rownames(m) %in% colnames(tmp)
  }
  if(includeSurvival){
    if("103dals" %in% studies){  ##  same as dals1 / dals2?  
      tmp <- read.csv(DataLocation("Jihyoun-dals"))
      m <- cbind(m, rownames(m) %in% colnames(tmp))
      colnames(m)[length(colnames(m))] <- "103dals"
    }
    if("CPS2" %in% studies){
      tmp <- read.csv("Jihyoun-cps2")
      m <- cbind(m, rownames(m) %in% colnames(tmp))
      colnames(m)[length(colnames(m))] <- "CPS2"
    }
    survdir <- DataLocation("harmonized-epi")
    tmp <- read.csv(survdir)
    survcols <- tolower(as.character(unique(tmp[,1])))
    survcols <- sapply(survcols, GiveStudiesNumbers, USE.NAMES=FALSE)
    for(k in survcols){
      if(k %in% colnames(m)){
        whichmcol <- which(colnames(m) == k)
        whichmrow <- which(rownames(m) %in% survvars)
        m[whichmrow, whichmcol] <- rep(TRUE, length(whichmrow))
      }
      else
        m <- cbind(m, rownames(m) %in% colnames(tmp))
    }
  }
  m <- apply(m, c(1,2), ConvertTrueFalse, True="X")
  tot <- cbind(c(as.character(listOfVariables[which(listOfVariables[,2] %in% epivars),4]), rep("Surv", length(survvars))), m)
  colnames(tot)[1] <- "Type"
  return(tot)
}


#' CreateEpiDatasetPerStudy
#'
#' Create an Epi dataset from just a single study
#'
#' This function will return a table with subjects as rows and variables as columns. It only gets data 
#' from a single study, so in this way it is really an internal function for \link{CreateEpiDataset}.
#'
#' @param variables Which variables to collect (ex: "smoke", "BMI")
#' @param study Which study data to pull
#' @param files Which files to use from the study (this is bad...)
#' @param chatty Option to print progress to screen
#' @return Returns a dataset of subjects x variables
#' @export
#' @seealso \link{CreateEpiDataset} 
#' @examples
#' CreateEpiDatasetPerStudy("smoke", "101ccfr")
CreateEpiDatasetPerStudy <- function(variables, study, files="1or2", chatty=TRUE){
  if(any(c("censor", "crcdeath", "time_surv") %in% variables))
    variables[-which(variables %in% c("censor", "crcdeath", "time_surv"))]
  if(!"outc" %in% variables)
    variables <- c("outc", variables)
  epi1 <- DataLocation("harmonized-epi")
  epifiles <- WhichEpiFilesToInclude(study, files=files)
  m <- matrix(nrow=0, ncol=length(variables)+4)
  ind <- 0
  for(j in epifiles$files){
    ind <- ind+1
    Study <- sub(".csv", "", epifiles$sst[ind])
    tmp <- read.csv(paste0(epi1, j, collapse="/"))
    tmp <- cbind(Study, tmp[,which(colnames(tmp) %in% c("compassid", "netcdfid", "gecco_study", variables))])
    if(chatty)
      print(paste("working on", Study, "; dim =", paste(dim(tmp), collapse="_")))
    m <- rbind(tmp)
  }
  return(m)
}


#' CreateEpiDataset
#'
#' Create an Epi dataset
#'
#' This function will return a table with subjects as rows and variables as columns. It gets data 
#' from a single or multiple stuies.
#'
#' @param variables Which variables to collect (ex: "smoke", "BMI")
#' @param studies Which studies to pull, if left NULL it will use all
#' @param files Which files to use from the study (this is bad...)
#' @param chatty Option to print progress to screen
#' @return Returns a dataset of subjects as rows, and compassID, netcdfID, study name, and variables as columns
#' @export
#' @seealso \link{CreateEpiDatasetPerStudy} \link{Yi_GetEpiDataFromGigs}
#' @examples
#' CreateEpiDataset("smoke", "101ccfr")
#' CreateEpiDataset(c("smoke", "BMI"))
CreateEpiDataset <- function(variables, studies=NULL, files="1or2", chatty=TRUE){
  warning("This is a super crappy function.....check all of Yi's notes for which studies to include and when or just use her function")
  if(is.null(studies))
    studies <- c("101ccfr","109colo23", "112mec", "114phs", "115vital", "108dachs", "103dals", "111fccs", 
                 "110hpfs", "113nhs", "102arctic", "117hrtccr", "104plco", "105whi")
  res <- matrix(nrow=0, ncol=length(variables)+4)
  for(i in studies){
    res <- rbind(res, CreateEpiDatasetPerStudy(variables, i, files, chatty))
  }
  return(res)
}


#' CreateSurvivalDataset
#'
#' Create a Survival dataset
#'
#' This function will return a table with subjects as rows and variables as columns. It gets data 
#' from a single or multiple stuies.
#'
#' @param variables Which variables to collect. If "all", then it returns all columns with data.
#' @param studies Which studies to pull, if NULL, then it returns all studies
#' @param data Which data files to use. There are several different survival datasets floating about, you can choose to use the ones for pooled GECCO, separate GECCO, or separate ISACC
#' @return Returns a dataset of subjects as rows, and compassID, netcdfID, study name, and variables as columns
#' @export
#' @seealso \link{CreateEpiDatasetPerStudy} 
#' @examples
#' CreateSurvivalDataset("all")
#' l <- CreateSurvivalDataset("censor", data="separateGecco")
CreateSurvivalDataset <- function(variables="all", studies=NULL, data="pooledGecco"){
  if(!is.null(studies))
    stop("need to fix this so that it doesn't return all studies")
  if(variables == "all")
    variables <- c("age_dx", "censor", "crcdeath", "time_surv", "stage_update", "sex")
  variables <- tolower(unique(c("compassid", "netcdfid", "study", variables)))
  if(data == "pooledGecco"){
    survdir <- DataLocation("pooledSurv")
    tmp <- read.csv(survdir, stringsAsFactors=FALSE)
    variables <- variables[which(variables %in% colnames(tmp))]
    if("103dals" %in% studies){
      tmp2 <- read.csv(DataLocation("Jihyoun-dals"), stringsAsFactors=FALSE)
      colnames(tmp2) <- colnames(tmp)  #103dals has anydeath rather than censor
      tmp <- rbind(tmp, tmp2)
    }
    if("CPS2" %in% studies){
      tmp2 <- read.csv(DataLocation("Jihyoun-cps2"), stringsAsFactors=FALSE)
      colnames(tmp2) <- colnames(tmp)
      tmp <- rbind(tmp, tmp2)
    }
    res <- tmp[,which(colnames(tmp) %in% variables)]  #remove when not all variables
    res[,1] <- sapply(res[,1], tolower)
    res[,1] <- sapply(res[,1], GiveStudiesNumbers) 
  }
  if(data == "separateGecco"){  #need to pull in the studies separately, which should maximize the amount of data
    survdir <- DataLocation("separate-epi")
    files <- system(paste0("ls ", survdir, "*.csv"), intern=TRUE)
    files <- files[-grep("Combined", files)]
    res <- matrix(nrow=0, ncol=length(variables))
    colnames(res) <- variables
    studiesToKeep <- rep(NA, length(files))
    studiesFromFilenames <- sapply(files, GetLastFileNameInPath, USE.NAMES=FALSE)
    for(i in sequence(length(files))){
      tmp <- read.csv(files[i], stringsAsFactors=FALSE)  #need a clever way of dropping studies
      colnames(tmp) <- tolower(colnames(tmp))
      if("anydeath" %in% colnames(tmp))
        colnames(tmp)[which(colnames(tmp) == "anydeath")] <- "censor"  #stinking CPS2 comes in as anydeath
      tmp <- tmp[,which(colnames(tmp) %in% variables)]  #remove when not all variables
      if(any(!colnames(res) %in% colnames(tmp))){
        whichToAdd <- colnames(res)[which(!colnames(res) %in% colnames(tmp))]
        for(vta in whichToAdd){
          tmp <- cbind(tmp, rep(NA, dim(tmp)[1]))
          colnames(tmp)[dim(tmp)[2]] <- vta
        }
      }
#      res <- rbind(res, tmp)
      studiesToKeep[i] <- length(which(is.na(tmp)))/dim(tmp)[1]  #amount of missing data
    }
    names(studiesToKeep) <- studiesFromFilenames
    studiesToUse <- ToKeepOrNot(studiesToKeep)  #might be a good place to add certain studies only
    for(i in sequence(length(files))){  #only kept files
      if(studiesToUse[i]){
        print(paste("adding", names(studiesToUse[i])))
        tmp <- read.csv(files[i], stringsAsFactors=FALSE)
        colnames(tmp) <- tolower(colnames(tmp))
        if("anydeath" %in% colnames(tmp))
          colnames(tmp)[which(colnames(tmp) == "anydeath")] <- "censor"  #stinking CPS2 comes in as anydeath
        tmp <- tmp[,which(colnames(tmp) %in% variables)]  #remove when not all variables
        if(any(!colnames(res) %in% colnames(tmp))){
          whichToAdd <- colnames(res)[which(!colnames(res) %in% colnames(tmp))]
          for(vta in whichToAdd){
            tmp <- cbind(tmp, rep(NA, dim(tmp)[1]))
            colnames(tmp)[dim(tmp)[2]] <- vta
          }
        }
        res <- rbind(res, tmp)
      }
    }
    res <- res[-which(duplicated(res[,1])), ]
  }
  if(data == "separateIsacc"){
    survdir <- DataLocation("separate-isacc")
    files <- list.files(path=survdir, pattern="csv")
    files <- files[-grep("Combined", files)]
    res <- matrix(nrow=0, ncol=length(variables))
    colnames(res) <- variables
    for(i in sequence(length(files))){
      print(paste("adding", files[i]))
      tmp <- read.csv(paste0(survdir, files[i]), stringsAsFactors=FALSE)
      colnames(tmp) <- tolower(colnames(tmp))
      if("anydeath" %in% colnames(tmp))
        colnames(tmp)[which(colnames(tmp) == "anydeath")] <- "censor"  #stinking CPS2 comes in as anydeath
      tmp <- tmp[,which(colnames(tmp) %in% variables)]  #remove when not all variables
      if(any(!colnames(res) %in% colnames(tmp))){
        whichToAdd <- colnames(res)[which(!colnames(res) %in% colnames(tmp))]
        for(vta in whichToAdd){
          tmp <- cbind(tmp, rep(NA, dim(tmp)[1]))
          colnames(tmp)[dim(tmp)[2]] <- vta
        }
      }
      res <- rbind(res, tmp)
    }  
  }   
  return(res)
}


#' ToKeepOrNot
#'
#' To Keep a Study or Discard it
#'
#' For a vector of studies, it returns T/F to keep each or not. Really odd function I wrote...might 
#' be worthless.  But it tries to maximize the amount of data. It will pull in studies with the same
#' name prefix and see which has more data and keep that one. 
#'
#' @param vectorofstudiestokeep Names vector of studies and their amount of missing data
#' @return Returns boolean vector of to keep study (TRUE) or not (FALSE)
#' @export
#' @seealso \link{CreateSurvivalDataset} 
ToKeepOrNot <- function(vectorofstudiestokeep){
  name <- sapply(names(vectorofstudiestokeep), gsub, pattern="[-_]", replacement=" ", USE.NAMES=FALSE)
  for(i in sequence(length(name))){
    name[i] <- strsplit(name[i], split=" ", fixed=TRUE)[[1]][1]
  }  
  keepVector <- rep(NA, length(name))
  for(i in unique(name)){
    keepVector[which(name == i)] <- vectorofstudiestokeep[which(name == i)] == min(vectorofstudiestokeep[which(name == i)])
  }
  names(keepVector) <- names(vectorofstudiestokeep)
  return(keepVector)
}


#' MakeEpiDetailsTable
#'
#' Create a Epi-Variable Table with details
#'
#' Returns a table with the epi variable, a short description, and the permissible values.
#' This table can be used when we release the data.  
#'
#' @param variables A vector of epi variables
#' @export
#' @seealso \link{CreateEpiDataset} 
#' @examples
#' MakeEpiDetailsTable(c("age_ref", "BMI", "alcoholc"))
MakeEpiDetailsTable <- function(variables) {
  listOfVariables <- NULL
  load(DataLocation("listOfEpiVars"))
  dets <- listOfVariables[which(listOfVariables[,2] %in% variables),]
  dets <- dets[match(variables, dets[,2]),]
  if(any(!variables %in% listOfVariables[,2]))
    warning(paste(variables[which(!variables %in% listOfVariables[,2])], "does not seem to be in the file"))
  dets <- dets[c(3,2,5)]
  dets[,1] <- sapply(dets[,1], gsub, pattern=",", replacement=";")
  dets[,3] <- sapply(dets[,3], gsub, pattern=",", replacement=";")
  return(dets)
}





##  ---------------------------------  ##
##                                     ##
##          Merging Datasets           ##
##                                     ##
##  ---------------------------------  ##


#' MergeEpiAndSurvivalData
#'
#' Merge Epi and Survival Datasets
#'
#' This is very straightforward function to merge two datasets by some variable.  Really could 
#' just use the merge function, but this catches a few extra things.  
#'
#' @param EpiDataset An Epi dataset
#' @param SurvivalDataset A Survival dataset
#' @param merge_by How to merge the two.  For Epi and Survival, it will always be by compassid
#' @param all Keep all records (TRUE) or just those that intersect (FALSE)
#' @return Returns a dataset of subjects as rows, and compassID, netcdfID, study name, and variables as columns
#' @export
#' @seealso \link{CreateEpiDataset} \link{CreateSurvivalDataset}
#' @examples
#' epi <- CreateEpiDataset(c("smoke", "BMI"))
#' surv <- CreateSurvivalDataset("all")
#' MergeEpiAndSurvivalData(epi, surv)
#' MergeEpiAndSurvivalData(epi, surv, all=FALSE)
MergeEpiAndSurvivalData <- function(EpiDataset, SurvivalDataset, merge_by="compassid", all=TRUE){
# maybe try to make this smart, bu detecting *.x or *.y columns and dealing with them
  mergedSet <- merge(EpiDataset, SurvivalDataset, by=merge_by, all=all)  #compare compassID
  if(!all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
    return("stop, merge didn't work right")
#  if(all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
#   mergedSet <- mergedSet[,-which(colnames(mergedSet) == "compassid.y")]
#  mergedSet <- mergedSet[,-which(colnames(mergedSet) == "sex.y")]
  return(mergedSet)
}


## New function to merge survival/GWAS data
# Do we have to go through epi data? 



#' MergeEpiAndGIGSdata
#'
#' Merge Epi and GIGS Datasets
#'
#' This is very straightforward function to merge two datasets by some variable.  Really could 
#' just use the merge function, but this catches a few extra things.  
#'
#' @param EpiDataset An Epi dataset
#' @param GigsDataset A Gigs dataset
#' @param merge_by How to merge the two.  For Epi and Gigs, it will always be by netcdfid
#' @param all Keep all records (TRUE) or just those that intersect (FALSE)
#' @return Returns a dataset of subjects as rows, and compassID, netcdfID, study name, and variables as columns
#' @export
#' @seealso \link{MergeEpiAndSurvivalData}
#' @examples
#' epi <- CreateEpiDataset(c("smoke", "BMI"))
#' tmp <- FindSNPpositions_gigs(c("5:1286516", "5:1322087", "5:1279790"))
#' gigs <- CreateDosageDataFromGigs(tmp)
#' MergeEpiAndGIGSdata(epi, gigs, all=FALSE)
MergeEpiAndGIGSdata <- function(EpiDataset, GigsDataset, merge_by="netcdfid", all=TRUE){
  if(tolower(merge_by) == "netcdfid"){
    x <- "netcdfid"
    y <- "netcdf_ID"
  }
  mergedSet <- merge(EpiDataset, GigsDataset, by.x=x, by.y=y, all=all)  #compare compassID
  if(!all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
    return("stop, merge didn't work right")
  return(mergedSet)
}









##  ---------------------------------  ##
##                                     ##
##       Other Peoples Functions       ##
##                                     ##
##  ---------------------------------  ##



#' Yi_GetEpiDataFromGigs
#'
#' Yis Function for Creating Epi Datast Using Gigs PCA
#'
#' 
#'
#' @param env Epi variables to include
#' @return Returns a dataset of subjects as rows, and variables plus lots of other info (like drop 
#' status) as columns. Also includes PCs from gigs data. 
#' @export
#' @seealso \link{MergeEpiAndSurvivalData}
#' @examples
#' env <- c('famhx1','famhx_reln1','ibd','study_site','sex','asp_ref','aspirin','cancer_site_sum1', 
#'          'cancer_site_sum2','age_dx','BMI5','age_dxsel','stage','stage2','stage3')
#' Yi_GetEpiDataFromGigs(c('famhx1','ibd'))
Yi_GetEpiDataFromGigs <- function(env){
# can not include variables which are in the sameple file (they get added anyway)
  gigs_sample <- NULL
  Sample_ID <- NULL
  PC <- NULL
  load(DataLocation("gigs_sample"))
  if(any(env %in% colnames(gigs_sample)))
    env <- env[-which(env %in% colnames(gigs_sample))]
  if(any(c("censor", "crcdeath", "time_surv", "outc") %in% env)){
    env <- env[-which(env %in% c("censor", "crcdeath", "time_surv", "outc"))]
    warning("env includes survival, which will have to be done separately and merged")
  }
  #Gpath <- MakePathtoPeters_U('/GECCO_DATA_POOLED/GIGS_V2/')
  nc <- nc_open(paste(DataLocation("GIGSv2"), 'gigs_v2_chr22.nc', sep=''))
  SampleID <- ncvar_get(nc, 'Sample_ID')
  Study <- ncvar_get(nc, 'Study')
  gigs2 <- data.frame(SampleID,Study,stringsAsFactors=F)
  
  gigs2 <- merge(gigs_sample,gigs2,by.x='netcdfid',by.y='SampleID')

  Epath <- DataLocation("harmonized-epi")

  sample0 <- data.frame(study = c(101:104,105,108:115,117),
		      lab = c('101ccfr0',"102arctic0",'103dals0','104plco0','105whi0',
			      '108dachs0',"109colo230",'110hpfs0','111fccs0',"112mec0",
			      '113nhs0',"114phs0",'115vital0','117hrtccr0'),stringsAsFactors=F)
  epidata = NULL

  #== Epi variables included 
  # env = c('famhx1','famhx_reln1','ibd','study_site','sex','asp_ref','aspirin','cancer_site_sum1',
  #        'cancer_site_sum2','age_dx','BMI5','age_dxsel','stage','stage2','stage3')

  # if(any(env %in% colnames(gigs_sample)))   #add this...not sure it works yet
  #   env <- env[-which(env %in% comnames(gigs_sample))]
        
  tab.sum <- data.frame(study=unique(gigs_sample$study),wgs=NA,epi=NA)
  rownames(tab.sum) = unique(gigs_sample$study)
  for(i in unique(gigs2$study)){
    print(i)
    dat <- read.csv(paste0(Epath,sample0[sample0$study==i,'lab'],'.csv'),stringsAsFactors=F)
    dat <- dat[,c('compassid','outc',env)]
    std <- sample0[sample0$study==i,'lab']
    if(i %in% 105){ # WHI samples from exomechip 
      dat.e <- read.csv(paste0(Epath,substr(std,1,nchar(std)-1),'_e.csv'))
      if(any(!dat.e$compassid %in% dat$compassid))  # Barbs addition, because it won't run if all ==
        dat <- rbind(dat,dat.e[!dat.e$compassid %in% dat$compassid,c('compassid','outc',env)])
    }
    if(i %in% 101){ #CCFR samples from CCFR and CCFR 2 
      dat.1 <- read.csv(paste0(Epath,substr(std,1,nchar(std)-1),'.csv'))
      dat.2 <- read.csv(paste0(Epath,substr(std,1,nchar(std)-1),'2.csv'))
      dat <- rbind(dat,dat.1[dat.1$compassid %in% setdiff(dat.1$compassid,dat$compassid),c('compassid','outc',env)])
      dat <- rbind(dat,dat.2[dat.2$compassid %in% setdiff(dat.2$compassid,dat$compassid),c('compassid','outc',env)])
    }
    wgs <- gigs2[gigs2$study == i,]
    epi <- merge(wgs,dat,by='compassid',all.x=T)
    tab.sum[as.character(i),-1] <- c(nrow(wgs),length(intersect(wgs$compassid,dat$compassid)))
    epidata <- rbind(epidata,epi)
  }  

  #= update outcome for WHI samples selected from whole seq 
  change.whi <- read.csv(paste0(Epath,'105whi_seq.csv'),stringsAsFactors=F)
  tmp <- epidata[!((epidata$case %in% 0 & epidata$outc %in% 'Control') | (epidata$case %in% 1 & epidata$outc %in% 'Case')),]
  changed <- as.character(tmp[tmp$drop==0,'compassid'])
  epidata[epidata$compassid %in% changed,c('compassid','outc',env)] <- change.whi[change.whi$compassid %in% changed,c('compassid','outc',env)]  #== change genotypingphase for ccfr2
  epidata$genotypingphase <- as.character(epidata$genotypingphase)
  epidata$genotypingphase[epidata$genotypingphase %in% 'CCFR 1 1/2'] <- 'CCFR 1'
  epidata$genotypingphase[epidata$genotypingphase %in% 'CCFR 1' & epidata$Study %in% c('101ccfr_usc21','101ccfr_usc22')] <- 'CCFR 2'

  #== change PCs to gigs2 PC
  epidata <- epidata[,!colnames(epidata) %in% c('pc1','pc2','pc3','Study')]
  load(DataLocation("gigs_pca"))
  pcs <- data.frame(Sample_ID,PC[,1:3],stringsAsFactors=F)
  colnames(pcs) <- c('netcdfid','pc1','pc2','pc3')
  epidata <- merge(epidata,pcs,by='netcdfid')
  epidata$drop <- 0
  
  return(epidata)
  #save(epidata,file=paste0(Epath,'GIGS2_EpiData.Rdata'))
}



##  ---------------------------------  ##
##                                     ##
##                NOTES                ##
##                                     ##
##  ---------------------------------  ##


# NOTES from sitting with Flora:
# snp data with R2 allele freqs imputation status
# helps create hap map data table
# count allele and baseline are in the legend file
# load('/Volumes/researcher/Peters_U/Results_Database/Version 4/Results - Meta of Marginal/gecco_version3_SNPinfo_23studies.Rdata')
# CAF.Matrix has count allele
# Imputed.matrix is for imputation (1), genotyped (0), error (NA)
# R2.matrix if imputed==1, then R2 gets calced.  If only genotyped data, then it will automatically be 1, but you might want to ignore (because it will inflate mean)


# marginal analysis for each snp is here:
#load('/Volumes/researcher/Peters_U/Results_Database/Version 4/Results - Meta of Marginal/gecco_marginal_version4_23studies.Rdata')


# epi data
# /Volumes/researcher/Peters_U/Data Harmonization/Post-harmonization/Data
# files that end in 0' don't touch
# files that end in 'e' exome chip




##  ---------------------------------  ##
##                                     ##
##      new datasets for parsing       ##
##                                     ##
##  ---------------------------------  ##

# Make SNP names list with all RS numbers for each study
#setwd(paste0(barb.working, "/HapMap_data/snp_names"))
#for(i in list.files(hapmap.data)){
#  # i <- list.files(hapmap.data)[15]
#  datatypes <- list.files(paste(hapmap.data, i, sep="/"))
#  for(typ in datatypes){
#    dosages <- list.files(paste(hapmap.data, i, typ, "mach", sep="/"))
#    snp_names <- list()
#    for(dos in dosages){
#      print(dos)
#      nc <- open.ncdf(paste(hapmap.data, i, typ, "mach", dos, sep="/"))
#      snp_names[[dos]] <- get.var.ncdf(nc, "SNP_Name", start=c(1,1), count=c(-1,-1))
#      save(snp_names, file=paste0("snp_names_", typ, ".Rdata"))  #save iteratively
#    }
#    class(snp_names) <- "snp_names"
#    save(snp_names, file=paste0("snp_names_", typ, ".Rdata"))  #save final with new class
#  }
#}

# Make SNP names list with all RS numbers for GIGSV2
#setwd(MakePathtoPeters_U(names.root))
# don't need this because gigsv2/snp_files has csv files with rs numbers



# To find rs numbers in GIGS, you need to know their respective locations
# These were done for GIGSv1, but not for GIGSv2
# Keith made the original Rdata files and I pulled them into R (took forever) and then created a two column csv that could then be grepped in system rather than having to load it all in R

# load("/shared/silo_researcher/Peters_U/GECCO_Working/keithworking/t249-work/WGS-rs-mapping.Rdata")
# whichSNPsHaveNames <- which(dbSNP_rs_name != "NA")
# write.table(matrix(c("rs_name", "position"), nrow=1), file="rs_names_to_gigs_positions.csv", col.names=FALSE, row.names=FALSE, sep=",", quote=FALSE)
#for(i in whichSNPsHaveNames){
#  write.table(matrix(c(dbSNP_rs_name[i], WGS_SNP_name[i]), nrow=1), file="rs_names_to_gigs_positions.csv", col.names=FALSE, row.names=FALSE, sep=",", append=TRUE, quote=FALSE)
#}

# file <- "/Volumes/bbanbury/Peters_U/GECCO_Working/barb_working/rs_names_to_gigs_positions.csv"
# system(paste("grep rs144022023", file), intern=TRUE)







