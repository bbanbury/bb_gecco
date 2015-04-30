##  ---------------------------------  ##
##                                     ##
##        Data Search Functions        ##
##             (c) BBanbury            ##
##             24 April 15             ##
##                                     ##
##  ---------------------------------  ##


require(ncdf)
require(ncdf4)

## In the middle of transforming the files to a single function throughout.  Alos need to finish documentation for all the functions after the regulatory ones.  


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
#' @return Returns a path to the directory. THis function is where we can store where data lives in directories, so that when we change where data lives, we only have to change it once.  
#' @seealso \link{MakePathtoNewcomb_P} \link{MakePathtoPeters_U}
#' @examples
#' DataLocation(hapmap_snp_names)
DataLocation <- function(whichData){
  # GWAS
  if(whichData == "hapmap_snp_names")  return(MakePathtoPeters_U("/GECCO_Working/barb_working/HapMap_data/snp_names/"))
  if(whichData == "hapmap")  return(MakePathtoPeters_U("/hapmap.data/GECCO_Data/"))
  if(whichData == "hapmap_SNPinfo")  return(MakePathtoPeters_U("GECCO_Working/barb_working/HapMap_data/"))
  if(whichData == "rs_to_gigs_file")  return(MakePathtoPeters_U("/GECCO_Working/barb_working/rs_names_to_gigs_positions.csv"))
  if(whichData == "gigs_sample") return(MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V1/sample_files/gigs_sample.Rdata"))
  if(whichData == "GIGSv2")  return(MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V2/"))
  if(whichData == "GIGS_snp_names")  return(MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V2/snp_files/*.csv"))
  if(whichData == "gigs_pca")  return(MakePathtoPeters_U('GECCO_Working/keithworking/t275-work/pca.Rdata'))
  # Epi / Survival
  if(whichData == "harmonized-epi")  return(MakePathtoPeters_U("/Data\\ Harmonization/Post-harmonization/Data/", server="cs"))
  if(whichData == "Jihyoun-dals")  return(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/DALS-surv-dat-04152014.csv"))
  if(whichData == "Jihyoun-cps2")  return(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/CPS2-surv-dat-04152014.csv"))
  if(whichData == "pooledSurv")  return(MakePathtoPeters_U("/GECCO_Working/mpassareworking/Survival/Combined\ Survival\ Update\ Has\ Surv\ Pooled.csv"))
  if(whichData == "listOfEpiVars")  return(MakePathtoPeters_U("/GECCO_Working/barb_working/listOfVariables.Rdata"))
  if(whichData == "separate-epi")  return(MakePathtoPeters_U("/GECCO_Working/mpassareworking/Survival/Data/"))
  if(whichData == "separate-isacc")  return(MakePathtoNewcomb_P("/Molecular Correlates_ISACC/Survival data harmonization/Harmonized data/", "cs"))
  #software or other
  if(whichData == "software")  return(MakePathtoPeters_U("/PetersGrp/GECCO_Software/"))
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
# make path no matter if on local or rhino
# server can also be "cs" for "researcher server
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
# make path no matter if on local or rhino
# server can also be "cs" for "researcher server
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
#' This function prints a short summary of the SNP names to console, so it doesn't freeze up the machine if the list is huge. 
#'
#' @param x an object in the class "snp_names"
#' @export
#' @seealso \link{find_rs_hapmap} \link{FindSNPposition_hapmap}
#' @examples
#' load(DataLocation("hapmap_snp_names"), snp_names_113nhs_omni1.Rdata")
#' print(snp_names)
print.snp_names <- function(x){
  writeLines("SNP names data contains:")
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
#' @export
#' @seealso \link{Use_snp_finder.py} \link{FindSNPpositions_gigs}
#' @examples
#' Use_snp_finder.py("MYC")
print.snps_in_gene_regions <- function(x, ...){
  writeLines(paste("This is a matrix of snps in", length(unique(x[,2])), "gene regions."))
  for(g in unique(x[,2])){
    writeLines(paste("     -- ", g, "has", length(which(x[,2] == g)), "snps"))
  }
}


#' print.snp_location_info
#'
#' Compact Display of SNPs locations
#'
#' This function prints a short summary of the SNP locations to console, so it doesn't freeze up the machine if the list is huge. 
#'
#' @param x an object in the class "snp_location_info"
#' @export
#' @seealso \link{FindSNPposition_hapmap} \link{FindSNPpositions_gigs} \link{MakeSNPDetailsTable_GIGS}
#' @examples
#' FindSNPposition_hapmap("rs2965667")
print.snp_location_info <- function(x, ...){
  writeLines(paste("This is a matrix of", dim(x)[1], "snps", "and all their locality information"))
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
#' This function returns a chromosome from a filename 
#'
#' @param ncdfFileName An ncdf file name with chromosome number at the end
#' @export
#' @seealso \link{GetStudy} \link{GetBatch} \link{GetLastFileNameInPath}
#' @examples
#' GetChromo("101ccfr_usc2_merged_dosage_5.nc")
GetChromo <- function(ncdfFileName){
  ncdfFileName  <- GetLastFileNameInPath(ncdfFileName)
  return(strsplit(ncdfFileName, "[_.]")[[1]][length(strsplit(ncdfFileName, "[_.]")[[1]]) - 1])
}


#' Get Study
#'
#' Get Study out of ncdf file name
#'
#' This function returns a study from a filename 
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



##  ---------------------------------  ##
##                                     ##
##           Hapmap Functions          ##
##                                     ##
##  ---------------------------------  ##


find_rs_hapmap <- function(rs_number, snp_names){
# snp_names_file should be an Rdata file in the class snp_names with a list of names 
# could make this an executable
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
# find_rs_hapmap(c("rs2286139", "rs11127519"), snp_names)  #works with knowns


FindSNPposition_hapmap <- function(rs_number, studies=NULL, chatty=TRUE){
# for a vector of rs_numbers 
# for a vector of c(studies)
# create rs_positions table (and also rbind these to save)
# and then rbind results
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
#RSAcross <- FindSNPposition_hapmap(rs, studies)
#FindSNPposition_hapmap(c("rs11412"), studies, MakePathtoPeters_U(names.root))


CreateDosageDataPerStudy <- function(rs_positions, directory=NULL){
#rs_positions should be like RSAcross (output from FindSNPpositions* functions)
# make this its own class too and print statement
  files <- unique(rs_positions[,4])
  probs <- NULL
  probsnamevector <- NULL
  for(whichFile in files){
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
# CreateDosageDataPerStudy(rs_positions)


CreateDosageDataAcrossStudies <- function(rs_positions, saveToRdata=NULL){
  # for a vector of c(rs_numbers)
  # for a vector of c(studies)
  #if saveToData is a filename then it will save to working dir
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
  #if(!is.null(saveToRdata)){
  #  
  #}
  return(ddall)
}
# CreateDosageDataAcrossStudies(rs_positions, hapmap.dir)


GetCountAndBaselineAlleles <- function(rs_numbers, directory=NULL){
# Get count alleles from legend file
# will return a matrix with rs, position, and alleles in number format
  res <- matrix(nrow=length(rs_numbers), ncol=4)
  colnames(res) <- c("rs_number", "position", "Allele1", "Allele2")
  res[,1] <- rs_numbers
  p <- paste0(directory, "legend-augmented.nc")
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


CreateSNPDetailsTable <- function(rs_numbers, studies){
  # hapmap
  # for a given list of snps, we want to create a table to be released with data
  # should include: snp name, which chromosome it is on, the number of studies genotyped, the number of studies imputed, the count allele, the baseline allele, the mean R2, the R2Range, the mean CAF, and the CAF range
  # be in barb_working/HapMap_data
  # load(paste0(MakePathtoPeters_U(directory), "gecco_version3_SNPinfo_23studies.Rdata"))
  dataNeeded <- c("snpsall", "Imputed.matrix", "R2.matrix", "CAF.matrix")
  if(!all(dataNeeded %in% ls())){
    whichNotLoaded <- which(!dataNeeded %in% ls())
    for(i in sequence(length(whichNotLoaded))){
      load(paste0(DataLocation(hapmap_SNPinfo), dataNeeded[i], ".Rdata"))
    }
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
  alleles <- GetCountAndBaselineAlleles(rs_numbers, DataLocation("hapmap_SNPinfo"))
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
# CreateSNPDetailsTable(rs, studies)




##  ---------------------------------  ##
##                                     ##
##           GIGSv2 Functions          ##
##                                     ##
##  ---------------------------------  ##


find_rs_to_gigs <- function(rs_number, chatty=TRUE){
# use rs_to_gigs_positions to match known rs numbers with positions in gigs
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


Find_position_gigs_single_chromo <- function(position, chromosome){
# position can be a vector of positions in the format 1:234
  locs <- matrix(nrow=length(position), ncol=5)
  colnames(locs) <- c("study", "gene", "gigs_name", "ncdf file", "position")
  file <- paste0("gigs_v2_chr", chromosome, ".nc")
  nc <- nc_open(paste0(DataLocation("GIGSv2"), file))
  snpnames <- ncvar_get(nc, "SNP_Name", start=c(1, 1), count=c(-1,-1))
  tocollect <- which(snpnames %in% position)
#  study <- ncvar_get(nc, "Study")[tocollect]
#  study <- ncvar_get(nc, "Position", start=c(tocollect), count=c(length(tocollect)))
#  pos <- ncvar_get(nc, "Position")[tocollect]  #weird numbers...
  pos <- tocollect

  locs[,1] <- rep("gigs", length(position))
  locs[,2] <- rep(NA, length(position))
  locs[,3] <- position
  locs[,4] <- rep(file, length(position))
  locs[,5] <- pos  
  return(locs)
}


FindSNPpositions_gigs <- function(snp_name, chatty=TRUE){
# snp_name can either be a vector of positions (ie, 1:234) or in the class "snps_in_gene_regions"
  if(class(snp_name) == "character"){
    snp_name <- cbind(snp_name, rep("Not given", length(snp_name)))
    snps <- snp_name
  }
  res <- NULL
  if(class(snp_name) == "snps_in_gene_regions" | class(snp_name) == "matrix")
    snps <- snp_name[,1]
  splitpos <- matrix(unlist(sapply(snps, strsplit, split=":")), ncol=2, byrow=TRUE)
  splitpos <- cbind(snp_name, splitpos)
  chromosomes <- unique(splitpos[,3])
  for(chr in chromosomes){
    if(chatty) 
      print(paste("working on chromosome", chr))
    sub_splitpos <- splitpos[which(splitpos[,3] == chr),]
    if(class(sub_splitpos) == "character")
      sub_splitpos <- matrix(sub_splitpos, nrow=1)
    noGenesonChromo <- unique(sub_splitpos[which(sub_splitpos[,3] == chr), 2])
    for(numberGenes in noGenesonChromo){
      rowsforgene <- which(sub_splitpos[,2] == numberGenes)
      positions <- sub_splitpos[rowsforgene, 1]
      res2 <- Find_position_gigs_single_chromo(positions, chr)
      res2[,2] <- rep(numberGenes, dim(res2)[1])
      # print(res2)
      res <- rbind(res, res2)
    }
  }
  #res <- res[match(snps, res[,3]),]  #return in original order...broken
  class(res) <- "snp_location_info"
  return(res)
}
# gigs_positions <- FindSNPpositions_gigs(snp_name)
# FindSNPpositions_gigs(snp_name)->l
#  l[which(l[,2] == "TINF2"),]
# FindSNPpositions_gigs(chr14)

CreateDosageDataFromGigs <- function(gigs_positions, chatty=TRUE, times=FALSE){
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
    study <- rep(GetStudy(i), length(samples))
    batch <- rep(GetBatch(i), length(samples))
    ind <- 0
    if(chatty)
      print(paste0("working on ", i, "which has ", length(sub_gigs_positions[,5]), "snps"))
    for(snpi in sub_gigs_positions[,5]){
      starttime <- proc.time()[[3]]
      ind <- ind + 1
      if(chatty)
        cat(paste(ind), " ")
      lo <- as.numeric(sub_gigs_positions[,5][ind])
      if(ncvar_get(nc, "SNP_Name", start=c(1, lo), count=c(-1,1)) == sub_gigs_positions[ind,3]){  #here to check that rs number matches correctly
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
  if(times)
   return(list(res=res, times=times))
  return(res)
}


MakeSNPDetailsTable_GIGS <- function(snps, chatty=TRUE){
  if(class(snps) != "snp_location_info")
    snps <- FindSNPpositions_gigs(snps)
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
    tmp <- read.csv(i)
    res <- rbind(res, tmp[which(tmp[,1] %in% sub_snps[,3]),])
  }
  return(res)
}







##  ---------------------------------  ##
##                                     ##
##      Epi Data Search Functions      ##
##                                     ##
##  ---------------------------------  ##


WhichEpiFilesToInclude <- function(epifiles, studies, files="1or2"){
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

ConvertTrueFalse <- function(TrueorFalse, True="1", False="0"){
  if(TrueorFalse) return(True)
  else return(False)
}


GrepForEpiVars <- function(grepTerms, pathToEpiVariables){
#  this function will grep for variable names that match some descriptor terms
#  for example, find all variables that are associated with "smoking"
#  careful with some terms, like "sex", "case" it will pull in other variables
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
  vars <- data.frame(vars[-which(duplicated(vars[,2])),], stringsAsFactors=FALSE)
  colnames(vars) <- c("keyword", "variable name")
  return(vars)
}  
# GrepForEpiVars("smoke", pathToEpiVariables)
# GrepForEpiVars(grepTerms, pathToEpiVariables)


MakeDataAvailabilityTable <- function(epivars=NULL, survvars=NULL, studies, pathToEpiVariables, includeSurvival=TRUE){
#eventually make another argument with pathToEpiData, whenever it is housed together
  if(includeSurvival && is.null(survvars))
    stop("You need to add which survival variates")
  load(DataLocation("listOfEpiVars"))
  if(includeSurvival)
      epivars <- c(epivars, survvars)
  epi1 <- DataLocation("harmonized-epi")
  epifiles <- WhichEpiFilesToInclude(system(paste0("ls ", epi1, "*.csv"), intern=TRUE), studies, files="1or2")
  m <- matrix(nrow=length(epivars), ncol=length(epifiles$files))
  rownames(m) <- epivars
  colnames(m) <- epifiles$sst
  for(j in sequence(length(epifiles$files))){
    tmp <- read.csv(epifiles$files[j])
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



CreateEpiDatasetPerStudy <- function(variables, study, files="1or2", chatty=TRUE){
# gather variables for a single study
  if(any(c("censor", "crcdeath", "time_surv") %in% variables))
    variables[-which(variables %in% c("censor", "crcdeath", "time_surv"))]
  if(!"outc" %in% variables)
    variables <- c("outc", variables)
  epi1 <- DataLocation("harmonized-epi")
  epifiles <- WhichEpiFilesToInclude(system(paste0("ls ", epi1, "*.csv"), intern=TRUE), study, files=files)
  m <- matrix(nrow=0, ncol=length(variables)+4)
  ind <- 0
  for(j in epifiles$files){
    ind <- ind+1
    Study <- sub(".csv", "", epifiles$sst[ind])
    tmp <- read.csv(j)
    tmp <- cbind(Study, tmp[,which(colnames(tmp) %in% c("compassid", "netcdfid", "gecco_study", variables))])
    if(chatty)
      print(paste("working on", Study, "; dim =", paste(dim(tmp), collapse="_")))
    m <- rbind(tmp)
  }
  return(m)
}
# CreateEpiDatasetPerStudy(vars, study)


CreateEpiDataset <- function(variables, studies=NULL, files="1or2", chatty=TRUE){
# then each row will be compassID, netcdfID, study, data
  if(is.null(studies))
    studies <- c("101ccfr","109colo23", "112mec", "114phs", "115vital", "108dachs", "103dals", "111fccs", "110hpfs", "113nhs", "102arctic", "117hrtccr", "104plco", "105whi")
  res <- matrix(nrow=0, ncol=length(variables)+4)
  for(i in studies){
    res <- rbind(res, CreateEpiDatasetPerStudy(variables, i, files, chatty))
  }
  return(res)
}

CreateSurvivalDataset <- function(variables="all", studies=NULL, data="pooledGecco"){
#make dataset with "pooledGecco", "separateGecco", "separateIsacc"
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
      studiesToKeep[i] <- length(which(is.na(tmp)))/dim(tmp)[1]
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


ToKeepOrNot <- function(vectorofstudiestokeep){
    #return a vector as long with T/F to keep per study
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


MakeEpiDetailsTable <- function(variables) {
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



## New function to merge epi/GWAS data?

MergeEpiAndGIGSdata <- function(EpiDataset, GigsDataset, merge_by="netcdfid"){
  mergedSet <- merge(EpiDataset, GigsDataset, by=merge_by, all=TRUE)  #compare compassID
  if(!all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
    return("stop, merge didn't work right")
  return(mergedSet)
}











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









##  ---------------------------------  ##
##                                     ##
##       Other Peoples Functions       ##
##                                     ##
##  ---------------------------------  ##



Yi_GetEpiDataFromGigs <- function(env){
# can not include variables which are in the sameple file (they get added anyway)
  load(DataLocation("gigs_sample"))
  if(any(env %in% colnames(gigs_sample)))
    env <- env[-which(env %in% colnames(gigs_sample))]
  if(any(c("censor", "crcdeath", "time_surv", "outc") %in% env)){
    env <- env[-which(env %in% c("censor", "crcdeath", "time_surv", "outc"))]
    warning("env includes survival, which will have to be done separately and merged")
  }
  #Gpath <- MakePathtoPeters_U('/GECCO_DATA_POOLED/GIGS_V2/')
  nc <- nc_open(paste(DataLocation("GIGSv2"), 'gigs_v2_chr22.nc', sep=''))
  SampleID <- ncvar_get( nc, 'Sample_ID')
  Study <- ncvar_get( nc, 'Study')
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












