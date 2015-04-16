##  ---------------------------------  ##
##                                     ##
##        Data Search Functions        ##
##             (c) BBanbury            ##
##             15 April 15             ##
##                                     ##
##  ---------------------------------  ##


require(ncdf)
require(ncdf4)




##  ---------------------------------  ##
##                                     ##
##         Assistant Functions         ##
##                                     ##
##  ---------------------------------  ##


ChangeStudyNames <- function(study){
# study names changes from paper proposals to datasets
  if(study == "ASTERISK") return("111fccs")
  else if(study == "OFCCR") return("102arctic")
  else if(study == "PMH-CCFR") return("117hrtccr")
  else return(study)
}
# ChangeStudyNames("OFCCR")
# sapply(studies, ChangeStudyNames)


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


print.snp_names <- function(x, ...){
  writeLines("SNP names data contains:")
  for(i in sequence(length(x))){
    writeLines(paste("     ", names(x)[i], length(x[[i]])))
  }
}


print.snps_in_gene_regions <- function(x, ...){
  writeLines(paste("This is a matrix of snps in", length(unique(x[,2])), "gene regions."))
  for(g in unique(x[,2])){
    writeLines(paste("     -- ", g, "contains", length(which(x[,2] == g))))
  }
}


print.snp_location_info <- function(x, ...){
  writeLines(paste("This is a matrix of", dim(x)[1], "snps", "and all their locality information"))
  writeLines(paste("Includes data from:"))
  writeLines(paste0("     -- ", length(unique(x[,1])), " data source {", paste(unique(x[,1]), collapse=", "), "}"))
  writeLines(paste0("     -- ", "from {", paste(unique(x[,2]), collapse=", "), "}"))
  writeLines(paste0("     -- ", length(unique(x[,4])), " chromosomes {", paste(unique(x[,4]), collapse=", "), "}"))
}


Use_snp_finder.py <- function(gene, upstream=0, downstream=0, snp_list="gigs", buildver="hg19", report.call=FALSE, chatty=TRUE){
#this passes a gene to Chucks snp_finder py script
# snp_list can be gigs, exome_pooled_20130624, hapmap_imputed_20120208, or combined_exome_1kgp
#buildver can be hg18 or hg19 (use hg18 for hapmap imputed)
#This won't work local until I add something to my machine to deal with databases
  allsnps <- NULL
  for(g in gene){
    if(chatty)
      print(paste("working on", g))
    path <- MakePathtoPeters_U("/PetersGrp/GECCO_Software/")
    com <- paste0(path, "bin/snp_finder.py --db ", path, "snp.db --gene ", g, " -u ", upstream, " -d ", downstream, " -b ", buildver, " --snp_list ", snp_list)
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
      warning(paste(g, "not found in database"))
  }
  allsnps[,1] <- sapply(allsnps[,1], sub, pattern="chr", replacement="", USE.NAMES=FALSE)
  colnames(allsnps) <- c("position", "gene")
  class(allsnps) <- "snps_in_gene_regions"
  return(allsnps)
}


GetChromo <- function(ncdfFileName){
  return(strsplit(ncdfFileName, "[_.]")[[1]][length(strsplit(ncdfFileName, "[_.]")[[1]]) - 1])
}
# GetChromo("101ccfr_usc2_merged_dosage_5.nc")


GetStudy <- function(ncdfFileName){
  return(strsplit(ncdfFileName, "[_.]")[[1]][1])
}


GetBatch <- function(ncdfFileName){
  return(strsplit(ncdfFileName, "[_.]")[[1]][2])
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


FindSNPposition_hapmap <- function(snp_name, directory, studies=NULL, chatty=TRUE){
# for a vector of rs_numbers 
# for a vector of c(studies)
# create rs_positions table (and also rbind these to save)
# and then rbind results
  res <- NULL
  for(i in studies){
    if(chatty)
      print(paste("Working on", i))
    snps_names_file <- system(paste0("ls ", directory, "*", i, "*"), intern=TRUE)
    for(j in snps_names_file){
      nafile <- strsplit(j, "/", fixed=TRUE)[[1]][length(strsplit(j, "/", fixed=TRUE)[[1]])]
      load(j)
      if("snp_names" %in% ls()){
        rs_positions <- find_rs_hapmap(rs_numbers, snp_names)
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


CreateDosageDataAcrossStudies <- function(rs_positions, hapmap.data.dir=hapmap.data, saveToRdata=NULL){
  # for a vector of c(rs_numbers)
  # for a vector of c(studies)
  #if saveToData is a filename then it will save to working dir
  nostudies <- unique(paste(rs_positions[,1], rs_positions[,2], sep="_"))
  ddall <- data.frame(matrix(nrow=0, ncol=length(unique(rs_positions[,3]))+3))
  colnames(ddall) <- c("study", "batch", "netcdf_ID", unique(rs_positions[,3]))
  for(i in nostudies){
    dir <- paste0(MakePathtoPeters_U(hapmap.data), GetStudy(i), "/", paste(GetStudy(i), GetBatch(i), sep="_"), "/mach/")
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
  p <- paste0(MakePathtoPeters_U(directory), "legend-augmented.nc")
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


CreateSNPDetailsTable <- function(rs_numbers, studies, directory){
  # hapmap
  # for a given list of snps, we want to create a table to be released with data
  # should include: snp name, which chromosome it is on, the number of studies genotyped, the number of studies imputed, the count allele, the baseline allele, the mean R2, the R2Range, the mean CAF, and the CAF range
  # be in barb_working/HapMap_data
  # load(paste0(MakePathtoPeters_U(directory), "gecco_version3_SNPinfo_23studies.Rdata"))
  dataNeeded <- c("snpsall", "Imputed.matrix", "R2.matrix", "CAF.matrix")
  if(!all(dataNeeded %in% ls())){
    whichNotLoaded <- which(!dataNeeded %in% ls())
    for(i in sequence(length(whichNotLoaded))){
      load(paste0(MakePathtoPeters_U(directory), dataNeeded[i], ".Rdata"))
    }
  }   
  nc <- nc_open(paste0(MakePathtoPeters_U(directory), "legend-augmented.nc"))
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
  alleles <- GetCountAndBaselineAlleles(rs_numbers, directory)
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
# CreateSNPDetailsTable(rs, studies, directory="GECCO_Working/barb_working/HapMap_data/")




##  ---------------------------------  ##
##                                     ##
##           GIGSv2 Functions          ##
##                                     ##
##  ---------------------------------  ##


find_rs_to_gigs <- function(rs_number, chatty=TRUE){
# use rs_to_gigs_positions to match known rs numbers with positions in gigs
  rs_to_gigs_file <- MakePathtoPeters_U("/GECCO_Working/barb_working/rs_names_to_gigs_positions.csv")
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


Find_position_gigs_single_chromo <- function(position, chromosome, directory){
# position can be a vector of positions in the format 1:234
  locs <- matrix(nrow=length(position), ncol=5)
  colnames(locs) <- c("study", "gene", "gigs_name", "ncdf file", "position")
  file <- paste0("gigs_v2_chr", chromosome, ".nc")
  nc <- nc_open(paste0(MakePathtoPeters_U(directory), file))
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


FindSNPpositions_gigs <- function(snp_name, directory="/GECCO_DATA_POOLED/GIGS_V2/", chatty=TRUE){
# snp_name can either be a vector of positions (ie, 1:234) or in the class "snps_in_gene_regions"
  res <- NULL
  if(class(snp_name) == "snps_in_gene_regions")
    snps <- snp_name[,1]
  else
    snps <- snp_name
  splitpos <- matrix(unlist(sapply(snps, strsplit, split=":")), ncol=2, byrow=TRUE)
  splitpos <- cbind(snps, splitpos)
  chromosomes <- unique(splitpos[,2])
  for(chr in chromosomes){
    if(chatty) 
      print(paste("working on chromosome", chr))
    
    positions <- splitpos[which(splitpos[,2] == chr), 1]
    res2 <- Find_position_gigs_single_chromo(positions, chr, directory)
    res <- rbind(res, res2)
  }
  res <- res[match(snps, res[,3]),]  #return in original order
  if(class(snp_name) == "snps_in_gene_regions")
    res[,2] <- snp_name[,2]
  class(res) <- "snp_location_info"
  return(res)
}
# gigs_positions <- FindSNPpositions_gigs(snp_name)


CreateDosageDataFromGigs <- function(gigs_positions, directory="/GECCO_DATA_POOLED/GIGS_V2/", chatty=TRUE){
  nochromos <- unique(gigs_positions[,4])
  ddall <- data.frame(matrix(nrow=0, ncol=length(unique(gigs_positions[,3]))+3))
  colnames(ddall) <- c("study", "batch", "netcdf_ID", unique(gigs_positions[,3]))
  probs <- NULL
  probsnamevector <- NULL
  for(i in nochromos){
    sub_gigs_positions <- gigs_positions[which(gigs_positions[,4] == i),]  
    if(class(sub_gigs_positions) == "character"){
      sub_gigs_positions <- matrix(sub_gigs_positions, nrow=1)
    }
    nc <- nc_open(paste0(MakePathtoPeters_U(directory), i))
    samples <- ncvar_get(nc, "Sample_ID", start=c(1, 1), count=c(-1,-1))
    study <- rep(GetStudy(i), length(samples))
    batch <- rep(GetBatch(i), length(samples))
    ind <- 0
    for(snpi in sub_gigs_positions[,5]){
      ind <- ind + 1
      lo <- as.numeric(sub_gigs_positions[,5][ind])
      if(ncvar_get(nc, "SNP_Name", start=c(1, lo), count=c(-1,1)) == sub_gigs_positions[ind,3]){  #here to check that rs number matches correctly
        #dosage <- ncvar_get(nc, "Dosage", start=c(lo, 1), count=c(1,-1))
        dosage <- 2*ncvar_get(nc, "Prob_AA", start=c(lo,1), count=c(1, -1)) + ncvar_get(nc, "Prob_AB", start=c(lo,1), count=c(1,-1))
        probs <- cbind(probs, dosage)
        probsnamevector <- c(probsnamevector, paste0(sub_gigs_positions[ind,2], "_", sub_gigs_positions[ind,3]))
      }
      res <- cbind(study, batch, samples, probs)
      colnames(res) <- c("study", "batch", "netcdf_ID", probsnamevector)
    }
  }
  return(res)
}


MakeSNPDetailsTable_GIGS <- function(snps, chatty=TRUE){
  if(class(snps) != "snp_location_info")
    snps <- FindSNPpositions_gigs(snps)
  snps <- cbind(snps, sapply(snps[,4], GetChromo, USE.NAMES=FALSE))
  res <- matrix(nrow=0, ncol=7)
  files <- MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V2/snp_files/*.csv")
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
      studyname <- strsplit(i, "/", fixed=TRUE)[[1]][length(strsplit(i, "/", fixed=TRUE)[[1]])]
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
  load(paste0(MakePathtoPeters_U(pathToEpiVariables), "listOfEpiVariables.Rdata"))
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
  load(paste0(MakePathtoPeters_U(pathToEpiVariables), "listOfEpiVariables.Rdata"))
  if(includeSurvival)
      epivars <- c(epivars, survvars)
  epi1 <- MakePathtoPeters_U("/Data\\ Harmonization/Post-harmonization/Data/", server="cs")
  epifiles <- WhichEpiFilesToInclude(system(paste0("ls ", epi1, "*.csv"), intern=TRUE), studies)
  m <- matrix(nrow=length(epivars), ncol=length(epifiles$files))
  rownames(m) <- epivars
  colnames(m) <- epifiles$sst
  for(j in sequence(length(epifiles$files))){
    tmp <- read.csv(epifiles$files[j])
    m[,j] <- rownames(m) %in% colnames(tmp)
  }
  if(includeSurvival){
    if("103dals" %in% studies){  ##  same as dals1 / dals2?  
      tmp <- read.csv(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/DALS-surv-dat-04152014.csv"))
      m <- cbind(m, rownames(m) %in% colnames(tmp))
      colnames(m)[length(colnames(m))] <- "103dals"
    }
    if("CPS2" %in% studies){
      tmp <- read.csv(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/CPS2-surv-dat-04152014.csv"))
      m <- cbind(m, rownames(m) %in% colnames(tmp))
      colnames(m)[length(colnames(m))] <- "CPS2"
    }
    survdir <- MakePathtoPeters_U("/GECCO_Working/mpassareworking/Survival/Combined\ Survival\ Update\ Has\ Surv\ Pooled.csv")
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
  epi1 <- MakePathtoPeters_U("/Data\\ Harmonization/Post-harmonization/Data/", server="cs")
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


CreateEpiDataset <- function(variables, studies, files="1or2", chatty=TRUE){
# then each row will be compassID, netcdfID, study, data
  res <- matrix(nrow=0, ncol=length(variables)+4)
  for(i in studies){
    res <- rbind(res, CreateEpiDatasetPerStudy(variables, i, files, chatty))
  }
  return(res)
}

CreateSurvivalDataset <- function(variables="all", studies="all"){
# make dataset with 
  if(studies == "all")
    studies <- c("101ccfr","109colo23", "112mec", "114phs", "115vital", "108dachs", "103dals", "111fccs", "110hpfs", "113nhs", "102arctic", "117hrtccr", "104plco", "105whi")
  if(variables == "all")
    variables <- c("age_dx", "anydeath", "crcdeath", "time_surv", "stage_update", "sex")
  survdir <- MakePathtoPeters_U("/GECCO_Working/mpassareworking/Survival/Combined\ Survival\ Update\ Has\ Surv\ Pooled.csv")
  tmp <- read.csv(survdir, stringsAsFactors=FALSE)
  variables <- variables[which(variables %in% colnames(tmp))]
  if("103dals" %in% studies){
    tmp2 <- read.csv(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/DALS-surv-dat-04152014.csv"), stringsAsFactors=FALSE)
    colnames(tmp2) <- colnames(tmp)
    tmp <- rbind(tmp, tmp2)
  }
  if("CPS2" %in% studies){
    tmp2 <- read.csv(MakePathtoPeters_U("GECCO_Working/Jihyounworking/Survival/DALS-CPS-II/CPS2-surv-dat-04152014.csv"), stringsAsFactors=FALSE)
    colnames(tmp2) <- colnames(tmp)
    tmp <- rbind(tmp, tmp2)
  }
  tmp <- tmp[,which(colnames(tmp) %in% c("compassid", "netcdfid", "study", variables))]  #remove when not all variables
  tmp[,1] <- sapply(tmp[,1], tolower)
  tmp[,1] <- sapply(tmp[,1], GiveStudiesNumbers) 
  return(tmp)
}

MergeEpiAndSurvivalData <- function(EpiDataset, SurvivalDataset, merge_by="netcdfid"){
  mergedSet <- merge(EpiDataset, SurvivalDataset,by=merge_by, all=TRUE)  #compare compassID
  if(!all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
    return("stop, merge didn't work right")
#  if(all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
#   mergedSet <- mergedSet[,-which(colnames(mergedSet) == "compassid.y")]
#  mergedSet <- mergedSet[,-which(colnames(mergedSet) == "sex.y")]
  return(mergedSet)
}



MakeEpiDetailsTable <- function(variables) {
  load(paste0(MakePathtoPeters_U("GECCO_Working/barb_working/"), "listOfVariables.Rdata"))
  dets <- listOfVariables[which(listOfVariables[,2] %in% variables),]
  dets <- dets[match(variables, dets[,2]),]
  if(any(!variables %in% listOfVariables[,2]))
    warning(paste(variables[which(!variables %in% listOfVariables[,2])], "does not seem to be in the file"))
  dets <- dets[c(3,2,5)]
  dets[,1] <- sapply(dets[,1], gsub, pattern=",", replacement=";")
  dets[,3] <- sapply(dets[,3], gsub, pattern=",", replacement=";")
  return(dets)
}








## New function to merge survival/epi data with GWAS data?












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
  load(MakePathtoPeters_U("/GECCO_DATA_POOLED/GIGS_V1/sample_files/gigs_sample.Rdata"))
  if(any(env %in% colnames(gigs_sample)))
    env <- env[-which(env %in% colnames(gigs_sample))]
  if(any(c("censor", "crcdeath", "time_surv", "outc") %in% env)){
    env <- env[-which(env %in% c("censor", "crcdeath", "time_surv", "outc"))]
    warning("env includes survival, which will have to be done separately and merged")
  }
  Gpath <- MakePathtoPeters_U('/GECCO_DATA_POOLED/GIGS_V2/')
  nc <- nc_open(paste(Gpath, 'gigs_v2_chr22.nc', sep=''))
  SampleID <- ncvar_get( nc, 'Sample_ID')
  Study <- ncvar_get( nc, 'Study')
  gigs2 <- data.frame(SampleID,Study,stringsAsFactors=F)
  
  gigs2 <- merge(gigs_sample,gigs2,by.x='netcdfid',by.y='SampleID')

  Epath <- MakePathtoPeters_U('/Data Harmonization/Post-harmonization/Data/', "cs")

  sample0 <- data.frame(study = c(101:104,105,108:115,117),
		      lab = c('101ccfr0',"102arctic0",'103dals0','104plco0','105whi0',
			      '108dachs0',"109colo230",'110hpfs0','111fccs0',"112mec0",
			      '113nhs0',"114phs0",'115vital0','117hrtccr0'),stringsAsFactors=F)
  epidata = NULL

  #== Epi variables included 
  # env = c('famhx1','famhx_reln1','ibd','study_site','sex','asp_ref','aspirin','cancer_site_sum1',
  #        'cancer_site_sum2','age_dx','BMI5','age_dxsel','stage','stage2','stage3')
        
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
  load(MakePathtoPeters_U('GECCO_Working/keithworking/t275-work/pca.Rdata'))
  pcs <- data.frame(Sample_ID,PC[,1:3],stringsAsFactors=F)
  colnames(pcs) <- c('netcdfid','pc1','pc2','pc3')
  epidata <- merge(epidata,pcs,by='netcdfid')
  epidata$drop <- 0
  
  return(epidata)
  #save(epidata,file=paste0(Epath,'GIGS2_EpiData.Rdata'))
}












