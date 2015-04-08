##  ---------------------------------  ##
##                                     ##
##       HapMap Search Functions       ##
##                                     ##
##  ---------------------------------  ##


require(ncdf)
require(ncdf4)

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


# study names change
ChangeStudyNames <- function(study){
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



# Some functions for dealing with snp_names class

print.snp_names <- function(x, ...){
  writeLines("SNP names data contains:")
  for(i in sequence(length(x))){
    writeLines(paste("     ", names(x)[i], length(x[[i]])))
  }
}
#snp_names_file

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

MakeRSnum <- function(num){
  num <- strsplit(num, ":", fixed=TRUE)[[1]][2]
  return(paste0("rs", num))
}


find_rs_gigs <- function(rs_number, chatty=TRUE){
  nc_files <- system(paste("ls", MakePathtoPeters_U("GECCO_DATA_POOLED/GIGS_V2/*.nc")), intern=TRUE)
  res <- NULL
  for(i in nc_files){
    if(chatty)
      print(paste("Working on", i))
    tmp <- nc_open(i)
    tmp_names <- ncvar_get(tmp, "SNP_Name", start=c(1,1), count=c(-1,-1))
    tmp_names <- sapply(tmp_names, MakeRSnum)
    ### Totally doesn't work.  No RS numbers in GIGS
    ### Maybe have to use rentrez or rsnps to determine the chromosome and positions from rs numbers
    ### Can use dbsnp and put in rs numbers, but we need a more computational way
    ### Also might help solve the issue of finding snps for a gene
    ### emailed scott chaimberlain (rsnps maintainer) to see if NCBI_snp_query can do this for build 37
    ### Also might consider using rentrez
         # res <- entrez_search(db = "snp", term = "TERF2IP", retmax=100)
         # cv <- entrez_summary(db="snp", id=res$ids)
         # sapply(cv, "[[", "snp_id")

   # if(any(rs_number %in% tmp_names))
  }

}



FindRSAcrossStudies <- function(rs_numbers, studies, directory, chatty=TRUE){
  # for a vector of c(rs_numbers)
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
      # study <- strsplit(nafile, "[_.]")[[1]][3]
      # batch <- strsplit(nafile, "[_.]")[[1]][4]
      load(j)
      if("snp_names" %in% ls()){
        rs_positions <- find_rs_hapmap(rs_numbers, snp_names)
        # rs_positions <- cbind(rep(study, dim(rs_positions)[1]), rep(batch, dim(rs_positions)[1]), rs_positions)
        # colnames(rs_positions)[1:2] <- c("study", "batch")
        res <- rbind(res, rs_positions)
      }
    }
  }
  (return(res))
}
#RSAcross <- FindRSAcrossStudies(rs, studies)
#FindRSAcrossStudies(c("rs11412"), studies, MakePathtoPeters_U(names.root))

# Create new dataset with probs and all samples from each dataset using rs_positions

CreateDosageDataPerStudy <- function(rs_positions, directory=NULL){
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


GetChromo <- function(ncdfFileName){
  return(as.numeric(strsplit(ncdfFileName, "[_.]")[[1]][length(strsplit(ncdfFileName, "[_.]")[[1]]) - 1]))
}
# GetChromo("101ccfr_usc2_merged_dosage_5.nc")

GetStudy <- function(ncdfFileName){
  return(strsplit(ncdfFileName, "[_.]")[[1]][1])
}

GetBatch <- function(ncdfFileName){
  return(strsplit(ncdfFileName, "[_.]")[[1]][2])
}


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

#MakeGenomeDetailsTable <- function(legendFile="legend-augmented.nc", FlorasRdata="gecco_version3_SNPinfo_23studies.Rdata"){
# make sure you are in barb_working/HapMap.data
# idea is to make one giant table that you can then scrape...not sure
#  load(FlorasRdata)
#}
# MakeGenomeDetailsTable("GECCO_Working/GECCO_Data_Working/pca/legend-augmented.nc")



CreateSNPDetailsTable <- function(rs_numbers, studies, directory){
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

#CreateSNPDetailsTable(rs, studies, directory="GECCO_Working/barb_working/HapMap_data/")









##  ---------------------------------  ##
##                                     ##
##      Epi Data Search Functions      ##
##                                     ##
##  ---------------------------------  ##


WhichEpiFilesToInclude <- function(epifiles, studies){
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



CreateEpiDatasetPerStudy <- function(variables, study){
# gather variables for a single study
  if(any(c("censor", "crcdeath", "time_surv") %in% variables))
    variables[-which(variables %in% c("censor", "crcdeath", "time_surv"))]
  epi1 <- MakePathtoPeters_U("/Data\\ Harmonization/Post-harmonization/Data/", server="cs")
  epifiles <- WhichEpiFilesToInclude(system(paste0("ls ", epi1, "*.csv"), intern=TRUE), study)
  m <- matrix(nrow=0, ncol=length(variables)+3)
  colnames(m) <- c("compassID", "netcdfID", "study", variables)
  for(j in epifiles$files){
    tmp <- read.csv(j)
    tmp <- tmp[,which(colnames(tmp) %in% c("compassid", "netcdfid", "gecco_study", variables))]
    m <- rbind(tmp)
  }
  return(m)
}
# CreateEpiDatasetPerStudy(vars, study)


CreateEpiDataset <- function(variables, studies, chatty=TRUE){
# then each row will be compassID, netcdfID, study, data
  res <- matrix(nrow=0, ncol=length(variables)+3)
  for(i in studies){
    res <- rbind(res, CreateEpiDatasetPerStudy(variables, i))
    if(chatty)
      print(paste("working on", i, "; dim =", paste(dim(res), collapse="_")))
  }
  return(res)
}

CreateSurvivalDataset <- function(variables, studies){
# make dataset with 
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

MergeEpiAndSurvivalData <- function(EpiDataset, SurvivalDataset){
  mergedSet <- merge(EpiDataset, SurvivalDataset,by="netcdfid", all=TRUE)  #compare compassID
  if(!all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
    return("stop, merge didn't work right")
  if(all(mergedSet$compassid.x == mergedSet$compassid.y, na.rm=TRUE))
   mergedSet <- mergedSet[,-which(colnames(mergedSet) == "compassid.y")]
#  if(all(mergedSet$sex.x == mergedSet$sex.y, na.rm=TRUE))
  mergedSet <- mergedSet[,-which(colnames(mergedSet) == "sex.y")]
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








## New function to merge survival/epi data with GWAS data












##  ---------------------------------  ##
##                                     ##
##     RS Number Search Functions      ##
##                                     ##
##  ---------------------------------  ##

# To find rs numbers in GIGS, you need to know their respective locations
# These were done for GIGSv1, but not for GIGSv2
# Keith made the original Rdata files and I pulled them into R (took forever) and then created a two column csv that could then be grepped in system rather than having to load it all in R

# load("/shared/silo_researcher/Peters_U/GECCO_Working/keithworking/t249-work/WGS-rs-mapping.Rdata")
# whichSNPsHaveNames <- which(dbSNP_rs_name != "NA")
# write.table(matrix(c("rs_name", "position"), nrow=1), file="rs_names_to_gigs_positions.csv", col.names=FALSE, row.names=FALSE, sep=",")
#for(i in whichSNPsHaveNames){
#  write.table(matrix(c(dbSNP_rs_name[i], WGS_SNP_name[i]), nrow=1), file="rs_names_to_gigs_positions.csv", col.names=FALSE, row.names=FALSE, sep=",", append=TRUE)
#}

# file <- "/Volumes/bbanbury/Peters_U/GECCO_Working/barb_working/rs_names_to_gigs_positions.csv"
# system(paste("grep rs144022023", file), intern=TRUE)
























