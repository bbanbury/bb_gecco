% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/SearchFunctions.R
\name{CreateSNPDetailsTable}
\alias{CreateSNPDetailsTable}
\title{CreateSNPDetailsTable}
\usage{
CreateSNPDetailsTable(rs_numbers, studies, chatty = TRUE)
}
\arguments{
\item{rs_numbers}{A vector of rs numbers}

\item{studies}{A vector of studies to be included in the count}

\item{chatty}{Option to print progress to screen}
}
\description{
Create a SNP Table with details on hapmap position and alleles
}
\details{
For a vector of snps, this function will pull all of the information we want to release with hapmap data.
This will create a table to use in the data release. Results will include: snp name (rs number), chromosome,
position, the number of studies genotypes, the number of studies imputed, the count allele, the baseline allele,
the mean R2, and the R2 range (min-max), the mean CAF, and the CAF range (min-max).
}
\examples{
studies <- c("101ccfr", "102arctic", "103dals")
CreateSNPDetailsTable(c("rs2736100", "rs401681", "rs10069690"), studies)
}
\seealso{
\link{GetCountAndBaselineAlleles}
}

