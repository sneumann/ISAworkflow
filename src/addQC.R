#
# Create INPUT for YetiBot from Study Sample File 
#

yetiIn <- read.delim("s_E261.txt")
write.table(yetiIn[,"Sample.Name"],
          file="myYetiInput.csv",
          col.names=c("SampleID"), quote=FALSE, row.names=FALSE)

#
# Read YetiBot Output
#

yetiOut <- read.delim("Output_20101115_210733.csv", skip=1)

# Check for end of "regular" output and prune failures Rack
failuresBeyond <- grep("Failures Rack", yetiOut[,1])
yetiOut <- yetiOut[1:(failuresBeyond-1), ]


###################################
#
# Create Input for the Bruker MS
#


d <- data.frame(matrix(nrow=nrow(yetiOut), ncol=61))

colnames(d) <- c("ACQEND_ARGUMENT", "ACQEND_EXECUTE",
 "ACQSTART_ARGUMENT", "ACQSTART_EXECUTE", "AcquisitionID","Amount",
 "AutosamplerMethod","BioTools", "CalibAction", "CalibLevel",
 "CheckToRun", "Comment", "Company", "DataAnalysis", "DataPath",
 "Description", "Dilution", "EMail", "ExperimentID", "Fax",
 "FractionTreatment", "GelCode", "Injections", "InstumentCalibration",
 "InternalStandard", "IsLastLineInGroup", "LC Method", "LCSystem",
 "LOT", "Laboratory", "Line", "MBToolsDiffNo", "MBToolsDiffPath",
 "MBToolsIonization","MBToolsMethod","MBToolsPredicted",
 "MBToolsStateDoIt", "MBToolsStateDone", "MBToolsStateRef",
 "MS Acquisition", "MS_Method","MethodSet", "NumOnGel",
 "NumberInSequence", "Operator", "Phone","PlateCode","Preparation",
 "Preruntime", "ReferenceSampleNum", "Result Path", "RunDAScript",
 "SaltConcentration","SampleID", "SampleType", "Status",
 "SuperMethod", "Switchos", "Vial", "Volume", "Weight")

#
# Dummy values for Bruker
#

d[, c("ACQEND_ARGUMENT", "ACQEND_EXECUTE", "ACQSTART_ARGUMENT", "ACQSTART_EXECUTE",
      "BioTools", "CalibAction", "CalibLevel", "Comment", "Company",
      "Description", "EMail", "ExperimentID", "Fax", "FractionTreatment",
      "GelCode", "LCSystem", "LOT", "Laboratory", "MBToolsDiffPath",
      "MBToolsIonization","MBToolsMethod","MBToolsPredicted", "MethodSet",
      "Phone","PlateCode","Preparation", "Result Path", "SuperMethod",
      "Switchos")] <- ""

d[, c("AcquisitionID", "Amount",
      "InstumentCalibration", "InternalStandard", "IsLastLineInGroup",
      "MBToolsDiffNo",  "MBToolsStateDoIt", "MBToolsStateDone", "MBToolsStateRef",
      "NumOnGel", "Preruntime", "ReferenceSampleNum", "SaltConcentration",
      "SampleType", "Status")]  <- 0

d[, c("Dilution", "Injections", "RunDAScript")]  <- 1

d[, c("Volume")]  <- 2

d$AutosamplerMethod <- "Standard"

d$DataAnalysis <- "D:\\Stephan\\DA-Methoden\\MM8+Date-15min-(pos)forall.m"

d$DataPath <- "stephan\\e261"

d$"LC Method" <- "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m"
d$"MS Acquisition" <- "D:\\Stephan\\MS-Methoden\\20minKalibThresh20posCap5kV.m"
d$"MS_Method" <- "D:\\Stephan\\MS-Methoden\\20minKalibThresh20posCap5kV.m"
d$Operator <- "Bruker Customer"

rackVialMapping <- function(x) {x}

# Values from Yeti Output
d$Line <- 1:nrow(yetiOut)
d$NumberInSequence <- 1:nrow(yetiOut)

d$Weight <- yetiOut$"Sample.Weight..g."
d$CheckToRun <- ifelse(is.na(yetiOut$"Sample.Weight..g."), 0, 1)
d$CheckToRun <- ifelse(yetiOut$Alert=="ALERT", 0, 1)
d$Vial <- rackVialMapping(yetiOut$"Rack.Position")
d$SampleID <- yetiOut$Sample.ID


##
## Randomize Input
##

o <- sample(1:nrow(d))
d <- d[o,]

##
## add QC sample
##

nth=4
samplenr=1

while (samplenr <= nrow(d)) {  
  if ( (samplenr-1) %% nth == 0) {
    cat("QC\n")
  } 

  cat(samplenr, "\n")
  samplenr <- samplenr+1
}
cat("QC\n")



write.csv(d, file="myBrukerInput.csv",
          quote=FALSE, row.names=FALSE)



#
# so far ...
#----------------------------------------




assayFile <- read.delim("../faah_archive/a_metabolite.txt")
assayFile[1:11,"Raw.Spectral.Data.File"]

#
# and mangle into an assay file
#

#----------------------------------------
#
# Add QC and Cleaning Sample
#
nth=4

for (i in 1:nrow(yetiOut)) {
  if (i %% nth == 0) {
    cat("QC\n")
  } else{
    cat(i, "\n")
  }
}


#----------------------------------------
#
# Extract Weight from Yeti Out 
# unoconv -f csv 201109b.xls
# create FileName Column from Bruker Output
# 

brukerIn <- read.csv("201109b.csv")
brukerIn[,"SampleID"]
brukerIn[,"Weight"]

#----------------------------------------
#
# Extract result file from Bruker Output
# unoconv -f csv 171008.xls 
#
brukerOut <- NULL
brukerOut <- read.csv("171008.csv", stringsAsFactors=FALSE)

## Please ignore double (and quadruple) backslashes here:
brukerOut[1:4,c("Status", "Result.Path")]

