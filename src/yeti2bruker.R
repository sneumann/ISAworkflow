library(XML)

########################################
##
## write sample table as XML file
##

convertToXML <- function(df)
{
  xml <- xmlTree()

  xml$addNode("SampleTableHeader",
              attrs=c(Version="2", CreationDate="11/26/10",
              CreationTime="10:49:50", Creator="Bruker Customer",
              CreationComputer="QTOF-PC", UpdateDate="11/26/10",
              UpdateTime="10:49:50", UpdateUser="Bruker Customer",
              UpdateComputer="QTOF-PC", Author="Bruker Customer",
              Company="Bruker Customer",
              Laboratory="unknown",
              Phone ="",
              Fax="",
              EMail="",
              Description="",
              NumOfSamples=nrow(df),
              HyStarVersion="3.2.44.0"))

  xml$addNode("SampleTable", close=FALSE)
  for (i in 1:nrow(df)) {
      xml$addNode("Sample",
                  attrs=as.list(df[i,]))
  }
  xml$closeTag()
  return(xml)
}


##
## The yetiOutput is a 8*12=96 rack,
## the UPLC racks are 2*6*8=96
##
## r <- rackVialMapping (c("A1", "A7", "A8", "A9", "A12",
##                         "B1", "B12", "C1", "C12", "D12", "E1",
##                          "H1", "H12"))

rackVialMapping <- function(x) {
    if (length(x) > 94) {
        warning("LC/MS OVERFLOW !! There are ", length(x), "input vials")
    }

    incolumn <- sapply(substring(x, 1, 1), function(x) {as.integer(charToRaw(x))} ) - as.integer(charToRaw("A")) +1
    inrow    <- as.integer(substring(x, 2))
    inposition <- (incolumn-1)*12 + inrow

    outrack <- (inposition-1) %/% 48 +1
    outposition <- (inposition-1) %% 48 +1
    outrow <- (inposition-1) %% 8 +1

    outcolumn <- sapply((outposition-1) %/% 8 +1, function(x) {rawToChar(as.raw(64+x))} )

    paste(outrack, ":", outcolumn, ",", outrow, sep="")
}

writeMapping <- function(infile) {
    yetiOut <- read.delim("Output_20101125_152753.csv", skip=1, as.is=TRUE)

    ## Check for end of "regular" output and prune failures Rack
    failuresBeyond <- grep("Failures Rack", yetiOut[,1])
    successes <- yetiOut[1:(failuresBeyond-3), ]

    failures <- yetiOut[(failuresBeyond-2):nrow(yetiOut), ]

    brukerPosition <- rackVialMapping(yetiOut$"Rack.Position")

    write.table(cbind(yetiOut$"Rack.Position"), brukerPosition)
}
writeMapping <- readYetiOutput("Output_20101125_152753.csv",

readYetiOutput <- function(infile, randomize=TRUE, polarity="pos", SuperMethod, MSMethod, MSProcessingMethod, AutosamplerMethod) {
    ##
    ## Read YetiBot Output
    ##

    yetiOut <- read.delim("Output_20101125_152753.csv", skip=1, as.is=TRUE)

    ## Check for end of "regular" output and prune failures Rack
    failuresBeyond <- grep("Failures Rack", yetiOut[,1])
    yetiOut <- yetiOut[1:(failuresBeyond-3), ]

    ###################################
    ##
    ## Create Input for the Bruker MS
    ##

    x <- data.frame(matrix(nrow=nrow(yetiOut), ncol=30))
    names(x) <- c("Line", "CheckToRun", "Status", "Injections", "Volume", "Amount", "Dilution", "Weight",
                  "Position", "InternalStandard", "PreRunTime", "SampleID", "DataPath", "Operator",
                  "SampleType", "ReferenceSampleNum", "NumOnGel", "ResultDatafile",
                  "NumberInSequence", "CalibAction", "CalibLevel", "AcquisitionID", "SaltConc",
                  "LastLineInGroup", "InstrumentCalibration", "RunDAScript", "Method",
                  "MS_Method", "MSProcessingMethod", "AutosamplerMethod")

    x[, c("Status", "Amount", "InternalStandard", "PreRunTime", "SampleType",
          "ReferenceSampleNum", "NumOnGel", "AcquisitionID", "SaltConc",
          "LastLineInGroup", "InstrumentCalibration")]  <- 0

    x[, c("ResultDatafile", "CalibAction", "CalibLevel")]  <- ""

    x[, c("Dilution", "Injections", "RunDAScript")]  <- 1
    x[, c("Volume")]  <- 2

    x$Operator <- "Bruker Customer"

    ## Values from Function Arguments
    x$Method <- SuperMethod
    x$MS_Method <- MSMethod
    x$MSProcessingMethod <-MSProcessingMethod
    x$AutosamplerMethod <- AutosamplerMethod

    ## Values from Yeti Output
    x$Line <- 1:nrow(yetiOut) ## STN ??????
    x$NumberInSequence <- 1:nrow(yetiOut)

    x$Weight <- yetiOut$"Sample.Weight..g."
    x$CheckToRun <- ifelse(yetiOut$Alert=="ALERT", 0, 1)
    x$Position <- rackVialMapping(yetiOut$"Rack.Position")
    x$SampleID <- paste(yetiOut$Sample.ID, polarity, sep="_")

    ##
    ## Randomize Input
    ##

    if (randomize) {
        o <- sample(1:nrow(x))
    } else {
        o <- (1:nrow(x))
    }

    x[o, , drop=FALSE]
}

addQC <- function(samples, vials="2:F,8", nth=8, polarity="pos", SuperMethod, MSMethod, MSProcessingMethod, AutosamplerMethod) {

    ##
    ## add QC sample
    ##

  newidx <- 1:nrow(samples)
  newidx <- newidx + 2*( (newidx-1) %/% (nth-1) )

  newsamples <- samples[0,]
  newsamples[newidx,] <- samples


  newsamples[seq(nth, nrow(newsamples), by=nth+1), ] <- data.frame(Line=1, CheckToRun=1, Status=0, Injections= 1,
                                        Volume=2, Amount=0, Dilution=1, Weight=0, Position=vials,
                                        InternalStandard=0, PreRunTime=0,
                                        SampleID <- paste("Spuel", polarity, sep="_"), DataPath="",
                                        Operator="Bruker Customer", SampleType=0, ReferenceSampleNum=0,
                                        NumOnGel=0, ResultDatafile="", NumberInSequence=1, CalibAction="",
                                        CalibLevel="", AcquisitionID=0, SaltConc=0, LastLineInGroup=0,
                                        InstrumentCalibration=0, RunDAScript=1, Method=SuperMethod,
                                        MS_Method=MSMethod, MSProcessingMethod=MSProcessingMethod,
                                        AutosamplerMethod=AutosamplerMethod,
                                        stringsAsFactors=FALSE)

  newsamples[seq(nth+1, nrow(newsamples), by=nth+1), ] <- data.frame(Line=1, CheckToRun=1, Status=0, Injections= 1,
                                          Volume=2, Amount=0, Dilution=1, Weight=0, Position="2:F,8",
                                          InternalStandard=0, PreRunTime=0,
                                          SampleID <- paste("MM8", polarity, sep="_"),
                                          DataPath="",
                                          Operator="Bruker Customer", SampleType=0, ReferenceSampleNum=0,
                                          NumOnGel=0, ResultDatafile="", NumberInSequence=1, CalibAction="",
                                          CalibLevel="", AcquisitionID=0, SaltConc=0, LastLineInGroup=0,
                                          InstrumentCalibration=0, RunDAScript=1, Method=SuperMethod,
                                          MS_Method=MSMethod, MSProcessingMethod=MSProcessingMethod,
                                          AutosamplerMethod=AutosamplerMethod,
                                          stringsAsFactors=FALSE)

  newsamples
}

writeBrukerSamples <- function(samples, outfile, DataPath) {
    samples$DataPath <- DataPath
    tr = convertToXML(samples)
    cat(saveXML(tr$value()), file=outfile)

    write.csv(samples, file="myNewBrukerInput.csv",
          quote=TRUE, row.names=FALSE)

}

possamples <- readYetiOutput("Output_20101125_152753.csv",
                          randomize=TRUE, polarity="pos",
                          SuperMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                          MSMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                          MSProcessingMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20posCap5kV.m",
                          AutosamplerMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20posCap5kV.m")

possamples <- addQC(possamples, "2:F,7", nth=10, polarity="pos",
                 SuperMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                 MSMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                 MSProcessingMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20posCap5kV.m",
                 AutosamplerMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20posCap5kV.m")

posspuel <- addQC(possamples[1:2,], "2:F,7", nth=2, polarity="pos",
                  SuperMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                  MSMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                  MSProcessingMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m",
                  AutosamplerMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m")[2:3,]
negspuel <- addQC(possamples[1:2,], "2:F,7", nth=2, polarity="neg",
                  SuperMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                  MSMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                  MSProcessingMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m",
                  AutosamplerMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m")[2:3,]

negsamples <- readYetiOutput("Output_20101125_152753.csv",
                          randomize=TRUE, polarity="neg",
                          SuperMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                          MSMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                          MSProcessingMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m",
                          AutosamplerMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m")

negsamples <- addQC(negsamples, "2:F,7", nth=10, polarity="neg",
                 SuperMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                 MSMethod = "D:\\Stephan\\LC-Methoden\\PDA-LC-20min-150uL.m",
                 MSProcessingMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m",
                 AutosamplerMethod = "D:\\Stephan\\MS-Methoden\\20minKalibThresh20negCap5kV.m")


allsamples <- rbind(posspuel, possamples, negspuel, negsamples, negspuel)

writeBrukerSamples(allsamples, "myNewBrukerInput.xml", DataPath = "stephan\\e261")




