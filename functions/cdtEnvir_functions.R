
##define new environment
# Data Extraction
EnvExtractData <- new.env()

#Interpolation delete data
EnvInterpolation <- new.env()

#QC zeros check  daily rainfall data
EnvQcZeroChkData <- new.env()

#QC outliers rainfall data
EnvQcOutlierData <- new.env()

#Homogenization
EnvHomogzData <- new.env()

#Hold-Out Validation
EnvHOValidation <- new.env()
EnvHOValidationplot <- new.env()

#Leave-one-out cross-validation
EnvLOOCValidation <- new.env()
EnvLOOCValidationplot <- new.env()

#Spatial analysis
EnvSpatialAnalysis <- new.env()
EnvSpatialAnalysisplot <- new.env()

# Zoom parameters and bbox
EnvZoomPars <- new.env()

# PICSA
EnvPICSA <- new.env()
EnvPICSAplot <- new.env()

# Compute climatologies
EnvClimatoCalcPlot <- new.env()

# Compute anomalies
EnvAnomalyCalcPlot <- new.env()

# Onset, Cessation, Length
EnvOnsetCalcPlot <- new.env()
EnvCessationCalcPlot <- new.env()
EnvSeasLengthCalcPlot <- new.env()

# daily rainfall analysis
EnvDailyRainAnalysisplot <- new.env()

# Summary data
EnvSummaryDataplot <- new.env()

# SPI
EnvSPICalcPlot <- new.env()

# plot CDT data
EnvCDTdataPlot <- new.env()

# plot sequential netcdf
EnvSeqNCDFPlot <- new.env()

# plot one netcdf
EnvOneNCDFPlot <- new.env()


