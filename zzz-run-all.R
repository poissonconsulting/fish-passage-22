if(FALSE) {
  source("header.R")
  source("install-packages.R")
  source("update-packages.R")
  
  source("archive-main.R")
  source("remove-main.R")
}

source("read-flow-diagrams.R")
source("read-dags.R")

# Download data? 
download <- FALSE  # This uses previously downloaded versions of data in Dropbox
# download <- TRUE   # This programatically downloads data
if (download) {
  # Downloads stream temperature data
  source("download-water-temp.R")
  # Downloads discharge data from PCIC to dropbox Data folder
  source("download-discharge-pcic.R")
  # Downloads air temperature and net solar radiation from ERA5 dataset (requires API key)
  # follow directions from https://github.com/joaohenry23/Download_ERA5_with_python
  source("download-air-temp.R")
  source("download-solar-rad.R")
}

source("read-water-temp.R")
source("clean-water-temp.R")
source("tidy-water-temp.R")

source("read-air-temp.R")
source("clean-air-temp.R")
source("tidy-air-temp.R")

source("read-discharge.R")
source("clean-discharge.R")
source("tidy-discharge.R")

source("read-solar-rad.R")
source("clean-solar-rad.R")
source("tidy-solar-rad.R")

source("create-database.R")
source("query-database.R")

source("plot-water-temp.R")
source("plot-air-temp.R")
source("plot-discharge.R")

source("SSN-distances-temp.R")

source("prepare-data.R")

source("manipulate-temperature.R")
source("analyse-temperature.R")
source("predict-temperature.R")
source("sensitivity-temperature.R")
source("predict-gsdd.R")

if(FALSE) {
  source("knit-report.R")
  source("session-info.R")
  source("archive-database.R")
}
