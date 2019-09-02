## Install required packages =============================================================

#install cran packages
packages = c("tictoc","assertthat","psych","collections","MASS","NlcOptim","mltools","data.table","devtools","grf","gridExtra","testthat")
packagecheck <- match( packages, utils::installed.packages()[,1] )

packagestoinstall <- packages[ is.na( packagecheck ) ]

if( length( packagestoinstall ) > 0L ) {
  utils::install.packages( packagestoinstall,
                           repos = "http://cran.us.r-project.org"
  )
} else {
  print( "All required cran packages already installed" )
}
#finish installing cran packages

#install packages from github
packages_from_github_names<-c("rlearner")
packages_from_github_repos<-c("xnie/rlearner")

packagecheck <- match( packages_from_github_names, utils::installed.packages()[,1] )

packagestoinstall <- packages_from_github_repos[ is.na( packagecheck ) ]

if( length( packagestoinstall ) > 0L ) {
  library(devtools)
  
  for(p in packagestoinstall){
    install_github(p)
  }
  
} else {
  print( "All required github packages already installed" )
}
#finish installing packages from github

## Finish installing required packages =======================================================