#run Nikki's code

package= 'geobamdata'

#check for installed version and load library, install from git if not there
  if (!require(package, character.only = TRUE)) {
    devtools::install_github("SWOT-Confluence/geobamdata", force=TRUE)
    library(package, character.only = TRUE)
  }

input_dir= 'D:/Box Sync/Lab members/Nikki/test input/'
output_dir='D:/Box Sync/Lab members/Nikki/test output/'

#generate output
process_data(input_dir, output_dir)

#did it work?
dir(output_dir)
