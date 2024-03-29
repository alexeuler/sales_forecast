MODEL = "bats" #bats, HoltWinters, stlm
FILENAME="Shop Figures.xlsx"
SHEETNAME="Takings"


use = function(lib_name) {
  if (!is.element(lib_name, installed.packages()[,1]))
    install.packages(lib_name, dep = TRUE)
  library(lib_name,character.only=TRUE)
}

use("ggplot2")
use("forecast")
use("reshape")
use("gridExtra")
options(java.parameters = "-Xmx4g" )
use("XLConnect")