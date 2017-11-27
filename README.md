# OC L2 netCDF Raster Extract

This product allows the quick and streamlined processing of NASA L2 Ocean Color data into standard raster types and projects. It can work with single or many files and allows simple visualization of the data. Metadata may also be extracted and files can are customizably named and saved. This project was created for  the purpose of processing many years of netCDF raster data for the general region of Patagonia. It sets up rasters so that they may later be easily retrievable and processed into higher level data products. This code could be adapted to work with other types of spatial netCDF files. Thanks to the rddj-template (https://github.com/grssnbchr/rddj-template) it is also completely reproducible and deployable with minimal modification. 

## Getting Started

Clone and reset git repository.

git clone https://github.com/mycid/rddj-template-TE-Raster-Extract.git
cd rddj-template-TE-Raster-Extract
rm -rf .git
git init

Alternatively, download them repository from github.com and launch the markdown file

### Prerequisites

You will need R, R Studio, the package checkpoint and markdown 1.6. To deploy with github you will need git downloaded on your computer as well. 

### What you get

Input, and output folders for data are provided and filled with dataproducts. Examine the input files to see how they may differ from yours and precede to the output products to see how they might differ from your desired result. Open the main.html file and read through the process. 

### Running the RMD

The main.Rmd file will run and build an html file open knitting. No input is needed. To use the Rmd file as a tool simply add your own data to the input folder and replace function variables to match your desired output. 

### Using the scripts

The scripts provide fully ready to use functions but require a number of spatial data packages that will need to be download first. The main.Rmd file automatically does this, but if you would like to use just the script functions open the script to see what is required. Use the main.Rmd file as an example for using the script functions.  

## Aknowledgements 

Special thanks to Timo Grossenbacher (https://github.com/grssnbchr) for building the RMD template and to the Huinay Foundation for providing the project and assistance. http://www.huinay.cl/site/sp/

## Author

* **Trevor Eakes**
