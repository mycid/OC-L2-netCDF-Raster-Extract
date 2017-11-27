#_________________________________________________________________
#CUSTOM FUNCTIONS
#' @title metaXtract
#' @param x is the full file name a netCDF file
#' @param useful Option to specify global variables by name in input list
#' @param band selects the band name which will be appended to the output data frame
#'Default is: "title", "product_name", "platform", "id", "date_created", "spatialResolution", "geospatial_lat_max", "geospatial_lat_min", "geospatial_lon_max", "geospatial_lon_min"
#' @return class dataframe with global attributes as columns
#' @examples x<- "A2015001183000.L2_LAC_OC.x.nc"
#' get(testNASA)
#' require(ncdf4)
#' meta <- data.frame(ncatt_get(testNASA, varid = 0))
#' df <- metaXtract(testNASA, band="chlor_a")
#' df2 <- metaXtract(testNASA, useful="geospatial_lat_max", "geospatial_lat_min", "geospatial_lon_max", "geospatial_lon_min", band="chlor_a") 
#' @author Trevor Eakes, mycid[at].uw.edu
#' @description This function allows streamlined metadata extraction of netCDF MODIS files using the ncdf4 package
#' metaExtract(x)
#' @details Extracts key metadata items from the global metadata of the netCDF file. The vectors extracted default to useful but can be changed if you know the names of the vectors and insert a list into useful instead. file name, and data of interest are added to the end of the dataframe. 
metaXtract <-  # netCDF metadata extraction function
  function(x, useful = NA, band) {
    require(ncdf4)#demand package
    test.nc <- nc_open(x)#open file
    meta <-
      data.frame(ncatt_get(test.nc, varid = 0))#get global attr
    nc_close(test.nc) #close file (necissary)
    if (is.na(useful) == FALSE) {
      useful = useful
    } else{
      useful <- #Create list of useful variables
        c(
          "title",
          "product_name",
          "platform",
          "id",
          "date_created",
          "spatialResolution",
          "geospatial_lat_max",
          "geospatial_lat_min",
          "geospatial_lon_max",
          "geospatial_lon_min"
        )
    }
    meta <- subset(meta, select = useful) #subset
    metas <-
      data.frame(meta, data1 = band, data2 = "flags", file = x) #add some extra important metadata
    return(metas)
  }
  
#' @title NetCDFdateT
#' @author Trevor Eakes, mycid[at]uw.edu
#' @description  Simple function for extracting date and time from netCDF metadata
#' Conveniently extracts the remote sensing date when the pass began for the select file. Selected from the global attributes of the netCDF file
#' @param x charecter name of netCDF (.nc) file
#' @return charecter list with date and time as seperate items
#' @example #' get(testNASA)
#' require(ncdf4)
#' meta <- data.frame(ncatt_get(testNASA, varid = 0))
#' dandt <- NetCDFdateT(testNASA, band="chlor_a")
#' print(dandt)
NetCDFdateT <- function(y) {
  test.nc = nc_open(y) #open file
  date <-
    as.character(ncatt_get(test.nc, varid = 0, attname = "time_coverage_start")) #Get the start time for the swath
  datetime <- str_split(date[2], "T", simplify = TRUE) #Edit string
  nc_close(test.nc) #close file
  return(datetime)
}
#Raster extraction main work horse function
#Designed to be implemented with the OCnetCDF2Raster function
#' Title
#'
#' @param x name of netCDF file to be rasterized. class character
#' @param band " name of band to be extracted from the netCDF file. Must perfectly match the name assigned to the band in the target net CDF file. Does not need to include prefix geophysical_data/ but can include prefixes if other MODIS band groupings are needed. 
#' @param Noflags Logical argument directing the function to extract the flag band. Only use if such a band exists. Band must be named "geophysical_data/l2_flags" if Noflags is TRUE
#' @param output.proj sets the output projection argument. Must be charecter argument compatable with CRS function. epsg # is best
#' @param proj.Ext bounding box extent of the raster. Must be of class extent and in the coordinates of output.proj
#' @param makeplots logical allowing for plots to be made in the plotting window.
#' @param saveplots logical allowing for plots to be saved. Plotting options must be modified within the function
#' @param allows user to specify the title of the plot
#' @param base allows wrapper function or user to input a ggplot object as the base map for plotting the raster over. Extent range of plot object must be contained in the base map. Saves computational time. 
#' @param out allows user to specify the output file folder. out must end with a /. Default is nothing. 
#' @return Returns a raster stack or raster layer file (package raster). plot files will also be saved as JPEG to the working directory
#' @examples 
#' get(testNASA)
#' band <- "chlor_a"
#' output.proj= "+init=epsg:5362"
#' proj.Ext <-extent(250000, xmax = 1200000,ymin = 3700000,ymax = 5500000)
#' ExtractRaster(testNASA, band=band, output.proj, proj.Ext, makeplots=TRUE, saveplots=FALSE, plotname="this is an example")

ExtractRaster <-
  function(y,
           band,
           Noflags = FALSE,
           output.proj,
           proj.Ext = NA,
           makeplots = TRUE,
           saveplots = TRUE,
           plotname = NA,
           base=NA,
           out = "") {
    #Demand packages
    require(raster)
    require(RNetCDF)
    require(ncdf4)
    require(plyr)
    require(rgdal)
    require(sp)
    require(ggmap)
    require(ggplot2)
    require(stringr)
    #Set variables
    if(is.na(str_extract(band, "/"))) { #Detects if band variable is abreviated or includes the prefix geophsical data. Adds prefix if needed
    bnd <-
      paste("geophysical_data/", band, sep = "") #full length band variable name
    }else {
      
      bnd <- band
    }
    #extract flagging raster 
    if (Noflags == TRUE) { #Allows function to ignore flagging raster if so desired
      #Do nothing
      flag <- "nothing"
    } else {
      #extract flagging
      flag <- raster(y,  varname = "geophysical_data/l2_flags")
    }
    #create raster variables
    var <- raster(y, varname = bnd) #extract raster variable of interest with variable object bnd
    #Get lat and lon data
    lat <- 
      raster(y, varname = "navigation_data/latitude") #Get lat and lon data
    lon <- raster(y, varname = "navigation_data/longitude")
    require(data.table) #demand data.table for optimal speed
    #make data tables of all the variables
    var <- data.table(rasterToPoints(var))
    Y <- data.table(rasterToPoints(lat))
    X <- data.table(rasterToPoints(lon))
    #exclude or don't exclude flag raster data
    if (class(flag) == "RasterLayer") {
      Fl <- data.table(rasterToPoints(flag))
      names(Fl) <- c("a", "b", "Flags")
    }
    #Rename columns
    names(var) <- c("a", "b", "Var")
    names(X) <- c("a", "b", "lon")
    names(Y) <- c("a", "b", "lat")
    #merge data, the sequential order of this merging is necissary
    XY <- merge(X, Y, by = c("a", "b"))
    #exclude or include flag in merging
    if (class(flag) == "RasterLayer") {
      XY <- merge(XY, Fl, by = c("a", "b"))
    }
    XYZ = merge(XY, var, by = c("a", "b"))
    XYZ = XYZ[, 3:ncol(XYZ)]
    #Create spatial dataframe
    SpatDat <-
      SpatialPointsDataFrame(XYZ[, 1:2], XYZ, proj4string = CRS("+proj=longlat +datum=WGS84"))
    SpatDat <-
      spTransform(SpatDat, CRS(output.proj)) #Change projection
    #Set project extent and supress warnings
    if (suppressWarnings(is.na(proj.Ext)))
    {#If project extent not provided uses data extent
      #Use defualt extent
      proj.Ext = extent(SpatDat)
    }
    #Convert projection extent into polygon
    e <- as(proj.Ext, "SpatialPolygons")
    #set extent projection
    proj4string(e) <- CRS(output.proj)
    #create an empty raster with the project extent, raster must be the same resolution as data (in this case 1km squares in the UTM projection)
    frame <-
      raster(
        crs = CRS(output.proj),
        ext = extent(e),
        resolution = 1000,
        vals = NULL
      )
    #Rasterize points to the new study extent frame
    field <-
      names(SpatDat[, ncol(SpatDat)]) #get variable column name
    print(y)#
    ChlorA <- #Rasterize the data to the empty raster frame
      rasterize(
        SpatDat,
        y = frame,
        field = field,
        fun = function(x, ...) {
          mean(x, na.rm = TRUE)
        },
        proj4string = CRS(output.proj)
      )
    #Repeat process for flagging band
    if(Noflags==TRUE)
    {return(ChlorA)} else{
    Flags <-
      rasterize(
        SpatDat,
        y = frame,
        field = "Flags",
        proj4string = CRS(output.proj)
      )
    #Stack Rasters
    Chlor <- stack(ChlorA, Flags)
    }
    #Visualization option, if make plots has been selected
    #
    if (isTRUE(makeplots)) {
      require(Hmisc) #demand useful package 
      SU <- spTransform(e, CRS("+init=epsg:4326")) # make Study Area
      #Turn study area into a bounding box
      b <- bbox(SU)
      #Transform spatial dataframe into google maps projection, lat lon
      TestP <-
        spTransform(SpatDat, CRS("+init=epsg:4326"))
      #Remove na data
      TestP <- data.frame(SpatDat[!is.na(SpatDat$Var), ])
      #add quartile ranges to each row for visualization
      TestP$quartile <- with(TestP, cut(TestP$Var, breaks=quantile(TestP$Var, probs=seq(0,1, by=0.1), na.rm=TRUE), include.lowest=TRUE))
      #Hacky fix to create polygon dataframe bounding box with basic bounding box for ggplot2
      bb <-
        data.frame(Lon = c(rep(b[1, 1], 2), rep(b[1, 2], 2), b[1, 1]),
                   Lat = c(b[2, ], b[2, 2], rep(b[2, 1], 2)))
      require(RColorBrewer)
      #Allows a custom base map to be inputed for plotting over so basemap does not have to be drawn each time
      if(is.na(base)) {
      bm <- #Calls basemap from google
        ggmap(suppressWarnings(
          get_map(
            location = b,
            maptype = "hybrid",
            zoom = 5,
            messaging = FALSE
          )
        ))
      } else{
        bm<- base
      }
      #Create color palette for plotting
      myPalette <- rev(brewer.pal(10, "Spectral"))
      #Create color scale from color palette
      sc <- scale_colour_manual(values=myPalette)
      #Make the plot of raster data as points over ggmap
        PLOT <- bm + #adds point data
          geom_point(
            data = TestP,
            aes(x = lon, y = lat,
                colour = factor(quartile)),
            size = 0,
            alpha = 0.5
          ) +sc+ #adds color scheme
          guides(colour = guide_legend(override.aes = list(size=10)))+ #increases legend size 
          geom_path( #Create semitransparent bounding box
            data = bb,
            aes(x = Lon, y = Lat),
            colour = "white",
            alpha = 0.7,
            size = 2
          ) +
          theme_bw() + #basic theme
          ggtitle(plotname) + #add plot title
          guides(fill = guide_legend(title = paste(band, "units"))) + #change legend title
          theme( #set custom theme
            axis.title.x = element_text(size = 20),
            plot.title = element_text("bold", 28),
            axis.title.y = element_text(size = 20, angle = 90),
            axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "right",
            legend.key = element_blank(),
            legend.title = element_text(size = 16)
          )
        #Display plot without warnings
      suppressWarnings(plot(PLOT))
      #Optional save plot as jpg
      if (isTRUE(saveplots)) {
        datet <- NetCDFdateT(y) #Extract date
        suppressWarnings(
          ggsave(
            filename = paste(out, y, "_", band, ".jpg", sep = ""),
            plot = PLOT,
            device = "jpeg",
            height = 35,
            width = 25,
            units = "cm"
          )
        )
      }
    }
    return(Chlor)
  }

#' @title oc.netCDF.2raster
#' @description 
#' @details Troubleshooting: 
#' if error message is:  Error in file(file, ifelse(append, "a", "w")) cannot open the connection, you have the metadata spreadsheet open when trying to run the program. Close the spreadsheet and run again.
#' Make sure you are not editting any of the rasters being overwritten at the time of running the function
#' Ignore ggmaps messages about makeing the bounding box

#' @param dat 
#' @param band 
#' @param filename 
#' @param output.extension 
#' @param output.proj 
#' @param meta.name 
#' @param proj.Ext 
#' @param meta.exp 
#' @param Noflags 
#' @param makeplots 
#' @param saveplots 
#' @param makeMeta 
#' @return
#' @author
#' @examples
oc.netCDF.2raster <-
  function(dat,
           band,
           filename = NA,
           output.extension = ".grd",
           output.proj = "+init=epsg:5362",
           meta.name = "NASA_proj_metaD.csv",
           proj.Ext,
           meta.exp = TRUE,
           Noflags = FALSE,
           makeplots = FALSE,
           saveplots = FALSE,
           makeMeta = TRUE,
           out = "" ) 
{
    #X list of files to be processed.
    #Can be a single file.
    #If wd is same as file dr than only file names, charecter type, else use full path
    #Selected geospatial band to be extracted. Must use standard NASA abreviations, spaces as underscores, Function may only process one raster band at a time currently
    #filename allows for an overiding file name, date of image will be appended to name. Else name is raster band name with date appended
    #output.extension selects the format for all output raster files to be saved
    #output.proj selects the output project to convert to
    #meta.name names the extracted metadata file being saved
    #project.Ext specifies the output raster(s) extent and must be in the coordinate system of output.proj
    #meta.exp allows users to choose whether metadata is even exported. if False no metadata is saved
    #additional functions may be written in such as masking using flags or masks before exporting the raster.
    #All additional functions will have available the temporary rasterized band file and all other bands and masks in the netCDF
    #additional function results will be saved to the export files
    #NoFlags <- flags automatically exported unless otherwise specified
    #Require packages
    require(raster)
    require(RNetCDF)
    require(ncdf4)
    require(plyr)
    require(rgdal)
    require(sp)
    require(ggmap)
    require(ggplot2)
    require(stringr)
    require(svMisc)
    #
    #Start the timer
    Start=proc.time()
    #Extract all metadata
    if (isTRUE(makeMeta)) {
      if(is.na(str_extract(band, "/"))) { #Detects if band variable is abreviated or includes the prefix geophsical data. Adds prefix if needed
        bnd <-
          paste("geophysical_data/", band, sep = "") #full length band variable name
      }else {
        
        bnd <- band
      }
      metadat <- data.frame() # empty container
      print("Extracting metadata...")
      for (i in 1:length(dat)) {
        entry <- try(metaXtract(dat[i], band = band), silent=TRUE)
        datet <- NetCDFdateT(dat[i]) #Extract date
        name <- paste(band, datet[1], "_", datet[2], output.extension, sep = "") #make name
        name <- str_replace_all(name, ":", "_") #edit string
        entry <- cbind(entry, name) #add file save name to end of metadata
        metadat <- rbind(metadat, entry)
        #add metadata to dataframe row
        #Create progress counter by percentage
        z <- round(i / length(dat), 2) * 100
        cat(progress(z))
        if (i == length(dat)) {
          cat("Done!\n")
        }
      }
      write.csv(metadat, paste(out, meta.name, sep = "")) #save metadata for whole list
    }
    #
    #Make the basemap for plotting
    if(makeplots==TRUE) {
      message("making basemap for plotting...")
      e <- as(proj.Ext, "SpatialPolygons") #Make Polygon of extent
      proj4string(e) <- CRS(output.proj)#set extent projection
      SU <- spTransform(e, CRS("+init=epsg:4326")) #Transform bounding polygon to standard google projection
      b <- bbox(SU) #Create bounding box
    bm <- 
      ggmap(suppressWarnings(
        get_map(
          location = b,
          maptype = "hybrid",
          zoom = 5,
          messaging = FALSE)))
    } else { #Escape option if makeplots=FALSE
      bm<-NA
    }
    #Extract Rasters
    #Begin processing loop
    print("Extracting Rasters...")
    error_list <- list() #Create empty list to catalog any files that fail to rasterize
    for (i in 1:length(dat)) {# make loop
      datet <- NetCDFdateT(dat[i]) #Extract date
      name <- #create name
        paste(band, datet[1], "_", datet[2], output.extension, sep = "") #make name
      name <- str_replace_all(name, ":", "_")
      plotname <- paste(band, datet[i]) #create automated plotname
      y <- dat[i]
      #try extracting the raster
      Rasta <- try(ExtractRaster(
        y,
        band = band,
        Noflags = FALSE,
        output.proj,
        proj.Ext = proj.Ext,
        makeplots = makeplots,
        saveplots = saveplots,
        plotname = plotname,
        base=bm
      ),
      silent = TRUE)
      if (class(Rasta) == "try-error") {
        #Deal with errors
        message(paste("Error: file", y, "failed to extract"))
        error_list <- c(error_list, y)
      } else {
        z <- round(i / length(dat), 2) * 100
        cat(progress(z))
        if (i == length(dat)) {
          cat("Done!\n")
        }
        #Save raster
        attr(Rasta, "attributes") <-
          metaXtract(y, band = band) #Add global attributes to file
        writeRaster(Rasta, paste(out, name, sep=""), overwrite = TRUE)
      }
      if (i == length(dat) & length(error_list > 0)) {
        assign("error_list", error_list, envir = globalenv())
        message("the following files failed to rasterize:")
        print(error_list)
      }
    }
    Start-proc.time()
  }
