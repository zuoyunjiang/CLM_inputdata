####future inputdata####
library(ncdf4)
library(progress)

a <- list.dirs('D:/AAAA-资料E盘/CLM/inputdata/future inputdata/inputdat/')
a <- a[c(-1,-2,-6,-10,-14)]
setpath <- list.dirs('D:/Rtest/future/')
setpath <- setpath[c(-1,-2,-6,-10,-14)]
for(i in 7:12){
  b <- list.files(a[i])
  b_path <- paste(a[i],'/',b,sep = '')
  n <- length(b_path)
  # dirs <- paste('D:/Rtest/future/',substr(a[i],55,100),'/',sep = '')
  setwd(setpath[i])
  pb <- progress_bar$new(total=n)
  for(j in 1:n){
    # o_data <- nc_open('D:/AAAA-资料E盘/CLM/inputdata/inputdata/atm/datm7/CLM1PT_data/1901-01.nc')
    o_data <- nc_open(b_path[j])
    EDGEE1 <- ncvar_get(nc = o_data,varid = 'EDGEE')
    EDGEN1 <- ncvar_get(nc = o_data,varid = 'EDGEN')
    EDGES1 <- ncvar_get(nc = o_data,varid = 'EDGES')
    EDGEW1 <- ncvar_get(nc = o_data,varid = 'EDGEW')
    FLDS1 <- ncvar_get(nc = o_data,varid = 'FLDS')
    FSDS1 <- ncvar_get(nc = o_data,varid = 'FSDS')
    LAT1 <- ncvar_get(nc = o_data,varid = 'LATIXY')
    LON1 <- ncvar_get(nc = o_data,varid = 'LONGXY')
    PRECTMMS1 <- ncvar_get(nc = o_data,varid = 'PRECTmms')
    PSRF1 <- ncvar_get(nc = o_data,varid = 'PSRF')
    QBOT1 <- ncvar_get(nc = o_data,varid = 'QBOT')
    TBOT1 <- ncvar_get(nc = o_data,varid = 'TBOT')
    WIND1 <- ncvar_get(nc = o_data,varid = 'WIND')
    ZBOT1 <- ncvar_get(nc = o_data,varid = 'ZBOT')
    TIME1 <- ncvar_get(nc = o_data,varid = 'time')
    timea <- ncatt_get(nc = o_data,varid = 'time')
    
    RH1 <- (QBOT1*1013.15/(0.378*QBOT1+0.622))/(6.112*exp((17.67*(TBOT1-273.15))/(TBOT1-273.15+243.5)))*100
    
    num <-length(RH1) 
    # create dim 
    filename <- b[j]
    
    dimX <- ncdim_def('lon','',1:1,unlim = FALSE,create_dimvar = FALSE)
    dimy <- ncdim_def('lat','',1:1,unlim = FALSE,create_dimvar = FALSE)
    dims <- ncdim_def('scalar','',1:1,unlim = FALSE,create_dimvar = FALSE)
    # c <- paste('days since ',substr(filename[i],1,7) ,'-01 00:00:00',sep = '')
    c <- timea$units
    dimtime <- ncdim_def('time',longname = 'Time axis', units = c,vals = TIME1,unlim = FALSE,create_dimvar = TRUE,calendar = 'noleap')
    
    #mv <- 1.e30
    EDGEE <- ncvar_def('EDGEE',longname = 'eastern edge in atmospheric data', units = 'degrees E', dims, prec = 'double')
    EDGEN <- ncvar_def('EDGEN',longname = 'northern edge in atmospheric data', units = 'degrees N', dims, prec = 'double')
    EDGES <- ncvar_def('EDGES',longname = 'southern edge in atmospheric data', units = 'degrees N', dims, prec = 'double')
    EDGEW <- ncvar_def('EDGEW',longname = 'western edge in atmospheric data', units = 'degrees E', dims, prec = 'double')
    FLDS <- ncvar_def('FLDS','W/m2',list(dimX, dimy, dimtime),longname = 'incident longwave (FLDS)',prec = 'double')
    FSDS <- ncvar_def('FSDS','W/m2',list(dimX, dimy, dimtime),longname = 'incident solar (FSDS)',prec = 'double')
    LATIXY <- ncvar_def('LATIXY', longname = 'latitude', units = 'degrees N', list(dimX,dimy),prec = 'double')
    LONGXY <- ncvar_def('LONGXY', longname = 'longitude',units = 'degrees E', list(dimX,dimy),prec = 'double')
    PRECTmms <- ncvar_def('PRECTmms','mm/s',list(dimX, dimy, dimtime),longname = 'precipitation (PRECTmms)',prec = 'double')
    PSRF <- ncvar_def('PSRF','Pa',list(dimX, dimy, dimtime),longname = 'pressure at the lowest atm level (PSRF)',prec = 'double')
    RH <- ncvar_def('RH','%',list(dimX, dimy, dimtime),longname = 'relative humidity at the lowest atm level (RH)',prec = 'double')
    TBOT <- ncvar_def('TBOT','K',list(dimX, dimy, dimtime),longname = 'temperature at the lowest atm level (TBOT)',prec = 'double')
    # time <- ncvar_def('time','days since 2005-01-01 00:00:00',dimtime,longname = 'Time axis',prec = 'double',missval = mv)
    WIND <- ncvar_def('WIND','m/s',list(dimX, dimy, dimtime),longname = 'wind at the lowest atm level (WIND)',prec = 'double')
    ZBOT <- ncvar_def('ZBOT','m',list(dimX, dimy, dimtime),longname = 'observational height',prec = 'double')
    
    
    nc_data <- nc_create(filename = filename, list(EDGEE,EDGEN,EDGES,EDGEW,FLDS,FSDS,LATIXY,LONGXY,PRECTmms,PSRF,RH,TBOT,WIND,ZBOT))
    
    
    ncvar_put(nc_data,EDGEE,EDGEE1)
    ncvar_put(nc_data,EDGEN,EDGEN1)
    ncvar_put(nc_data,EDGES,EDGES1)
    ncvar_put(nc_data,EDGEW,EDGEW1)
    ncvar_put(nc_data,FLDS,FLDS1)
    ncvar_put(nc_data,FSDS,FSDS1)
    ncvar_put(nc_data,LATIXY,LAT1)
    ncvar_put(nc_data,LONGXY,LON1)
    ncvar_put(nc_data,PRECTmms,PRECTMMS1)
    ncvar_put(nc_data,PSRF,PSRF1)
    ncvar_put(nc_data,RH,RH1)
    ncvar_put(nc_data,TBOT,TBOT1)
    ncvar_put(nc_data,WIND,WIND1)
    ncvar_put(nc_data,ZBOT,ZBOT1)
    
    sstr <- paste(substr(b_path[j],55,100),'Lat:',LAT1,'LON:',LON1,sep = ' ')
    
    ncatt_put( nc_data, 0, "institute", "IGA") 
    ncatt_put( nc_data, 0, "history", "Jun, 2022, the file was created at IGA for CLM-mircrobe simulation --Yunjiang Zuo  zuoyunjiang@iga.ac.cn")
    ncatt_put( nc_data, 0, "site_location", sstr)
    nc_close(nc_data)
    nc_close(o_data)
    
    pb$tick()
    Sys.sleep(0.05)
  }
  
}


