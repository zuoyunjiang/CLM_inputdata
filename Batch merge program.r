####  merge code  ####
#### Yunjiang Zuo --zuoyunjiang@iga.ac.cn ####
####  2020-08--IGA   ####
#input file name to a
a <- list.files('atm data')
a
#create the file path
dir <- paste('./atm data/',a ,sep = '')
dir
# file number
n <- length(dir)
n
library(ncdf4)
merge.data <- nc_open(filename = dir[1])
print(merge.data)
v1 <- ncvar_get(nc = merge.data, varid = 'ZBOT')
v2 <- ncvar_get(nc = merge.data, varid = 'WIND')
v3 <- ncvar_get(nc = merge.data, varid = 'PSRF')
v4 <- ncvar_get(nc = merge.data, varid = 'TBOT')
v5 <- ncvar_get(nc = merge.data, varid = 'RH')
v6 <- ncvar_get(nc = merge.data, varid = 'PRECTmms')
v7 <- ncvar_get(nc = merge.data, varid = 'FSDS')
v8 <- ncvar_get(nc = merge.data, varid = 'FLDS')
merge.data <- cbind(v1,v2,v3,v4,v5,v6,v7,v8)
for(i in 2:n){
  new.data <- nc_open(filename = dir[i])
  v1 <- ncvar_get(nc = new.data, varid = 'ZBOT')
  v2 <- ncvar_get(nc = new.data, varid = 'WIND')
  v3 <- ncvar_get(nc = new.data, varid = 'PSRF')
  v4 <- ncvar_get(nc = new.data, varid = 'TBOT')
  v5 <- ncvar_get(nc = new.data, varid = 'RH')
  v6 <- ncvar_get(nc = new.data, varid = 'PRECTmms')
  v7 <- ncvar_get(nc = new.data, varid = 'FSDS')
  v8 <- ncvar_get(nc = new.data, varid = 'FLDS')
  new.data <- cbind(v1,v2,v3,v4,v5,v6,v7,v8)
  merge.data <- rbind(merge.data, new.data)
}

write.csv(merge.data, file = './atm data/2005-2013.csv', row.names = F)
