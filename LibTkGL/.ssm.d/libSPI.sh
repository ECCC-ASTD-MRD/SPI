#----- Profile for external users (non SSM)

#----- GDAL data
export GDAL_DATA=${SPI_LIB}/share/gdal
export GDAL_PAM_ENABLED=NO
export CPL_LOG=/dev/null

#----- PROJ4 data
export PROJ_LIB=${SPI_LIB}/share/proj

#----- Force GRIB API Tables
export GRIB_SAMPLES_PATH=${SPI_LIB}/share/grib_api/samples
export GRIB_DEFINITION_PATH=${SPI_LIB}/share/grib_api/definitions

#----- Force BURP table dir since it's hardcoded with RMN and problem outside cmc
export MA_TABLEBURP_PERSONNELLE=${SPI_LIB}/share/rmn/table_b_bufr
