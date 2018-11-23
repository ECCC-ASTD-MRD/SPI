#----- Profile for external users (non SSM)

#----- GDAL data
export GDAL_PAM_ENABLED=NO
export CPL_LOG=/dev/null

#----- Force BURP table dir since it's hardcoded with RMN and problem outside cmc
export MA_TABLEBURP_PERSONNELLE=${SPI_LIB}/share/rmn/table_b_bufr
