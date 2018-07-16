#!/ssm/net/cmoe/apps/libSPI_7.12.2_ubuntu-14.04-amd64-64/TCL/bin/tclsh8.6

# The above path is by checking what ../../../eer_SPI/bin/SPI execs at the end

load ../libTclGeoEER7.12.2[info sharedlibextension]

# metobs test create_test_obs test_obj_name

# metobs test show test_obs

# metobs test show test_obj_name
metobs table -readmaster B /home/binops/afsi/sio/env_ubuntu-14.04-amd64-64/afsisio/datafiles/constants/table_b_bufr_f
metobs table -readmaster D /home/binops/afsi/sio/env_ubuntu-14.04-amd64-64/afsisio/datafiles/constants/table_d_bufr_f
#metobs test sql
#
metobs create ACARS
metobs read ACARS /fs/cetus/fs2/ops/cmoe/afsr005/Data/SQLite/acars.sqlite

puts "TEST.TCL : ====== NOTE : The following test is supposed to fail because of those"
puts "TEST.TCL : ============= new undoucmented element codes.  Good thing the errors"
puts "TEST.TCL : ============= are well handled and reported :)"
metobs create OTHEROBS
metobs read OTHEROBS /users/dor/afsm/pca/Documents/GitHub/SPI_PHIL/LibTkGL/TclGeoEER/MetObsTest/sql.other
# metobs test show sql_test

puts "TEST.TCL : done"
