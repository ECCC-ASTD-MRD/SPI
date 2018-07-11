all:
	cd .. ; make

install:
	cd ../.. ; ./makeit -spi

reconf:
	cd ../.. ; ./makeit -spi -reconf

test: all
	../MetObsTest/test.tcl
