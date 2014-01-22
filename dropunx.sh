#!/bin/sh -u
# This script removed the need for "-f makefile.unx" when making the system
# by taking the .unx off all the unix makefiles
UNX_FILES=`find . -name makefile.unx -print`
for UNX_FILE in $UNX_FILES
do
	FILE=`echo $UNX_FILE | sed 's/\.unx//g'`
	mv $UNX_FILE $FILE
done
sed 's/\makefile.unx/makefile/g' <makefile >xxx
mv xxx makefile
