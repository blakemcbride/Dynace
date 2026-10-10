#!/bin/sh
#  Build and run every example against an ASAN/UBSAN build of Dynace.
#  Invoked by "make asan" from the top-level directory.

SAN="-g -O1 -fno-omit-frame-pointer -fsanitize=address,undefined"
ASAN_OPTIONS=${ASAN_OPTIONS:-detect_leaks=0:abort_on_error=0}
UBSAN_OPTIONS=${UBSAN_OPTIONS:-print_stacktrace=1}
export ASAN_OPTIONS UBSAN_OPTIONS

cd `dirname $0`
fail=""
for d in exam*/ ; do
	d=${d%/}
	( cd $d && make -s clean >/dev/null 2>&1 && make -s DEBUG="$SAN" >build.log 2>&1 ) || {
		echo "$d: BUILD FAILED (see $d/build.log)"; fail="$fail $d"; continue; }
	( cd $d && timeout 60 ./main </dev/null >run.log 2>&1 )
	rc=$?
	if grep -q "runtime error:\|ERROR: AddressSanitizer\|ERROR: LeakSanitizer" $d/run.log ; then
		echo "$d: SANITIZER REPORT (rc=$rc, see $d/run.log)"; fail="$fail $d"
	else
		echo "$d: ok (rc=$rc)"
	fi
done
[ -z "$fail" ] && echo "All examples clean." || { echo "Problems:$fail"; exit 1; }
