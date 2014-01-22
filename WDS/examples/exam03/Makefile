
all: main.res resources.scm

main.res : main.rc resource.h
	rc main.rc


resources.scm : resource.h
	gawk -f h2scm.awk <$<  >$@

clean:
	rm -zq resources.scm main.res
	rm -zq *~ *.bak

