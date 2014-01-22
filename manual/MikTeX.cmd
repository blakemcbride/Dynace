@echo off

rem To build the texinfo dump use:
rem    initex texinfo @dump
rem and put it with the other .fmt files

rem tex --undump=texinfo %1.tex
rem texindex %1.fn
rem tex --undump=texinfo %1.tex
rem dvipdfm %1

texinfo %1.tex
texindex %1.fn
texinfo %1.tex
dvipdfm %1
