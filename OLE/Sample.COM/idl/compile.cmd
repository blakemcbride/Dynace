@echo off
midl /nologo /ms_ext /c_ext /app_config %1.idl 
cl -nologo -c %1_i.c
cl -nologo -c %1_p.c dlldata.c
link /DLL /NOLOGO /def:interface.def /out:interface.dll *.obj rpcrt4.lib
del %1_p.* dlldata.* *.lib *.exp
