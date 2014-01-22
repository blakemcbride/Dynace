/*
  Copyright (c) 1996 Blake McBride
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/



#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif

#include <windows.h>
#include <tchar.h>
#include <imagehlp.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <shlwapi.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <sys\locking.h>
#include <io.h>

// Make typedefs for some IMAGEHLP.DLL functions so that we can use them
// with GetProcAddress
typedef BOOL (__stdcall * SYMINITIALIZEPROC)( HANDLE, LPSTR, BOOL );
typedef BOOL (__stdcall *SYMCLEANUPPROC)( HANDLE );

typedef BOOL (__stdcall * STACKWALKPROC)
				( DWORD, HANDLE, HANDLE, LPSTACKFRAME, LPVOID,
				PREAD_PROCESS_MEMORY_ROUTINE,PFUNCTION_TABLE_ACCESS_ROUTINE,
				PGET_MODULE_BASE_ROUTINE, PTRANSLATE_ADDRESS_ROUTINE );

typedef LPVOID (__stdcall *SYMFUNCTIONTABLEACCESSPROC)( HANDLE, DWORD );

typedef DWORD (__stdcall *SYMGETMODULEBASEPROC)( HANDLE, DWORD );

typedef BOOL (__stdcall *SYMGETSYMFROMADDRPROC)
							( HANDLE, DWORD, PDWORD, PIMAGEHLP_SYMBOL );

typedef BOOL (__stdcall *SYMGETLINEFROMADDRPROC)
							( HANDLE, DWORD, PDWORD, PIMAGEHLP_LINE );

typedef DWORD (__stdcall *SYMSETOPTIONSPROC)
							( DWORD );

defclass StackTracer
{
	LPTOP_LEVEL_EXCEPTION_FILTER iPreviousFilter;
	TCHAR iLogFileName[MAX_PATH];
	BOOL iDisplayOn;
	BOOL iLogOn;
	ifun	iFunction;	//  message function

class:
	object cTracer;
	char cImageName[255];
	char cPDBPath[255];
	char cAppName[255];
	char cImageHlpVersion[32];

	SYMINITIALIZEPROC _SymInitialize;
	SYMCLEANUPPROC _SymCleanup;
	STACKWALKPROC _StackWalk;
	SYMFUNCTIONTABLEACCESSPROC _SymFunctionTableAccess;
	SYMGETMODULEBASEPROC _SymGetModuleBase;
	SYMGETSYMFROMADDRPROC _SymGetSymFromAddr;
	SYMGETLINEFROMADDRPROC _SymGetLineFromAddr;
	SYMSETOPTIONSPROC _SymSetOptions;

init: init_class;
};

static	int	command_line_error_handler(char *msg);

static	int	lock_file(char *file)
{
	int	h;
	char	buf[256];
	
	sprintf(buf, "%s.lck", file);
	h = open(buf, _O_CREAT | _O_TRUNC |_O_WRONLY, _S_IREAD | _S_IWRITE);
	if (h == -1)
		return h;
	lseek(h, 0L, SEEK_SET);
	_locking(h, _LK_LOCK, 1L);
	return h;
}

static	void	unlock_file(int h)
{
	if (h != -1) {
		lseek(h, 0L, SEEK_SET);
		_locking(h, _LK_UNLCK, 1L);
	}
}

#define	MIN_SIZE	60000
#define	MAX_SIZE	1000000

private imeth void pLogMessage(LPCTSTR pszMessage)
{
	int	h, lf;
	struct	_stat	sb;

	if (iLogOn == FALSE)
		return;
	lf = lock_file(iLogFileName);
	if (!_stat(iLogFileName, &sb)  &&  sb.st_size > MAX_SIZE) {
		int	h2;
		char	*buf, tmpfile[256], *p;

		if (!(h = open(iLogFileName, _O_BINARY | _O_RDONLY))) {
			unlock_file(lf);
			return;
		}
		sprintf(tmpfile, "%s.tmp", iLogFileName);
		if (!(h2 = open(tmpfile, _O_TRUNC | _O_CREAT | _O_BINARY | _O_WRONLY, _S_IREAD | _S_IWRITE))) {
			close(h);
			unlock_file(lf);
			return;
		}
		buf = malloc(MIN_SIZE+10);
		lseek(h, -MIN_SIZE, SEEK_END);
		read(h, buf, MIN_SIZE);
		buf[MIN_SIZE] = '\0';
		for (p=buf ; *p  &&  (p[0] != '/'  ||  p[1] != '/'  ||  p[2] != ' '  ||  p[3] != '='  ||  p[4] != '=') ; p++);
		write(h2, p, strlen(p));
		close(h);
		close(h2);
		free(buf);
		unlink(iLogFileName);
		rename(tmpfile, iLogFileName);
	}

	h = open(iLogFileName, _O_APPEND | _O_CREAT | _O_TEXT | _O_WRONLY, _S_IREAD | _S_IWRITE);
	if (h != -1) {
		write(h, pszMessage, strlen(pszMessage));
		close(h);
	}
	unlock_file(lf);
}

static	void	init_class()
{
	HMODULE hModImagehlp = LoadLibrary( _T("DBGHELP.DLL") );
	DLLGETVERSIONPROC proc;
	cTracer = gNew(StackTracer);

	if ( !hModImagehlp )
	{
		pLogMessage(cTracer, "LoadLibrary() for imagehlp.dll failed.");
		return;
	}

	GetModuleFileName(hModImagehlp, cImageName, sizeof(cImageName));

	proc = (DLLGETVERSIONPROC)GetProcAddress(hModImagehlp, "DllGetVersion");
	if (proc)
	{
		DLLVERSIONINFO info;
		info.cbSize = sizeof(info);
		proc(&info);
		sprintf(cImageHlpVersion, "%ld,%ld,%ld", info.dwMajorVersion, info.dwMinorVersion, info.dwBuildNumber);
	}
	else
	{
		strcpy(cImageHlpVersion, "Unknown");
	}
	
	_SymInitialize = (SYMINITIALIZEPROC)GetProcAddress( hModImagehlp,
							    "SymInitialize" );
	if ( !_SymInitialize )
	{
		pLogMessage(cTracer, "SymInitialize address not found.");
		return;
	}

	_SymCleanup = (SYMCLEANUPPROC)GetProcAddress( hModImagehlp, "SymCleanup" );
	if ( !_SymCleanup )
	{
		pLogMessage(cTracer, "SymCleanup address not found.");
		return;
	}

	_StackWalk = (STACKWALKPROC)GetProcAddress( hModImagehlp, "StackWalk" );
	if ( !_StackWalk )
	{
		pLogMessage(cTracer, "StackWalk address not found.");
		return;
	}

	_SymFunctionTableAccess = (SYMFUNCTIONTABLEACCESSPROC)
                GetProcAddress( hModImagehlp, "SymFunctionTableAccess" );

	if ( !_SymFunctionTableAccess )
	{
		pLogMessage(cTracer, "SymFunctionTableAccess address not found.");
		return;
	}

	_SymGetModuleBase = (SYMGETMODULEBASEPROC)GetProcAddress( hModImagehlp,
								  "SymGetModuleBase");
	if ( !_SymGetModuleBase )
	{
		pLogMessage(cTracer, "SymGetModuleBase address not found.");
		return;
	}

	_SymGetSymFromAddr = (SYMGETSYMFROMADDRPROC)GetProcAddress( hModImagehlp,
								    "SymGetSymFromAddr" );
	if ( !_SymGetSymFromAddr )
	{
		pLogMessage(cTracer, "SymGetSymFromAddr address not found.");
		return;
	}

	_SymGetLineFromAddr = (SYMGETLINEFROMADDRPROC)GetProcAddress( hModImagehlp,
								      "SymGetLineFromAddr" );
/*
	We are deliberately not failing here because imagehlp.dll that comes with
	NT 4.0 does not support this functionality. 

	if ( !_SymGetLineFromAddr )
		return;
*/

	_SymSetOptions = (SYMSETOPTIONSPROC)GetProcAddress( hModImagehlp,
							    "SymSetOptions" );
	if ( !_SymSetOptions )
	{
		pLogMessage(cTracer, "SymSetOptions address not found.");
		return;
	}
	gSetErrorFunction(Object, command_line_error_handler);
}

imeth void gSetLogOn(BOOL bOn) 
{ 
	iLogOn = bOn; 
}

imeth void gSetDisplayOn(BOOL bOn) 
{ 
	iDisplayOn = bOn; 
}

imeth BOOL gIsLogOn() 
{ 
	return iLogOn; 
}

imeth BOOL gIsDisplayOn() 
{ 
	return iDisplayOn; 
}

imeth    gGetLogFileName()
{
	return gNewWithStr(String, iLogFileName);
};

imeth	gSetLogFileName(char *logFileName)
{
	strcpy(iLogFileName, logFileName);
	return self;
}

cmeth   gGetTracer()
{
	return cTracer;
}

static LONG WINAPI ST_UnhandledExceptionFilter(PEXCEPTION_POINTERS pExceptionInfo );

cmeth   gNew()
{
	PTSTR pszDot;
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iDisplayOn = FALSE;
	iLogOn = TRUE;

	// Install the unhandled exception filter function
	iPreviousFilter = SetUnhandledExceptionFilter(ST_UnhandledExceptionFilter);

	// Figure out what the report file will be named, and store it away
	GetModuleFileName( 0, iLogFileName, MAX_PATH );
	strcpy(cPDBPath, iLogFileName);
	strcpy(cAppName, iLogFileName);
	
	*(_tcsrchr(cPDBPath, _T('\\')) + 1) = 0x00;

	// Look for the '.' before the "EXE" extension.  Replace the extension
	// with ".stackdump.txt"
	pszDot = _tcsrchr( iLogFileName, _T('.') );
	if ( pszDot )
	{
		pszDot++;   // Advance past the '.'
		if ( _tcslen(pszDot) >= 3 )
		{
			_tcscpy( pszDot, _T("stackdump.txt") );   
		}
	}

	return obj;
}

imeth    gDispose, gDeepDispose()
{
	SetUnhandledExceptionFilter( iPreviousFilter );
	return gDispose(super);
}

private imeth void pDisplayMessage(LPCTSTR pszMessage)
{
	if (iDisplayOn)
//		MessageBox(NULL, pszMessage, "Stack Dump", MB_OK);
		if (iFunction)
			iFunction(pszMessage);
}

static	int	command_line_error_handler(char *msg)
{
	gStackDump(gGetTracer(StackTracer), msg);
	return 0;
}

imeth	ofun	gSetFunction(int (*fun)())
{
	ofun	ret = (ofun) iFunction;
	iFunction = fun;
	return ret;
}

private imeth LPTSTR pGetExceptionString( DWORD dwCode )
{
#define EXCEPTION( x ) case EXCEPTION_##x: return _T(#x);

    switch ( dwCode )
    {
        EXCEPTION( ACCESS_VIOLATION )
        EXCEPTION( DATATYPE_MISALIGNMENT )
        EXCEPTION( BREAKPOINT )
        EXCEPTION( SINGLE_STEP )
        EXCEPTION( ARRAY_BOUNDS_EXCEEDED )
        EXCEPTION( FLT_DENORMAL_OPERAND )
        EXCEPTION( FLT_DIVIDE_BY_ZERO )
        EXCEPTION( FLT_INEXACT_RESULT )
        EXCEPTION( FLT_INVALID_OPERATION )
        EXCEPTION( FLT_OVERFLOW )
        EXCEPTION( FLT_STACK_CHECK )
        EXCEPTION( FLT_UNDERFLOW )
        EXCEPTION( INT_DIVIDE_BY_ZERO )
        EXCEPTION( INT_OVERFLOW )
        EXCEPTION( PRIV_INSTRUCTION )
        EXCEPTION( IN_PAGE_ERROR )
        EXCEPTION( ILLEGAL_INSTRUCTION )
        EXCEPTION( NONCONTINUABLE_EXCEPTION )
        EXCEPTION( STACK_OVERFLOW )
        EXCEPTION( INVALID_DISPOSITION )
        EXCEPTION( GUARD_PAGE )
        EXCEPTION( INVALID_HANDLE )
    }

    return NULL;
}

private imeth BOOL pGetLogicalAddress(PVOID addr, PTSTR szModule, DWORD len, DWORD *section, DWORD *offset )
{
	MEMORY_BASIC_INFORMATION mbi;
	DWORD hMod;
	PIMAGE_DOS_HEADER pDosHdr;
	PIMAGE_NT_HEADERS pNtHdr;
	DWORD rva;
	PIMAGE_SECTION_HEADER pSection;
	unsigned i;


	__try
	{	

		if ( !VirtualQuery( addr, &mbi, sizeof(mbi) ) )
			return FALSE;

		hMod = (DWORD)mbi.AllocationBase;

		if ( !GetModuleFileName( (HMODULE)hMod, szModule, len ) )
			return FALSE;

		// Point to the DOS header in memory
		pDosHdr = (PIMAGE_DOS_HEADER)hMod;

		
		// From the DOS header, find the NT (PE) header
		pNtHdr = (PIMAGE_NT_HEADERS)(hMod + pDosHdr->e_lfanew);

		pSection = IMAGE_FIRST_SECTION( pNtHdr );

		rva = (DWORD)addr - hMod; // RVA is offset from module load address

		// Iterate through the section table, looking for the one that encompasses
		// the linear address.
		for (i = 0;
			 i < pNtHdr->FileHeader.NumberOfSections;
			 i++, pSection++ )
		{
			DWORD sectionStart = pSection->VirtualAddress;
			DWORD sectionEnd = sectionStart
				+ max(pSection->SizeOfRawData, pSection->Misc.VirtualSize);

			// Is the address in this section???
			if ( (rva >= sectionStart) && (rva <= sectionEnd) )
			{
				// Yes, address is in the section.  Calculate section and offset,
				// and store in the "section" & "offset" params, which were
				// passed by reference.
				*section = i+1;
				*offset = rva - sectionStart;
				return TRUE;
			}
		}
	}
	__except(EXCEPTION_EXECUTE_HANDLER)
	{
	}

	return FALSE;   // Should never get here!
}

private imeth void pImagehlpStackWalk(LPTSTR szText, PCONTEXT pContext )
{
	STACKFRAME sf;

	_tcscat(szText, _T("\n") );
	memset( &sf, 0, sizeof(sf) );

	// Initialize the STACKFRAME structure for the first call.  This is only
	// necessary for Intel CPUs, and isn't mentioned in the documentation.
	sf.AddrPC.Offset       = pContext->Eip;
	sf.AddrPC.Mode         = AddrModeFlat;
	sf.AddrStack.Offset    = pContext->Esp;
	sf.AddrStack.Mode      = AddrModeFlat;
	sf.AddrFrame.Offset    = pContext->Ebp;
	sf.AddrFrame.Mode      = AddrModeFlat;

	while ( 1 ) {
		DWORD dwTempDis = 0; 
		BYTE symbolBuffer[ sizeof(IMAGEHLP_SYMBOL) + 512 ];
		PIMAGEHLP_SYMBOL pSymbol = (PIMAGEHLP_SYMBOL)symbolBuffer;
		DWORD symDisplacement = 0;  // Displacement of the input address,
		// relative to the start of the symbol
		IMAGEHLP_LINE line;

		if ( ! _StackWalk(  IMAGE_FILE_MACHINE_I386,
				    GetCurrentProcess(),
				    GetCurrentThread(),
				    &sf,
				    pContext,
				    0,
				    _SymFunctionTableAccess,
				    _SymGetModuleBase,
				    0 ) )
			break;

		if ( 0 == sf.AddrFrame.Offset ) // Basic sanity check to make sure
			break;                      // the frame is OK.  Bail if not.

		// IMAGEHLP is wacky, and requires you to pass in a pointer to a
		// IMAGEHLP_SYMBOL structure.  The problem is that this structure is
		// variable length.  That is, you determine how big the structure is
		// at runtime.  This means that you can't use sizeof(struct).
		// So...make a buffer that's big enough, and make a pointer
		// to the buffer.  We also need to initialize not one, but TWO
		// members of the structure before it can be used.

		pSymbol->SizeOfStruct = sizeof(symbolBuffer);
		pSymbol->MaxNameLength = 512;
                        
		line.SizeOfStruct = sizeof(line);

		if ( _SymGetSymFromAddr(GetCurrentProcess(), sf.AddrPC.Offset,
					&symDisplacement, pSymbol))
		{
			_stprintf(szText + _tcslen(szText), _T("%hs() "), pSymbol->Name);
		}

		if (_SymGetLineFromAddr)
		{
			// The problem is that the symbol engine finds only those source 
			// line addresses (after the first lookup) that fall exactly on 
			// a zero displacement. I?ll walk backward 100 bytes to 
			// find the line and return the proper displacement. 
			dwTempDis = 0;
			while ( FALSE == _SymGetLineFromAddr (GetCurrentProcess(), 
				sf.AddrPC.Offset - dwTempDis, &symDisplacement, &line) ) 
			{ 
				dwTempDis += 1 ; 
				if ( 100 == dwTempDis ) 
					break; 
			} 

		}


		if (_SymGetLineFromAddr && dwTempDis < 100)
		{
			char fname[_MAX_FNAME];
			char ext[_MAX_EXT];

			_splitpath(line.FileName, NULL, NULL, fname, ext);
			_stprintf(szText + _tcslen(szText), _T("in %hs%hs line %ld\n"), fname, ext, line.LineNumber);
		}
		else    // No symbol found.  Print out the logical address instead.
		{
			TCHAR szModule[MAX_PATH] = _T("");
			DWORD section = 0, offset = 0;
			char fname[_MAX_FNAME];
			char ext[_MAX_EXT];

			if (pGetLogicalAddress(self, (PVOID)sf.AddrPC.Offset,
					   szModule, sizeof(szModule), &section, &offset ))
			{
				_splitpath(szModule, NULL, NULL, fname, ext);

				_stprintf(szText + _tcslen(szText), _T("%04X:%08X %s%s\n"),
					  section, offset, fname, ext );
			}
			else
			{
				_tcscat(szText, _T(" Unrecognized file format in stackframe.\n"));
			}
		}
	}
}

private imeth void pGenerateExceptionReport(LPTSTR szText, PEXCEPTION_POINTERS pExceptionInfo )
{
	PEXCEPTION_RECORD pExceptionRecord = pExceptionInfo->ExceptionRecord;
	TCHAR szFaultingModule[MAX_PATH];
	DWORD section, offset;
	PCONTEXT pCtx = pExceptionInfo->ContextRecord;

	if (_SymSetOptions == 0)
	{
		strcpy(szText, "Unable to load imagehlp.dll\n");
		return;
	}

	_SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES);

	if ( !_SymInitialize( GetCurrentProcess(), cPDBPath, TRUE ) )
	{
		LPVOID lpMsgBuf;
		FormatMessage( 
			FORMAT_MESSAGE_ALLOCATE_BUFFER | 
			FORMAT_MESSAGE_FROM_SYSTEM | 
			FORMAT_MESSAGE_IGNORE_INSERTS,
			NULL,
			GetLastError(),
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
			(LPTSTR) &lpMsgBuf,
			0,
			NULL 
			);
		_stprintf(szText + _tcslen(szText), "SymInitialize Error: %s\n", lpMsgBuf);
		if (lpMsgBuf)
			LocalFree( lpMsgBuf );
		return;
	}
	// First print information about the type of fault
	_stprintf(szText + _tcslen(szText),  _T("Exception code: %08X %s\n"),
		  pExceptionRecord->ExceptionCode,
		  pGetExceptionString(self, pExceptionRecord->ExceptionCode) );

	// Now print information about where the fault occured
	pGetLogicalAddress(self, pExceptionRecord->ExceptionAddress,
			   szFaultingModule,
			   sizeof( szFaultingModule ),
			   &section, &offset );

	pImagehlpStackWalk(self, szText, pCtx );
	_SymCleanup(GetCurrentProcess());
	_tcscat(szText, _T("\n") );
}

private imeth LONG pExecutePrevious(PEXCEPTION_POINTERS pExceptionInfo)
{
	if ( iPreviousFilter )
		return iPreviousFilter( pExceptionInfo );
	else
		return EXCEPTION_CONTINUE_SEARCH;
}

static LONG WINAPI ST_UnhandledExceptionFilter(PEXCEPTION_POINTERS pExceptionInfo )
{
	TCHAR szText[64000];
	object tracer = gGetTracer(StackTracer);
	char szTime[128];
	char szDate[128];

	__try
	{
		sprintf(szText, "// =====================================================\nDateTime: %s:%s\nAppName: %s\nImagehlp: %s (%s)\n\n", _strdate(szDate), _strtime(szTime), cAppName, cImageName, cImageHlpVersion);

		pGenerateExceptionReport(tracer, szText, pExceptionInfo );

		pLogMessage(tracer, szText);
		pDisplayMessage(tracer, szText);
	}
	__except(EXCEPTION_EXECUTE_HANDLER)
	{
		sprintf(szText, "// =====================================================\nDateTime: %s:%s\nAppName: %s\nImagehlp: %s (%s)\n\n", _strdate(szDate), _strtime(szTime), cAppName, cImageName, cImageHlpVersion);
		strcat(szText, "Exception while building exception report.");
		pLogMessage(cTracer, szText);
	}

	return pExecutePrevious(tracer, pExceptionInfo);

}

private imeth void pCopyPointers(PEXCEPTION_POINTERS pIn, PEXCEPTION_POINTERS pOut)
{
	memcpy(pOut->ContextRecord, pIn->ContextRecord, sizeof(CONTEXT));
	memcpy(pOut->ExceptionRecord, pIn->ExceptionRecord, sizeof(EXCEPTION_RECORD));
}

private imeth void pGetStackDumpText(LPTSTR szText)
{
	EXCEPTION_POINTERS ptrs;
	EXCEPTION_RECORD rec;
	CONTEXT context;

	ptrs.ContextRecord = &context;
	ptrs.ExceptionRecord = &rec;

	__try
	{
		memcpy(0, 0, 1);
//		DebugBreak();
	}
	__except(pCopyPointers(self, GetExceptionInformation(), &ptrs),1)
	{
		pGenerateExceptionReport(self, szText, &ptrs);
	}
}

imeth    gGetStackDumpText()
{
	TCHAR szText[64000];
	pGetStackDumpText(self, szText);
	return gNewWithStr(String, szText);
}

imeth void gStackDump(char *pszMessage)
{
	TCHAR szText[64000];
	char szTime[128];
	char szDate[128];

	sprintf(szText, "// =====================================================\nDateTime: %s:%s\nAppName: %s\nImagehlp: %s (%s)\n\n", _strdate(szDate), _strtime(szTime), cAppName, cImageName, cImageHlpVersion);

	if (pszMessage)
	{
		_tcscat(szText, pszMessage);
		_tcscat(szText, "\n\n");
	}

	pGetStackDumpText(self, szText);

	pLogMessage(self, szText);
	pDisplayMessage(self, szText);
}

