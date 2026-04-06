# Dynace

Portable, open-source object-oriented extension to C. Adds classes, instances, methods, generic functions, multiple inheritance, and garbage collection to standard C via a metaclass-based system (similar to CLOS/Smalltalk). Written entirely in C — no new syntax beyond what the `dpp` preprocessor handles.

## Build

```bash
make                  # Build (Linux/Mac/BSD/Cygwin/Solaris)
make debug            # Build with -g debug symbols
make all-scratch      # Full rebuild from scratch
make clean            # Remove intermediate files
make realclean        # Remove all generated files
make makegens         # Rebuild generics.h from scratch
make newgens          # Regenerate generics.h and preprocess all .d files
```

Windows: `nmake -f makefile.msc`  
Plan 9: `mk`

### Build options (via environment or make vars)

- `DEBUG=1` — debug build (-g)
- `NATIVE_THREADS=1` — enable pthreads support
- `BOEHM_GC=1` — use Boehm GC instead of built-in mark-and-sweep
- `STRAT=-S1|-S2|-S3|-S4` — dispatch strategy (S1=asm fastest, S2=portable C default, S3/S4=C++ inline)
- `ARM32SF=1` — cross-compile for 32-bit ARM
- `FASTWIDE=1` — fast-and-wide generic cache
- `THIRTYTWOBIT=1` — force 32-bit build

### Build order

kernel → class → threads → dpp → generics

The `setup.unx` target runs automatically on first build to normalize line endings and set file timestamps (`.d` before `.c` for make dependency ordering).

## Project structure

```
kernel/       Core OO runtime (Object, Class, MetaClass, Method, GenericFunction, Behavior, Dynace)
class/        Class library (~70 classes: String, Array, Dictionary, Date, File, Set, etc.)
threads/      Thread, Pipe, Semaphore classes
dpp/          Dynace preprocessor (converts .d → .c, generates generics.h/generics.c)
include/      Global headers (generics.h, dynl.h)
lib/          Built libraries (dynace.a)
bin/          Built executables (dpp, delcr, addcr)
generics/     Scripts for generating generics.h
examples/     29 tutorial programs (exam01–exam38)
docs/         Text documentation (BUILD.txt, Tech-Summary.txt, DESCRIBE.txt, etc.)
manual/       300+ page manual (PDF, HTML, LaTeX source)
```

## The .d file format

`.d` files are Dynace class definitions processed by `dpp` into standard `.c` files. Structure:

```c
defclass ClassName : ParentClass {
    int iVar1;           // instance variables (i prefix convention)
    char *iVar2;
class:
    int cClassVar;       // class variables (c prefix convention)
init: init_function;     // optional class initializer
};

imeth int gMethodName(int arg) { ... }    // instance method
cmeth object gNew(int size) { ... }       // class method
```

### Method types
- `imeth` — instance method
- `cmeth` — class method
- `objrtn` — constructor/initializer returning object
- `vfun` — varargs function

### Generic naming convention
- `g` prefix — compile-time argument checked generics
- `v` prefix — varargs generics (different signatures across classes)

## DPP preprocessor

Located in `dpp/dpp.c` (5154 lines). Converts `.d` → `.c` and maintains `generics.h`/`generics.c`.

Key DPP flags: `-C` (application source), `-p` (preprocess .d to .c), `-h` (generate header), `-i` (generate .iv), `-s` (short scan), `-g file` (base generics), `-S1`–`-S4` (strategy), `-F` (fast-wide cache), `-X` (allow overloads)

The make rule is: `%.c: %.d` → `$(DPP) $(DPPOPTS) -p $<`

**Do not hand-edit generated .c files in kernel/, class/, or threads/.** Edit the corresponding `.d` file instead.

## Coding conventions

- Instance variables: `i` prefix (`iStr`, `iSize`, `iRank`)
- Class variables: `c` prefix (`cMaskVal`)
- Access via `ivPtr(obj)` macro returning pointer to instance var struct
- K&R brace style, tab indentation in class files
- All files carry BSD 2-Clause copyright (Blake McBride)
- One class per `.d` file; class name matches filename
- Methods are file-private; external access only via generic functions declared in `generics.h`

## Compiler settings

```
CC = gcc
CFLAGS = -I$(INCDIR) $(OPT) -Wno-parentheses -fPIC
LDFLAGS = -lm
```

Link against `lib/dynace.a` and `-lm`. Add `-lpthread` if `NATIVE_THREADS`.

## Testing / examples

Examples in `examples/exam01/` through `examples/exam38/`. Each has a `main.c`, `makefile`, and `readme`. Build with `make` from within the example directory (requires Dynace to be built first).

## Key documentation

- `docs/Tech-Summary.txt` — best technical overview
- `docs/BUILD.txt` — build instructions for all platforms
- `docs/DESCRIBE.txt` — component descriptions
- `manual/Dynace.pdf` — comprehensive 300+ page reference
