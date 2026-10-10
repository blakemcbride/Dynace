# Dynace — Bug Findings

Deep static analysis of the Dynace source tree (kernel, class library, threads,
and the `dpp` preprocessor). Findings below were located by subsystem sweeps and
then **individually verified against the source** by re-reading the cited code.

Line numbers refer to the maintainable `.d` sources (and the hand-written `.c`
files); do **not** edit the generated `.c` files.

## Severity legend

- **High** — memory corruption, crash, or silent wrong results reachable by
  ordinary (often untrusted/network) input.
- **Medium** — corruption/crash reachable with unusual-but-plausible input, or a
  clear functional defect.
- **Low** — latent defect, narrow trigger, or platform-specific.

## Cross-cutting patterns (root causes)

1. **`vsprintf`/`vsnprintf`-less formatting into fixed stack buffers.** Nearly
   every `vPrintf`/`vSprintf`/`vError` uses `vsprintf(buf, fmt, …)` into a fixed
   `char buf[N]` with no bound. Any formatted argument longer than the buffer
   smashes the stack. Instances: `String.d:643` (1024), `Stream.d:67` (1024,
   only a *post-hoc* sentinel), `Object.d:70` (4096), `dpp/OutputStream.d:596`
   and `dpp/OutputStream2.d:138` (256). Fix once: switch to `vsnprintf(buf,
   sizeof buf, …)` everywhere (and grow-or-truncate as appropriate).
2. **`strcpy`/unbounded copies of caller- or network-supplied data into fixed
   buffers.** Sockets, HTTP, XML, MIME, filenames, masks, and the preprocessor
   all copy variable-length input into fixed arrays without length checks.
3. **The two `dpp` tokenizers disagree on `MAXWORDSZ`** (`InputStream.d` = 256,
   `dpp.c` = 80) while downstream `dpp.c` name buffers are sized for 80. This
   underlies several preprocessor overflows.

---

# Kernel / runtime (`kernel/`)

### K1 — Integer overflow in `_Dynace_calloc` — Medium
`kernel/malloc.c:226-233`
```c
void *_Dynace_calloc( long size, long nelem ) {
    char *rv = _Dynace_malloc(size*nelem);   // no overflow check
    if (rv) memset(rv, 0, (size_t)(size * nelem));
    ...
```
`size*nelem` is computed in `long` with no overflow guard, then used for both
allocation and `memset`. A large element count (reachable through `Tncalloc`,
e.g. array classes sized from input) wraps to a small allocation followed by
callers writing the full count → heap overflow. Real `calloc` rejects this.
Worse on 32-bit-`long` builds (`THIRTYTWOBIT`). Fix: reject when
`nelem != 0 && size > LONG_MAX/nelem`.

### K2 — GC marker overflows `short`/`unsigned short` for large objects — Medium
`kernel/kernel.c:1703, 1710, 1732` (`Dynace_cm_gMarkObject`)
```c
sz = (short)(SCIV(ClassOf(obj))->effective_iv_size
             - (EVEN(sizeof(Object_iv_t)) + sizeof(char *)));
while (sz >= 0) { ... obj->siz = sz; ... sz = (short) rtn->siz; ... }
```
`sz` is a signed `short` and the pointer-reversal offset is stashed in
`obj->siz` (`unsigned short`). For any class whose direct+inherited
instance-variable area exceeds ~32 KB, `sz` starts negative, so `while (sz >= 0)`
never runs — **the object's instance variables are never scanned, so referenced
objects are not marked and can be collected while still reachable**
(use-after-free). Offsets >32767 also corrupt the reversal traversal. Fix: use
`int`/`unsigned` for the marking offset and widen `siz` (or store the temp
elsewhere).

### K3 — Lock-free method-cache reads lack memory barriers — Low
`kernel/kernel.c:1209` (read) and `1071-1074` (`cache_add` publish)
The fast path in `_FindMethod` walks `method_cache->table[...]` without
`MC_CS`, while `cache_add` publishes a node with `table[pos] = e` after writing
`e->next`. There is no acquire/release fence. On weak-memory architectures
(ARM/AArch64) with `NATIVE_THREADS`, a reader can observe the linked node before
its field writes are visible and dispatch through a garbage `meth` pointer. Fix:
use an explicit release on publish / acquire on read (or C11 atomics).

### K5 — Unbounded format buffers in kernel error paths — Low
`kernel/Object.d:70` (`vError`, `buf[4096]`), `kernel/Object.d:80,90`
(`gSubclassResponsibility`/`gShouldNotImplement`, `sprintf` into `buf[100]`),
`kernel/Behavior.d:113` (`gDoesNotImplement`, `buf[100]`). A long class or
method name (or a long `vError` message) overflows these fixed buffers. Same fix
as the cross-cutting `vsnprintf`/`snprintf` pattern.

### K6 — GC stack scan misses objects held in `main`'s frame — High — **FIXED**
**Status: FIXED** in commit `0a5a303` — `InitKernel` now uses the OS-reported stack
beginning (`__libc_stack_end` on glibc, `pthread_get_stackaddr_np` on macOS,
`NtCurrentTeb()->StackBase` on Windows) instead of the pointer passed to `InitDynace`.

`kernel/kernel.c` (`InitKernel`, `Dynace_cm_gGC`): the GC scanned the stack from
its own frame up to `StackBeg`, which was `&argc` from `InitDynace(&argc)`. Any
object pointer the compiler placed in `main`'s frame *above* `argc` was never
marked, so live objects were freed (use-after-free). Found by ASan in exam34
(`gPrint(a)` after a GC freed `a`); normal builds escaped only by frame-layout luck.

### K7 — Heap objects are only 2-byte aligned — Medium
`kernel/kernel.c:59` (`ALIGNMENT (2)`), `EVEN()` rounds instance sizes to 2 or 4
bytes, so objects containing pointers/doubles land on addresses like `...a2`.
UBSan reports this throughout the kernel, `Link.d` and `LinkList.d`. Undefined
behavior in C; harmless on x86 but can fault or run slowly on strict-alignment
CPUs (relevant to the `ARM32SF` build). Fix: round instance sizes and the
`instance_block` header to `sizeof(void *)`/`alignof(max_align_t)`.

### K8 — `_dynace_signal` locates handlers by scanning raw stack memory — Low
`kernel/kernel.c:2432`: walks upward from `&cond` in 2-byte steps reading `long`s
until it finds the `_SIG1`/`_SIG2` signature. Works today, but depends on stack
layout and reads outside any object (ASan reports a stack-buffer-overflow).
Fragile under other compilers/optimizations.

---

# `dpp` preprocessor (`dpp/`)

### D1 — Unbounded string-literal copy overflows `token[256]` — High — **FIXED**
**Status: FIXED** — `token` is now `MAXBUF` (the line-buffer size), so no token can outgrow it. Also removes the D7 overflow in this tokenizer. Verified under ASan with a 1000-char string literal (old dpp: stack-buffer-overflow; new: clean, literal intact).

`dpp/InputStream.d:218-228` (`gNextToken`), buffer declared line 176
(`token[MAXWORDSZ]`, `MAXWORDSZ` = 256). The string/char-literal branch copies
the whole literal into `token` with no bound; the source line buffer is
`MAXBUF` = 4096. **Any `.d` file with a string constant longer than 255
characters** (a long format/SQL/help string) overflows this stack buffer during
normal `.d`→`.c` conversion.

### D2 — Same unbounded string-literal copy in `gettoken` — High — **FIXED**
**Status: FIXED** — `token` is now sized to the 10000-byte line buffer. Verified under ASan with `dpp -s` on a 1000-char literal (old: global-buffer-overflow; new: clean). `make newgens` output unchanged.

`dpp/dpp.c:4865-4875`, `token[BUFSIZE]` (256), fed from `buf[10000]`. Reached in
the `-s`/`-g`/`-e` scan paths when a scanned source contains a string literal
> 255 chars. Overwrites adjacent static storage.

### D8 — `get_line` continuation lines overflow `buf[10000]` — High — **FIXED**
**Status: FIXED** — `gGets` is now passed only the space remaining in `buf`. Verified under ASan with 15 KB of `\`-continued lines (old: global-buffer-overflow; new: clean).

`dpp/dpp.c` (`get_line`): a line ending in `\` is joined with the next by reading
it at `tbuf += i`, but `gGets` was still passed `sizeof buf`, so a long run of
continuation lines (e.g. a large multi-line `#define`) wrote past the static
10000-byte buffer.

### D3 — Method-name construction overflows 81/90-byte buffers — Medium
`dpp/dpp.c:3420-3543` (`pp_method`). `mname/gname/fmname/ename` are
`char[MAXWORDSZ+1]` = 81, `olmname` is `MAXWORDSZ+10` = 90 (`MAXWORDSZ` = 80
here). `strcpy(gname, p)` (3464) copies a token that `InputStream` allows up to
256 chars; and `sprintf(fmname, "%s_%cfm_%s", className, …, gname)` (3521),
`sprintf(olmname, "%s_%cvm_%s", className, …, mname)` (3526, which re-embeds the
already-built `mname`) exceed 81/90 once class + generic names are moderately
long. Overflows on ordinary user code with long class/method names.

### D4 — `strcpy` of the input path into `sfile[50]` — Medium
`dpp/dpp.c:4003` + `4035` (`preprocess`): `char sfile[50]; strcpy(sfile, file);`
where `file` is the command-line `.d` path. Any input path ≥ 50 chars (common
with deep trees) overflows the stack buffer.

### D5 — `make_macro_name`/`new_ext` overflow 60/80-byte buffers — Medium
`dpp/dpp.c:621-639` (`make_macro_name`, no bound) writes into 60-byte buffers
`macro[60]` (2071, from a header filename), `td[60]` (2168, from a generic
name), and `macro[60]` (4275/4308, from the source filename). `dpp/dpp.c:664-667`
(`new_ext`) does `strcpy(fname[80], file)`. A filename or generic name longer
than ~58–76 chars overflows.

### D6 — Other command-line / class-name copies — Medium/Low
`dpp/dpp.c:314` `strcpy(Sfile2[128], <-64 arg>)`; `dpp/OutputStream.d`
class-name `sprintf` into `cvar[80]`/`cname[80]`/`ctype[80]` (122/187/439…);
`dpp/Prototype.d:121-123` copies a variable name into `var[50]`;
`dpp/dpp.c:3003/3057` `make_arg`/`pp_make_arg` can push `buf[256]` past its end
because the guard is checked before appending a whole token.

### D7 — Identifier off-by-two write to `token[256]/[257]` — Low
`dpp/InputStream.d:183-184`: a pre-loop write plus `for (i=0; ++i <= MAXWORDSZ …)`
writes one element past `token[MAXWORDSZ]` and the terminator one further.
Requires a ≥257-character identifier.

---

# String / array / collection classes (`class/`)

### S1 — `print_nest` double pointer-increment → OOB read — High — **FIXED**
**Status: FIXED** — `t = gStringRepValue(t);`. Verified `gStringRep` on an `ObjectArray` of {"alpha", NULL, 42, "omega"} prints each element once, in order.

`class/Array.d:547-549`
```c
for (i=0 ; i++ != iNelm ; )
    if (t = *val++) {
        t = gStringRepValue(*val++);   // advances val a SECOND time
```
Each iteration advances `val` twice for a non-NULL element and string-reps the
*wrong* element (`k+1`, not the tested `k`). For an all-non-NULL object array it
reads `iNelm` elements past `iArray` and calls `gStringRepValue` on a garbage
pointer. Triggered by `gStringRep` on any `ObjectArray`. Fix: `t = gStringRepValue(t);`

### S2 — `gJustifyCenter` shift-left: unbounded backward write — High — **FIXED**
**Status: FIXED** — fill loop is now `for (i=n-p ; i < n ; ) s[i++] = ' ';`. Verified under ASan: `"   ab "` → `"  ab  "`, `"      x"` → `"   x   "`, shift-right / unchanged / all-spaces cases correct.

`class/String.d:995-999`
```c
if (left2 < left) {          /* shift left */
    p = left - left2;
    memmove(s, s + p, n - p);
    for (i=n-(p+1) ; i < n ; )
        s[i--] = ' ';        // i DECREMENTS; i<n stays true forever
```
The fill loop decrements `i` while testing `i < n`, so it writes
`s[n-p-1], s[n-p-2], … s[0], s[-1], …` until it walks off the front of the
buffer (crash / heap corruption). Triggered whenever leading spaces > trailing
spaces (e.g. `gJustifyCenter` on `"   ab "`). Fix: mirror the shift-right branch:
`for (i=n-p ; i < n ; ) s[i++] = ' ';`

### S3 — `gGets` ignores the buffer-size argument — High — **FIXED**
**Status: FIXED** — copy loop now also stops at `len < n`. Verified under ASan with an 8-byte buffer: `"hello world\nnext"` reads as `"hello w"`, `"orld\n"`, `"next"`.

`class/String.d:597-602`. `gGets(char *buf, int n)` mimics `fgets` but computes
`len` from the string contents (up to `'\n'` or `iLen`) and never clamps to `n`:
`memcpy(buf, iStr, len); buf[len] = '\0';`. A line longer than the caller's
buffer overflows it. Fix: `if (len > n) len = n;` before the copy.

### S4 — `vSprintf` overflows a 1024-byte stack buffer — High — **FIXED**
**Status: FIXED** — `vsnprintf` into the 1024-byte buffer; longer results are formatted again into a heap buffer of the exact size. Verified under ASan: 3000-char `vSprintf` and `gStringRepValue` return full-length results.

`class/String.d:639-644`: `char buf[1024]; vsprintf(buf, fmt, _rest_);`.
Reachable through `gStringRepValue` → `vSprintf(String, "\"%s\"", iStr)`:
inspecting/printing any String longer than ~1022 chars smashes the stack. Fix:
`vsnprintf`.

### S5 — `gShape` writes 4-byte values into a 2-byte `ShortArray` — High — **FIXED**
**Status: FIXED** — store each dimension through `(short *)` (`class/Array.d:193`). Verified: shape of a 3×4×5 array returns `3 4 5`.

`class/Array.d:189-193`
```c
r = vNew(ShortArray, 1, iRank);                 // 2 bytes/element
...
((INDEX_TYPE *) iv2->iArray)[i] = iShape[i];    // INDEX_TYPE == unsigned (4 bytes)
```
`INDEX_TYPE` is `unsigned` (`array2.h:34`) but the backing store holds 2-byte
shorts. This writes `4*iRank` bytes into a `2*iRank`-byte heap block (corrupting
the adjacent allocation) and returns garbage shape values for rank > 1. `gIota`
(line 284) correctly uses `(short *)`. Fix: cast to `(short *)`.

### S6 — Mask copies overflow `mask[128]` — Medium
`class/String.d:1128` (and `gRemoveMask`, `:1189`): `strcpy(mask, … inmask+1)`
into `char mask[MASK_MAX]` (128) when the mask isn't a preset — a caller mask
≥ 128 chars overflows. `gLoadMask` (`:1092-1104`) likewise fills class buffer
`cMaskBuf[128]` unbounded, and indexes `cMaskVal[(int)*p1]` with a **signed**
char (line 1095) so bytes ≥ 0x80 give a negative index / OOB read (line 1097
correctly uses `(unsigned char)`).

---

# Linked structures / associations / trees (`class/`)

### L1 — `gMoveBeginning` uses `iList` after `Remove` nulled it — High — **FIXED**
**Status: FIXED** — save `lst = iList` before `Remove`; use `lst` for `AddAfter` and the critical section. Verified moving tail/middle/head links to the front.

`class/Link.d:178-195`
```c
Remove(self);              // sets iList = iNext = iPrev = NULLOBJ  (Link.d:94)
AddAfter(iList, self);     // iList is now NULLOBJ
```
`Remove(self)` clears `self`'s `iList`, so the following `AddAfter(iList, self)`
dispatches on a null object (crash / lost node). Under `NATIVE_THREADS` the
`gLeaveCriticalSection(iList)` at line 193 also runs on the nulled `iList`, so
the section entered at 181 is never released (lock leak). Triggered when moving
any non-first element to the front. `gMoveAfter`/`gMoveBefore` are correct
because they target the `lnk` *parameter*. Fix: save `object lst = iList;` before
`Remove`.

### L2 — `gMoveEnd` — same defect with `AddBefore` — High — **FIXED**
**Status: FIXED** — same fix as L1 in `gMoveEnd`. Verified moving head/middle/tail links to the end.

`class/Link.d:216-219`. Identical `Remove`-then-use-`iList` bug and lock leak.

### L3 — `gFindNext`/`gFindPrev` missing empty-tree guard — Low
`class/BTree.d:145-156, 176-187` call `gFindBTNGT`/`gFindBTNLT(iNodes, …)`
without the `iNodes && iNum` guard used by every sibling finder. After
`gDisposeAllNodes` sets `iNodes = NULL`, reusing a stale `*foundKey` dispatches a
generic on NULL → crash.

### L4 — `gDeleteBTNode` returns a just-disposed node — Low
`class/BTreeNode.d:329`: on the collapse path `collapse()` disposes the leaf
(`self`), then `delete` does `return self;` — a dangling pointer propagated up
to `gDisposeObj`. Harmless if only NULL-tested, a use-after-free if dereferenced.

---

# I/O and networking (`class/`)

### N1 — SOCKS5 domain reply overflows `buf[81]` — High
`class/Socket.d:409-413` (`gProxyConnect`): for `ATYP == 3` it reads one length
byte `c` (`unsigned char`, up to 255) then `gRead(obj, buf, c+2)` — up to 257
bytes — into `unsigned char buf[81]` (line 331). A malicious/compromised SOCKS5
proxy smashes the stack.

### N2 — `gRecvFile` filename length overflows `buf[2048]` — High
`class/Socket.d:785-788`: `RECV(nlen); len = ntohs(nlen); … gRead(self, buf,
len)` where `nlen` is a 16-bit length taken straight off the socket (up to
65535) and `buf` is `char buf[BUFLEN]` (2048). Any sender claiming a filename
length > 2048 overflows the stack.

### N3 — `gSendStr` unbounded `strcpy` — High
`class/Socket.d:685-687`: `char buf[80]; strcpy(buf, str);` with no length
check. Any `str` ≥ 80 chars overflows. (`gProxyConnect` has the same pattern at
`:362/367/395` copying proxy user/password/hostname into `buf[81]` — Medium.)

### N4 — XML `<!DOCTYPE>` parsing overflows `target[128]` and the body buffer — High
`class/XMLNode.d:418-444` (`parse_doctype`): the name loop `*p++ = c` into
`char target[128]` has no bound, and the body loop writes into the caller's
`buf` (`pstr->buf`, initially 10000) with a bare `*p++ = c` — **without** the
`CHECK_SIZE` guard the rest of the parser uses. A DOCTYPE name > 128 bytes or
body larger than the buffer overflows. Reachable from `gParseString`/`gParseFile`
on untrusted XML.

### N5 — XML `<? … ?>` processing-instruction parsing — same overflow — High
`class/XMLNode.d:447-490` (`parse_instruction`): `target[128]` filled unchecked
and the PI body written with bare `*p++ = c`, again bypassing `CHECK_SIZE`.

### N6 — `LOG_MODE_BOTH` writes the body to the wrong descriptor — Medium
`class/Logger.d:176-182`: the header goes to `STDOUT_FILENO` (line 177), but the
message body and newline are written to `fd` (the log file) again (179, 181)
instead of `STDOUT_FILENO`. Result: in BOTH mode the body is written to the log
file **twice** and never appears on stdout. Both should be `STDOUT_FILENO`.

### N7 — `BufferedTempFile.gRead` reads past valid data — Medium
`class/BufferedTempFile.d:80`: `bytesRead = min(n, iBufSize - iCurrentOffset)`
uses `iBufSize` (fixed capacity, 1024) instead of `iSize` (bytes actually
written). After a seek-back, `gRead` returns calloc'd zero bytes beyond real
end-of-data, over-reporting valid data.

### N8 — `BufferedTempFile.gGets` double-counts the offset → OOB write — Medium
`class/BufferedTempFile.d:138-141`: `tmpBuffer` already points inside `iBuffer`,
yet the code computes `tmpBuffer - iBuffer + iCurrentOffset`, adding
`iCurrentOffset` again. When `iCurrentOffset > 0` the read length and the
`buf[…] = 0` index exceed `sz`, overrunning the caller's buffer. Correct index:
`tmpBuffer - (iBuffer + iCurrentOffset)`.

### N9 — `FileFindCRC` calls `fclose(NULL)` on open failure — Medium
`class/crc.d:213-218`: `fp = fopen(file, MODE); if (!fp) *ret = 1; else crc =
FindCRC(...); fclose(fp);` — `fclose` runs unconditionally, so a failed `fopen`
calls `fclose(NULL)` (crashes on glibc). Move `fclose` into the `else`.

### N10 — `MimeBodyPart.gGetHeader` `strncpy` without null-termination — Medium
`class/MimeBodyPart.d:68`: `strncpy(tmpBuf, startPos, sizeof(tmpBuf))` (256) does
not terminate when the header line is ≥ 256 bytes; the subsequent
`strchr(tmpBuf, ':')` reads past the stack buffer. Header content is parsed MIME.
(`HttpRequest.d:233` has the same `strncpy`-no-terminator pattern before
`strlwr` — Low.)

### N11 — `HttpRequest` realloc-failure leaves a dangling payload — Medium (OOM only)
`class/HttpRequest.d:263-273`: on `realloc` failure `curl_callback` does
`free(p->payload)` but never sets `p->payload = NULL`. `restCall` later stores
`iResponseStr = cf->payload` (line 407) and `gDispose` frees it again →
use-after-free / double free. Only under out-of-memory.

### N12 — `Stream.vPrintf` fixed 1024 buffer; guard is post-overflow — Medium
`class/Stream.d:59-73`: `vsprintf(buf, fmt, _rest_)` into `char buf[1024]` with
sentinel bytes checked *after* the write — the stack is already smashed by the
time the check runs. `XMLNode.dump_xml` funnels untrusted element names/values
through this. Fix: `vsnprintf`.

### N13 — `sleepInMilliSeconds` uses wrong time units — Low
`class/Socket.d:872`: `tval.tv_usec = time%1000;` treats a millisecond remainder
as microseconds; should be `(time%1000)*1000`. Sub-second sleeps are 1000× too
short (affects the SSL-handshake wait).

### N14 — XML parser dead guard and inverted whitespace loop — Low
`class/XMLNode.d:427,459`: `if (p == buf)` compares a pointer into local
`target[128]` against `buf` (into `pstr->buf`); always false, so the intended
empty-name rejection never fires (should be `p == target`).
`class/XMLNode.d:451`: `while (EOF == (c=GETC(is)) && isspace(c));` can only loop
when a char is simultaneously EOF and whitespace — it never skips leading
whitespace (should be `EOF != …`).

### N15 — Unbounded XPath / filename copies — Low
`class/XMLNode.d` copies path segments/subscripts into `name[128]`,
`sub[128]`, `buf[1024]` unchecked (`pXPath`/`parse_sub`/`gXPath`). `Logger.d:80,
105` and `File.d:136,305` `strcpy` filenames / `getenv("TMP")` into
`iLogFileName[1024]` / `tp[256]` / `cTempFileDir[256]`. Normally developer- or
env-controlled, hence Low.

---

# Numbers, dates, and error handling (`class/`)

### T1 — `DateTime` add-time methods double-increment the day — High — **FIXED**
**Status: FIXED** — `ot` is now captured after the higher-unit call, so only the remainder's wrap is detected. Verified 23:30 + 90 min / 5400 s / 5.4M ms → day+1 01:00, and the negative direction across midnight.

`class/DateTime.d:145-209` (`gAddMinutes`, `gAddSeconds`, `gAddMilliseconds`).
Each captures `ot = gTimeValue(self)` **before** delegating to the higher-unit
method (`gAddHours(self, h)` etc.), which already advances the day when it
crosses midnight. The trailing wrap-detection then compares the final time
against the *stale* `ot` and adds a day a **second** time.
Verified by trace: **23:30 + 90 minutes yields day+2 01:00 instead of the
correct day+1 01:00.** (`gAddHours` is correct because it splits whole days via
`gAddDays`, which does not touch the time value.) Fix: capture `ot` around only
the lowest-unit `…(super, …)` adjustment.

### T2 — `Time.gNewWithStr` reads past a short string — Medium
`class/Time.d:332-351`: only checks `if (tmStr)`, then reads
`tmStr[0,1,3,4,6,7]` unconditionally → OOB read on inputs like `"1"`. The
millisecond branch is guarded by `strlen > 10` but reads `tmStr[11]` (needs
length ≥ 12); at length 11 it reads the NUL and computes a wrong value. `Date.d`
correctly guards with `strlen >= 10`.

### T3 — `readlink` return used as an index without checking −1 — Low
`class/StackTracer.d:228`: `name_buf[readlink("/proc/self/exe", name_buf, 511)] =
0;` — on `readlink` failure (−1) this writes `name_buf[-1]` (stack underflow).

### T4 — Midnight formats as `"0:MM AM"` — Low
`class/Date.d:317-331` (`Time2`): the AM/PM conversion subtracts 12 only for
hours > 12 and never maps hour 0 → 12, so midnight prints `"0:MM AM"` instead of
`"12:MM AM"` (fed by the `%T`/`%t` mask). `Time.d`'s `Tmfmt` handles this
correctly, so the two formatters disagree.

---

# Threads (`threads/`)

### H1 — `Pipe.gGets` writes the terminator past the data — High — **FIXED**
**Status: FIXED** — keep the buffer start, terminate with `*buf = '\0'`, return the start. Verified with a canary-filled buffer: no write past the NUL, return value is the caller's buffer.

`threads/Pipe.d:193-195`: `buf` is advanced inside the copy loop
(`buf += bg;`), so after the loop `buf` points at the end of the copied data;
`buf[tr] = '\0'` then stores the NUL `tr` bytes past the data just written —
outside the caller's buffer on any successful read. `return buf;` also returns
the advanced pointer instead of the start. Fix: `*buf = '\0'; return`
the saved original start.

### H2 — `Pipe.gGets` reads `iRptr[-1]` on a blocking empty pipe — Low — **FIXED**
**Status: FIXED** (with H1) — guard is now `if (bg && iRptr[bg-1] == '\n')`.

`threads/Pipe.d:172`: in blocking mode the empty-buffer early-out at line 163
doesn't fire, so the loop runs with `ba == 0`, leaving `bg == 0`, and
`if (iRptr[bg-1] == '\n')` dereferences one byte before the buffer.

### H3 — `autoDispose` cleanup is unreachable → thread leak — Medium
`threads/Thread.d:459-461`: after a worker returns, `Kill(cCt->iObj, …)` is
called; for a thread killing itself (`cCt == iv`) `Kill` sets `cCt = NULL` and
calls `_dynace_yield()`, which `longjmp`s away and never returns. The following
`if (cCt->iAutoDispose) Dispose(cCt->iObj);` therefore never runs, so
auto-dispose threads are never disposed and stay registered in the `cThreads`
dictionary (a GC root) — the thread object and its resources leak. (The defined
`EXPUNGE_THREAD` state is never used, consistent with this path being
unfinished.)

### H4 — `plan9.c` file-copy loops forever on a read error — Medium (Plan 9 only)
`kernel/plan9.c:146-162` (`copy1`): the `for(;;)` copy loop only breaks when
`read()` returns 0; a negative return (I/O error) skips the copy block and
re-reads forever. The `if (n < 0)` handler after the loop is dead code. Triggered
when `rename`/`mv` falls back to copying and a read error occurs.

---

## Summary counts

| Subsystem | High | Medium | Low |
|---|---|---|---|
| Kernel/runtime | 1 | 3 | 3 |
| dpp preprocessor | 3 | 3 | 2 |
| Strings/collections | 5 | 1 | – |
| Linked/trees | 2 | – | 2 |
| I/O & networking | 5 | 6 | 3 |
| Numbers/dates/errors | 1 | 1 | 2 |
| Threads | 1 | 2 | 1 |

**Fixed so far:** K6, D1, D2, D8, S1, S2, S3, S4, S5, L1, L2, T1, H1, H2.

The **highest-priority fixes**, because they are reachable from untrusted or
network input: N1, N2, N4, N5 (Socket/XML overflows), S1–S5 (String/Array
memory corruption), T1 (silently wrong dates), and D1/D2 (preprocessor crashes
on ordinary long string literals). Note that most of the buffer overflows share
the two root-cause patterns above and can be swept together (`vsnprintf`/
`snprintf` and bounded copies).
