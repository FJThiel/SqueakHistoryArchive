'From Squeak 2.5 of August 6, 1999 [latest update: #1544] on 13 October 1999 at 11:07:27 pm'!"Change Set:		cHeaderMods-jhmDate:			13 October 1999Author:			John MaloneySeveral minor changes to the Squeak Virtual Machine C header filessq.h, sqConfig.h, and sqPlatformSpecific.h. The changes include:1. Collected all the millisecond time functions in single place in   sq.h and added a comment explaining their purposes. Those   maintaining virtual machine ports should check this comment   to be sure that their timing implementations are reasonable.   (As we discovered, a poor choice of clock functions can slow   down the virtual machine significantly!!)2. Made the reservation of C heap space be a macro that does   nothing by default. Platforms other than Macintosh probably   need not override the definition of this macro.3. Defined the symbol UNIX if any variant of Unix or Linux is   detected in sqConfig.h.4. Declared the function pluginAllowAccessToFilePath(char *filePathName)   in sq.h. This is a hook for a browser plugin version of Squeak."!!Interpreter methodsFor: 'image save/restore' stamp: 'jm 10/13/1999 15:11'!readImageFromFile: f HeapSize: desiredHeapSize StartingAt: imageOffset	"Read an image from the given file stream, allocating the given amount of memory to its object heap. Fail if the image has an unknown format or requires more than the given amount of memory."	"Details: This method detects when the image was stored on a machine with the opposite byte ordering from this machine and swaps the bytes automatically. Furthermore, it allows the header information to start 512 bytes into the file, since some file transfer programs for the Macintosh apparently prepend a Mac-specific header of this size. Note that this same 512 bytes of prefix area could also be used to store an exec command on Unix systems, allowing one to launch Smalltalk by invoking the image name as a command."	"This code is based on C code by Ian Piumarta and Smalltalk code by Tim Rowledge. Many thanks to both of you!!!!"	| swapBytes headerStart headerSize dataSize oldBaseAddr minimumMemory memStart bytesRead bytesToShift heapSize |	self var: #f declareC: 'sqImageFile f'.	swapBytes _ self checkImageVersionFrom: f startingAt: imageOffset.	headerStart _ (self sqImageFilePosition: f) - 4.  "record header start position"	headerSize			_ self getLongFromFile: f swap: swapBytes.	dataSize				_ self getLongFromFile: f swap: swapBytes.	oldBaseAddr			_ self getLongFromFile: f swap: swapBytes.	specialObjectsOop	_ self getLongFromFile: f swap: swapBytes.	lastHash			_ self getLongFromFile: f swap: swapBytes.	savedWindowSize	_ self getLongFromFile: f swap: swapBytes.	fullScreenFlag		_ self getLongFromFile: f swap: swapBytes.	extraVMMemory		_ self getLongFromFile: f swap: swapBytes.	lastHash = 0 ifTrue: [		"lastHash wasn't stored (e.g. by the cloner); use 999 as the seed"		lastHash _ 999].	"decrease Squeak object heap to leave extra memory for the VM"	heapSize _ self cCode: 'reserveExtraCHeapBytes(desiredHeapSize, extraVMMemory)'.	"compare memory requirements with availability".	minimumMemory _ dataSize + 100000.  "need at least 100K of breathing room"	heapSize < minimumMemory		ifTrue: [self error: 'Insufficient memory for this image'].	"allocate a contiguous block of memory for the Squeak heap"	memory _ self cCode: '(unsigned char *) sqAllocateMemory(minimumMemory, heapSize)'.	memory = nil		ifTrue: [self error: 'Failed to allocate memory for the heap'].	memStart _ self startOfMemory.	memoryLimit _ (memStart + heapSize) - 24.  "decrease memoryLimit a tad for safety"	endOfMemory _ memStart + dataSize.	"position file after the header"	self sqImageFile: f Seek: headerStart + headerSize.	"read in the image in bulk, then swap the bytes if necessary"	bytesRead _ self cCode: 'sqImageFileRead(memory, sizeof(unsigned char), dataSize, f)'.	bytesRead ~= dataSize		ifTrue: [self error: 'Read failed or premature end of image file'].	swapBytes ifTrue: [self reverseBytesInImage].	"compute difference between old and new memory base addresses"	bytesToShift _ memStart - oldBaseAddr.	self initializeInterpreter: bytesToShift.  "adjusts all oops to new location"	^ dataSize! !!InterpreterSupportCode class methodsFor: 'source file exporting' stamp: 'jm 10/13/1999 23:06'!writePluginSupportFiles	"InterpreterSupportCode writePluginSupportFiles"	self	storeString: self squeakConfigFile onFileNamed: 'sqConfig.h'.	self	storeString: self squeakPlatSpecFile onFileNamed: 'sqPlatformSpecific.h'.	self	storeString: self squeakVirtualMachineHeaderFile onFileNamed: 'sqVirtualMachine.h'.! !!InterpreterSupportCode class methodsFor: 'source files' stamp: 'jm 10/13/1999 15:45'!squeakConfigFile	^ '/* sqConfig.h -- platform identification and configuration */#if defined(__MWERKS__) && !!defined(macintosh)  /* CodeWarrior 8 neglects to define "macintosh" */# define macintosh#endif#if defined(WIN32) || defined(_WIN32) || defined(Win32)  /* Some compilers use different win32 definitions.     Define WIN32 so we have only to check for one symbol. */# if !!defined(WIN32)#  define WIN32# endif#endif#if defined(macintosh)# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# define SQ_CONFIG_DONE#endif#if defined(ACORN)# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# define SQ_CONFIG_DONE#endif#if defined(WIN32)# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# if defined(_M_IX86) || defined(X86)  /* x86 systems */#  define DOUBLE_WORD_ALIGNMENT#  define DOUBLE_WORD_ORDER  /* Note: We include a generic sqWin32.h to override some settings */#  include "sqWin32.h"#  define SQ_CONFIG_DONE# elif defined(_WIN32_WCE)#  include "sqWin32.h"#  define SQ_CONFIG_DONE# else#  error unsupported win32 processor type (alpha?!!)# endif#endif/* for Unix variants, this file must define the following symbols as appropriate:   HAS_D_NAMLEN      defined if struct dirent has d_namlen field and hence directory names      are not null terminated.  if underfined then directory names are null      terminated.   HAS_TIMEZONE      defined if the external variable timezone is available, containing the      local offset from GMT in seconds.  if  undefined then the tm structure      must contain the same information in the tm_gmtoff field.  (Consider      defining NEED_TZSET too!!)   HAS_ON_EXIT      defined if cleanup functions are declared with on_exit().  If      undefined then cleanup functions are declared with atexit().   HAS_MSB_FIRST      defined if the most significant byte is first in an int.   HAS_LSB_FIRST      defined if the least significant byte is first in an int.      (Complains if neither of the last two are defined.)   HAS_SUN_AUDIO      defined if the platform supports the Sun /dev/audio device.   DOUBLE_WORD_ALIGNMENT      defined if the platform CANNOT support double-word accesses at an      arbitrary word address.         DOUBLE_WORD_ORDER      defined if the platform stores floats in the opposite order      to the Squeak image (the image is always PowerPC order).   SUN_FUN_KEYS      defined for Sun type 3 or 4 keyboards to enable the editing keys      (Again, Undo, Copy, Paste, Cut, Find, Stop on the "left keypad",      and PgUp, PgDn, Home, End keys on the right keypad).   NEED_FILIO      defined if the platform requires <sys/filio.h> to be included      to get a definition for FIONBIO.   NEED_SELECT      defined if the platform requires <sys/select.h> to be included      to get definitions for select().   NEED_TZSET      defined if the platform required tzset() to be called explicitly      before reading the local wall clock.   JUMP_ALIGN_BYTE      defined if the platform has no instruction alignment restrictions      (e.g. Pentium).   JUMP_ALIGN_STRICT      defined if the platform has word-aligned instructions, and cannot      tolerate the low two bits of an address being non-zero (e.g.      Sparc).  The symbol "UNIX" should defined below if any variant of Unix or  Linux is recognized.*/#if defined(sun) && (defined(sparc) || defined(__sparc))# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# include <errno.h># ifdef ECHRNG					/* Sparc/Solaris */#   undef  HAS_D_NAMLEN#   define HAS_TIMEZONE#   undef  HAS_ON_EXIT#   define HAS_MSB_FIRST#   undef  HAS_SUN_AUDIO#   define DOUBLE_WORD_ALIGNMENT#   undef  DOUBLE_WORD_ORDER#   define SUN_FUN_KEYS#   define NEED_FILIO#   define NEED_TZSET#   define JUMP_ALIGN_STRICT#   define SQ_CONFIG_DONE# else						/* Sparc/SunOS */#   include <unistd.h>#   define HAS_D_NAMLEN#   undef  HAS_TIMEZONE#   define HAS_ON_EXIT#   define HAS_MSB_FIRST#   undef  HAS_SUN_AUDIO#   define DOUBLE_WORD_ALIGNMENT#   undef  DOUBLE_WORD_ORDER#   define SUN_FUN_KEYS#   define NEED_TZSET#   define JUMP_ALIGN_STRICT#   define SQ_CONFIG_DONE# endif# if !!defined(UNIX)#  define UNIX# endif#endif#if defined(sun) && defined(i386)		/* iX86/Solaris */# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# undef  HAS_D_NAMLEN# define HAS_TIMEZONE# undef  HAS_ON_EXIT# define HAS_LSB_FIRST# undef  HAS_SUN_AUDIO# undef  DOUBLE_WORD_ALIGNMENT# define DOUBLE_WORD_ORDER# define NEED_FILIO# define NEED_TZSET# define JUMP_ALIGN_BYTE# define SQ_CONFIG_DONE# if !!defined(UNIX)#  define UNIX# endif#endif#if defined(mips) || defined(__mips)# if defined(_SYSTYPE_SVR4)			/* (SGI)/IRIX */#  if defined(SQ_CONFIG_DONE)#    error configuration conflict#  endif#  undef  HAS_D_NAMLEN#  define HAS_TIMEZONE#  undef  HAS_ON_EXIT#  define HAS_MSB_FIRST#  undef  HAS_SUN_AUDIO#  define  DOUBLE_WORD_ALIGNMENT#  undef  DOUBLE_WORD_ORDER#  define JUMP_ALIGN_STRICT#  define SQ_CONFIG_DONE#  if !!defined(unix)#   define unix#  endif# endif#endif#if defined(linux)# if defined(i386)				/* iX86/Linux */#   if defined(SQ_CONFIG_DONE)#     error configuration conflict#   endif#   undef  HAS_D_NAMLEN#   define HAS_TIMEZONE#   undef  HAS_ON_EXIT#   define HAS_LSB_FIRST#   undef  HAS_SUN_AUDIO#   undef  DOUBLE_WORD_ALIGNMENT#   define DOUBLE_WORD_ORDER#   define NEED_TZSET#   define JUMP_ALIGN_BYTE#  define SQ_CONFIG_DONE# endif# if defined(powerpc)				/* PPC/Linux */#   if defined(SQ_CONFIG_DONE)#     error configuration conflict#   endif#   undef  HAS_D_NAMLEN#   define HAS_TIMEZONE#   undef  HAS_ON_EXIT#   define HAS_MSB_FIRST#   undef  HAS_SUN_AUDIO#   undef  DOUBLE_WORD_ALIGNMENT#   undef  DOUBLE_WORD_ORDER#   define NEED_TZSET#   define SQ_CONFIG_DONE# endif# if defined(arm)				/* SA110/Linux, maybe Itsy */#   if defined(SQ_CONFIG_DONE)#     error configuration conflict#   endif#   undef  HAS_D_NAMLEN#   define HAS_TIMEZONE#   undef  HAS_ON_EXIT#   define HAS_LSB_FIRST#   undef  HAS_SUN_AUDIO#   undef  DOUBLE_WORD_ALIGNMENT#   undef  DOUBLE_WORD_ORDER#   define NEED_TZSET#   define SQ_CONFIG_DONE# endif# if !!defined(UNIX)#  define UNIX# endif#endif#if defined(__FreeBSD__)# if defined(i386)				/* iX86/FreeBSD */#   if defined(SQ_CONFIG_DONE)#     error configuration conflict#   endif#   undef  HAS_D_NAMLEN#   undef  HAS_TIMEZONE#   undef  HAS_ON_EXIT#   define HAS_LSB_FIRST#   undef  HAS_SUN_AUDIO#   undef  DOUBLE_WORD_ALIGNMENT#   define DOUBLE_WORD_ORDER#   define JUMP_ALIGN_BYTE#   define SQ_CONFIG_DONE# endif# if !!defined(UNIX)#  define UNIX# endif#endif#if defined(__alpha)				/* Alpha/OSF1 */# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# define HAS_D_NAMLEN# undef  HAS_TIMEZONE# undef  HAS_ON_EXIT# define HAS_LSB_FIRST# undef  HAS_SUN_AUDIO# define DOUBLE_WORD_ALIGNMENT# define DOUBLE_WORD_ORDER# define NEED_TZSET# define SQ_CONFIG_DONE# if !!defined(UNIX)#  define UNIX# endif#endif#if defined(hpux) || defined (__hpux)		/* HPPA/HP-UX */# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# define HAS_D_NAMLEN# define HAS_TIMEZONE# undef  HAS_ON_EXIT# define HAS_MSB_FIRST# undef  HAS_SUN_AUDIO# define DOUBLE_WORD_ALIGNMENT# undef  DOUBLE_WORD_ORDER# define NEED_TZSET  extern int h_errno;	/* loser!! */# define SQ_CONFIG_DONE# if !!defined(UNIX)#  define UNIX# endif#endif#if defined(_AIX) || defined(_M_UNIX)# if defined(SQ_CONFIG_DONE)#   error configuration conflict# endif# undef  HAS_D_NAMLEN				/* IBM RS6000/AIX */# define HAS_TIMEZONE# undef  HAS_ON_EXIT# define HAS_MSB_FIRST# undef  HAS_SUN_AUDIO# undef  DOUBLE_WORD_ALIGNMENT# undef  DOUBLE_WORD_ORDER# define NEED_SELECT# define SQ_CONFIG_DONE# if !!defined(UNIX)#  define UNIX# endif#endif#if !!defined(SQ_CONFIG_DONE)# error test for, and describe, your architecture here.#endif'! !!InterpreterSupportCode class methodsFor: 'source files' stamp: 'jm 10/13/1999 18:34'!squeakHeaderFile	^ '#include <math.h>#include <stdio.h>#include <stdlib.h>#include <string.h>#include <time.h>#include "sqConfig.h"#include "sqVirtualMachine.h"#define true 1#define false 0#define null 0  /* using "null" because nil is predefined in Think C *//* pluggable primitives macros *//* Note: All pluggable primitives are defined as	EXPORT(int) somePrimitive(void)   If the platform requires special declaration modifiers   the EXPORT macro can be redefined*/#define EXPORT(returnType) returnType/* image save/restore macros *//* Note: The image file save and restore code uses these macros; they   can be redefined in sqPlatformSpecific.h if desired. These default   versions are defined in terms of the ANSI Standard C libraries.*/#define sqImageFile FILE *#define sqImageFileClose(f)                  fclose(f)#define sqImageFileOpen(fileName, mode)      fopen(fileName, mode)#define sqImageFilePosition(f)               ftell(f)#define sqImageFileRead(ptr, sz, count, f)   fread(ptr, sz, count, f)#define sqImageFileSeek(f, pos)              fseek(f, pos, SEEK_SET)#define sqImageFileWrite(ptr, sz, count, f)  fwrite(ptr, sz, count, f)#define sqAllocateMemory(minHeapSize, desiredHeapSize)   malloc(desiredHeapSize)/* platform-dependent float conversion macros *//* Note: Second argument must be a variable name, not an expression!! *//* Note: Floats in image are always in PowerPC word order; change   these macros to swap words if necessary. This costs no extra and   obviates sometimes having to word-swap floats when reading an image.*/#if defined(DOUBLE_WORD_ALIGNMENT) || defined(DOUBLE_WORD_ORDER)# ifdef DOUBLE_WORD_ORDER/* word-based copy with swapping for non-PowerPC order */#   define storeFloatAtfrom(i, floatVarName) \	*((int *) (i) + 0) = *((int *) &(floatVarName) + 1); \	*((int *) (i) + 1) = *((int *) &(floatVarName) + 0);#   define fetchFloatAtinto(i, floatVarName) \	*((int *) &(floatVarName) + 0) = *((int *) (i) + 1); \	*((int *) &(floatVarName) + 1) = *((int *) (i) + 0);# else /*!!DOUBLE_WORD_ORDER*//* word-based copy for machines with alignment restrictions */#   define storeFloatAtfrom(i, floatVarName) \	*((int *) (i) + 0) = *((int *) &(floatVarName) + 0); \	*((int *) (i) + 1) = *((int *) &(floatVarName) + 1);#   define fetchFloatAtinto(i, floatVarName) \	*((int *) &(floatVarName) + 0) = *((int *) (i) + 0); \	*((int *) &(floatVarName) + 1) = *((int *) (i) + 1);# endif /*!!DOUBLE_WORD_ORDER*/#else /*!!(DOUBLE_WORD_ORDER||DOUBLE_WORD_ALIGNMENT)*//* for machines that allow doubles to be on any word boundary */# define storeFloatAtfrom(i, floatVarName) \	*((double *) (i)) = (floatVarName);# define fetchFloatAtinto(i, floatVarName) \	(floatVarName) = *((double *) (i));#endif/* platform-dependent memory size adjustment macro *//* Note: This macro can be redefined to allows platforms with a   fixed application memory partition (notably, the Macintosh)   to reserve extra C heap memory for special applications that need   it (e.g., for a 3D graphics library). Since most platforms can   extend their application memory partition at run time if needed,   this macro is defined as a noop here and redefined if necessary   in sqPlatformSpecific.h.*/#define reserveExtraCHeapBytes(origHeapSize, bytesToReserve) origHeapSize/* platform-dependent millisecond clock macros *//* Note: The Squeak VM uses three different clocks functions for   timing. The primary one, ioMSecs(), is used to implement Delay   and Time millisecondClockValue. The resolution of this clock   determines the resolution of these basic timing functions. For   doing real-time control of music and MIDI, a clock with resolution   down to one millisecond is preferred, but a coarser clock (say,   1/60th second) can be used in a pinch. The VM calls a different   clock function, ioLowResMSecs(), in order to detect long-running   primitives. This function must be inexpensive to call because when   a Delay is active it is polled twice per primitive call. On several   platforms (Mac, Win32), the high-resolution system clock used in   ioMSecs() would incur enough overhead in this case to slow down the   the VM significantly. Thus, a cheaper clock with low resolution is   used to implement ioLowResMSecs() on these platforms. Finally, the   function ioMicroMSecs() is used only to collect timing statistics   for the garbage collector and other VM facilities. (The function   name is meant to suggest that the function is based on a clock   with microsecond accuracy, even though the times it returns are   in units of milliseconds.) This clock must have enough precision to   provide accurate timings, and normally isn''t called frequently   enough to slow down the VM. Thus, it can use a more expensive clock   that ioMSecs(). By default, all three clock functions are defined   here as macros based on the standard C library function clock().   Any of these macros can be overridden in sqPlatformSpecific.h.*/int ioMSecs(void);int ioLowResMSecs(void);int ioMicroMSecs(void);#define ioMSecs()		((1000 * clock()) / CLOCKS_PER_SEC)#define ioLowResMSecs()	((1000 * clock()) / CLOCKS_PER_SEC)#define ioMicroMSecs()	((1000 * clock()) / CLOCKS_PER_SEC)/* this include file may redefine earlier definitions and macros: */#include "sqPlatformSpecific.h"/* squeak file record; see sqFilePrims.c for details */typedef struct {	FILE	*file;	int		sessionID;	int		writable;	int		fileSize;	int		lastOp;  /* 0 = uncommitted, 1 = read, 2 = write */} SQFile;/* file i/o */int sqFileAtEnd(SQFile *f);int sqFileClose(SQFile *f);int sqFileDeleteNameSize(int sqFileNameIndex, int sqFileNameSize);int sqFileGetPosition(SQFile *f);int sqFileInit(void);int sqFileOpen(SQFile *f, int sqFileNameIndex, int sqFileNameSize, int writeFlag);int sqFileReadIntoAt(SQFile *f, int count, int byteArrayIndex, int startIndex);int sqFileRenameOldSizeNewSize(int oldNameIndex, int oldNameSize, int newNameIndex, int newNameSize);int sqFileSetPosition(SQFile *f, int position);int sqFileSize(SQFile *f);int sqFileValid(SQFile *f);int sqFileWriteFromAt(SQFile *f, int count, int byteArrayIndex, int startIndex);/* directories */int dir_Create(char *pathString, int pathStringLength);int dir_Delete(char *pathString, int pathStringLength);int dir_Delimitor(void);int dir_Lookup(char *pathString, int pathStringLength, int index,	/* outputs: */	char *name, int *nameLength, int *creationDate, int *modificationDate,	int *isDirectory, int *sizeIfFile);int dir_PathToWorkingDir(char *pathName, int pathNameMax);int dir_SetMacFileTypeAndCreator(char *filename, int filenameSize, char *fType, char *fCreator);/* interpreter entry points */void error(char *s);int checkedByteAt(int byteAddress);int checkedByteAtput(int byteAddress, int byte);int checkedLongAt(int byteAddress);int checkedLongAtput(int byteAddress, int a32BitInteger);int fullDisplayUpdate(void);int initializeInterpreter(int bytesToShift);int interpret(void);int primitiveFail(void);int signalSemaphoreWithIndex(int index);int success(int);/* display, mouse, keyboard, time i/o */int ioBeep(void);int ioExit(void);int ioForceDisplayUpdate(void);int ioFormPrint(	int bitsAddr, int width, int height, int depth,	double hScale, double vScale, int landscapeFlag);int ioSetFullScreen(int fullScreen);int ioGetButtonState(void);int ioGetKeystroke(void);int ioMousePoint(void);int ioPeekKeystroke(void);int ioProcessEvents(void);int ioRelinquishProcessorForMicroseconds(int microSeconds);int ioScreenSize(void);int ioSeconds(void);int ioSetCursor(int cursorBitsIndex, int offsetX, int offsetY);int ioSetCursorWithMask(int cursorBitsIndex, int cursorMaskIndex, int offsetX, int offsetY);int ioShowDisplay(	int dispBitsIndex, int width, int height, int depth,	int affectedL, int affectedR, int affectedT, int affectedB);int ioHasDisplayDepth(int depth);int ioSetDisplayMode(int width, int height, int depth, int fullscreenFlag);/* image file and VM path names */extern char imageName[];int imageNameGetLength(int sqImageNameIndex, int length);int imageNamePutLength(int sqImageNameIndex, int length);int imageNameSize(void);int vmPathSize(void);int vmPathGetLength(int sqVMPathIndex, int length);/* save/restore *//* Read the image from the given file starting at the given image offset */int readImageFromFileHeapSizeStartingAt(sqImageFile f, int desiredHeapSize, int imageOffset);/* NOTE: The following is obsolete - it is only provided for compatibility */#define readImageFromFileHeapSize(f, s) readImageFromFileHeapSizeStartingAt(f,s,0)/* clipboard (cut/copy/paste) */int clipboardSize(void);int clipboardReadIntoAt(int count, int byteArrayIndex, int startIndex);int clipboardWriteFromAt(int count, int byteArrayIndex, int startIndex);/* sound output */int snd_AvailableSpace(void);int snd_InsertSamplesFromLeadTime(int frameCount, int srcBufPtr, int samplesOfLeadTime);int snd_PlaySamplesFromAtLength(int frameCount, int arrayIndex, int startIndex);int snd_PlaySilence(void);int snd_Start(int frameCount, int samplesPerSec, int stereo, int semaIndex);int snd_Stop(void);/* sound input */int snd_SetRecordLevel(int level);int snd_StartRecording(int desiredSamplesPerSec, int stereo, int semaIndex);int snd_StopRecording(void);double snd_GetRecordingSampleRate(void);int snd_RecordSamplesIntoAtLength(int buf, int startSliceIndex, int bufferSizeInBytes);/* joystick support */int joystickInit(void);int joystickRead(int stickIndex);/* browser plug-in support */int plugInAllowAccessToFilePath(char *pathString, int pathStringLength);int plugInInit(char *imageName);int plugInShutdown(void);int plugInInterpretCycles(int cycleCount);/* interpreter entry points needed by compiled primitives */void * arrayValueOf(int arrayOop);int checkedIntegerValueOf(int intOop);void * fetchArrayofObject(int fieldIndex, int objectPointer);double fetchFloatofObject(int fieldIndex, int objectPointer);int fetchIntegerofObject(int fieldIndex, int objectPointer);double floatValueOf(int floatOop);int pop(int nItems);int pushInteger(int integerValue);int sizeOfSTArrayFromCPrimitive(void *cPtr);int storeIntegerofObjectwithValue(int fieldIndex, int objectPointer, int integerValue);/* sound generation primitives (old, for backward compatibility) */int primWaveTableSoundmixSampleCountintostartingAtpan(void);int primFMSoundmixSampleCountintostartingAtpan(void);int primPluckedSoundmixSampleCountintostartingAtpan(void);int primSampledSoundmixSampleCountintostartingAtpan(void);int oldprimSampledSoundmixSampleCountintostartingAtleftVolrightVol(void);/* sound generation primitives */int primFMSoundmixSampleCountintostartingAtleftVolrightVol(void);int primLoopedSampledSoundmixSampleCountintostartingAtleftVolrightVol(void);int primPluckedSoundmixSampleCountintostartingAtleftVolrightVol(void);int primReverbSoundapplyReverbTostartingAtcount(void);int primSampledSoundmixSampleCountintostartingAtleftVolrightVol(void);/* squeak socket record; see sqMacNetwork.c for details */typedef struct {	int		sessionID;	int		socketType;  /* 0 = TCP, 1 = UDP */	void	*privateSocketPtr;}  SQSocket, *SocketPtr;/* networking primitives */int		sqNetworkInit(int resolverSemaIndex);void	sqNetworkShutdown(void);void	sqResolverAbort(void);void	sqResolverAddrLookupResult(char *nameForAddress, int nameSize);int		sqResolverAddrLookupResultSize(void);int		sqResolverError(void);int		sqResolverLocalAddress(void);int		sqResolverNameLookupResult(void);void	sqResolverStartAddrLookup(int address);void	sqResolverStartNameLookup(char *hostName, int nameSize);int		sqResolverStatus(void);void	sqSocketAbortConnection(SocketPtr s);void	sqSocketCloseConnection(SocketPtr s);int		sqSocketConnectionStatus(SocketPtr s);void	sqSocketConnectToPort(SocketPtr s, int addr, int port);void	sqSocketCreateNetTypeSocketTypeRecvBytesSendBytesSemaID(			SocketPtr s, int netType, int socketType,			int recvBufSize, int sendBufSize, int semaIndex);void	sqSocketDestroy(SocketPtr s);int		sqSocketError(SocketPtr s);void	sqSocketListenOnPort(SocketPtr s, int port);int		sqSocketLocalAddress(SocketPtr s);int		sqSocketLocalPort(SocketPtr s);int		sqSocketReceiveDataAvailable(SocketPtr s);int		sqSocketReceiveDataBufCount(SocketPtr s, int buf, int bufSize);int		sqSocketRemoteAddress(SocketPtr s);int		sqSocketRemotePort(SocketPtr s);int		sqSocketSendDataBufCount(SocketPtr s, int buf, int bufSize);int		sqSocketSendDone(SocketPtr s);/* 	ar 7/16/1999: New primitives for accept().	Note: If accept() calls are not supported simply make the calls fail	and the old connection style will be used */void	sqSocketListenOnPortBacklogSize(SocketPtr s, int port, int backlogSize);void	sqSocketAcceptFromRecvBytesSendBytesSemaID(			SocketPtr s, SocketPtr serverSocket,			int recvBufSize, int sendBufSize, int semaIndex);/* profiling */int clearProfile(void);int dumpProfile(void);int startProfiling(void);int stopProfiling(void);/* system attributes */int attributeSize(int id);int getAttributeIntoLength(int id, int byteArrayIndex, int length);/* miscellaneous primitives */int primBitmapcompresstoByteArray(void);int primBitmapdecompressfromByteArrayat(void);int primSampledSoundconvert8bitSignedFromto16Bit(void);int primStringcomparewithcollated(void);int primStringfindFirstInStringinSetstartingAt(void);int primStringfindSubstringinstartingAtmatchTable(void);int primStringindexOfAsciiinStringstartingAt(void);int primStringtranslatefromtotable(void);/* serial port primitives */int serialPortClose(int portNum);int serialPortOpen(  int portNum, int baudRate, int stopBitsType, int parityType, int dataBits,  int inFlowCtrl, int outFlowCtrl, int xOnChar, int xOffChar);int serialPortReadInto(int portNum, int count, int bufferPtr);int serialPortWriteFrom(int portNum, int count, int bufferPtr);/* MIDI primitives */int sqMIDIGetClock(void);int sqMIDIGetPortCount(void);int sqMIDIGetPortDirectionality(int portNum);int sqMIDIGetPortName(int portNum, int namePtr, int length);int sqMIDIClosePort(int portNum);int sqMIDIOpenPort(int portNum, int readSemaIndex, int interfaceClockRate);int sqMIDIParameter(int whichParameter, int modify, int newValue);int sqMIDIPortReadInto(int portNum, int count, int bufferPtr);int sqMIDIPortWriteFromAt(int portNum, int count, int bufferPtr, int time);/*** Experimental Asynchronous File I/O ***/typedef struct {	int			sessionID;	void		*state;} AsyncFile;int asyncFileClose(AsyncFile *f);int asyncFileOpen(AsyncFile *f, int fileNamePtr, int fileNameSize, int writeFlag, int semaIndex);int asyncFileRecordSize();int asyncFileReadResult(AsyncFile *f, int bufferPtr, int bufferSize);int asyncFileReadStart(AsyncFile *f, int fPosition, int count);int asyncFileWriteResult(AsyncFile *f);int asyncFileWriteStart(AsyncFile *f, int fPosition, int bufferPtr, int bufferSize);/*** pluggable primitive support ***/int ioLoadExternalFunctionOfLengthFromModuleOfLength(  int functionNameIndex, int functionNameLength,  int moduleNameIndex, int moduleNameLength);/*** sound compression primitives ***/int primADPCMCodecprivateDecodeMono(void);int primADPCMCodecprivateDecodeStereo(void);int primADPCMCodecprivateEncodeMono(void);int primADPCMCodecprivateEncodeStereo(void);/*** tablet support ***/int tabletGetParameters(int cursorIndex, int result[]);int tabletRead(int cursorIndex, int result[]);int tabletResultSize(void);'! !!InterpreterSupportCode class methodsFor: 'source files' stamp: 'jm 10/13/1999 15:47'!squeakPlatSpecFile	^ '/* sqPlatformSpecific.h -- Platform-specific prototypes and definitions *//* How to use this file:   This file is for general platform-specific macros and declarations.   Function prototypes that are unlikely to introduce name conflicts on   other platforms can be added directly. Macro re-definitions or conflicting   function prototypes can be wrapped in a #ifdefs. Alternatively, a customized   version of this file can be used on that platform. The goal is to keep all   the other header files generic across platforms. To override a definition or   macro from sq.h, you must first #undef it, then provide the new definition.*/#ifdef UNIX/* unix-specific prototypes and definitions */void aioPollForIO(int microSeconds, int extraFd);#define SQ_FORM_FILENAME	"squeak-form.ppm"/* undefine clock macros that are implemented as functions */#undef ioMSecs#undef ioMicroMSecs#endif /* UNIX */#ifdef macintosh/* macintosh memory allocation */#include <Memory.h>#undef sqAllocateMemory#define sqAllocateMemory(minHeapSize, desiredHeapSize) NewPtr(desiredHeapSize)/* replace the image file manipulation macros with functions */#undef sqImageFile#undef sqImageFileClose#undef sqImageFileOpen#undef sqImageFilePosition#undef sqImageFileRead#undef sqImageFileSeek#undef sqImageFileWritetypedef int sqImageFile;void        sqImageFileClose(sqImageFile f);sqImageFile sqImageFileOpen(char *fileName, char *mode);int         sqImageFilePosition(sqImageFile f);int         sqImageFileRead(void *ptr, int elementSize, int count, sqImageFile f);void        sqImageFileSeek(sqImageFile f, int pos);int         sqImageFileWrite(void *ptr, int elementSize, int count, sqImageFile f);/* override reserveExtraCHeapBytes() macro to reduce Squeak object heap size on Mac */#undef reserveExtraCHeapBytes#define reserveExtraCHeapBytes(origHeapSize, bytesToReserve) (origHeapSize - bytesToReserve)/* undefine clock macros that are implemented as functions */#undef ioMSecs#undef ioMicroMSecs#endif /* macintosh */#ifdef ACORN/* acorn memory allocation */#undef sqAllocateMemory#define sqAllocateMemory(minHeapSize, desiredHeapSize) platAllocateMemory(desiredHeapSize)#undef sqFilenameFromString#define sqFilenameFromString(dst, src, num) sqFilenameFromString(dst, src, num)/* undefine clock macros that are implemented as functions */#undef ioMicroMSecs#endif /* ACORN */#ifdef WIN32/* Override necessary definitions */#undef putchar#include "sqWin32Alloc.h"#ifdef WIN32_FILE_SUPPORT#undef sqImageFile#undef sqImageFileClose#undef sqImageFileOpen#undef sqImageFilePosition#undef sqImageFileRead#undef sqImageFileSeek#undef sqImageFileWrite#define sqImageFile unsigned longint sqImageFileClose(sqImageFile h);sqImageFile sqImageFileOpen(char *fileName, char *mode);int sqImageFilePosition(sqImageFile h);int sqImageFileRead(void *ptr, int sz, int count, sqImageFile h);int sqImageFileSeek(sqImageFile h, int pos);int sqImageFileWrite(void *ptr, int sz, int count, sqImageFile h);#endif /* WIN32_FILE_SUPPORT *//* pluggable primitive support */#ifdef _MSC_VER#  undef EXPORT#  define EXPORT(returnType) __declspec( dllexport ) returnType#endif /* undefine clock macros that are implemented as functions */#undef ioMSecs#undef ioLowResMSecs#undef ioMicroMSecs/* Declare GetTickCount() in case <windows.h> is not included */#ifndef _WINDOWS___declspec(dllimport) unsigned long __stdcall GetTickCount(void);#endif#define ioLowResMSecs() GetTickCount()#endif /* WIN32 */'! !