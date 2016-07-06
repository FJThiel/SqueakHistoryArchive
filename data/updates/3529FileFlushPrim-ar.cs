'From Squeak3.1alpha of 4 February 2001 [latest update: #3527] on 6 February 2001 at 6:00:07 pm'!"Change Set:		FileFlushPrim-arDate:			6 February 2001Author:			Andreas RaabThe file flush primitive. Oh my god, we've been waiting for it soooo long. But now I need it desparately, so here it comes."!!FilePlugin methodsFor: 'file primitives' stamp: 'ar 2/6/2001 17:53'!primitiveFileFlush	| file |	self var: 'file' declareC: 'SQFile *file'.	self export: true.	file _ self fileValueOf: (interpreterProxy stackValue: 0).	interpreterProxy failed ifFalse:[self sqFileFlush: file].	interpreterProxy failed ifFalse: [interpreterProxy pop: 1].! !!FilePlugin class methodsFor: 'translation' stamp: 'ar 2/6/2001 17:54'!headerFile^'/* File support definitions *//* squeak file record; see sqFilePrims.c for details */typedef struct {	FILE	*file;	int		sessionID;	int		writable;	int		fileSize;	int		lastOp;  /* 0 = uncommitted, 1 = read, 2 = write */} SQFile;/* file i/o */int sqFileAtEnd(SQFile *f);int sqFileClose(SQFile *f);int sqFileDeleteNameSize(int sqFileNameIndex, int sqFileNameSize);int sqFileGetPosition(SQFile *f);int sqFileInit(void);int sqFileShutdown(void);int sqFileOpen(SQFile *f, int sqFileNameIndex, int sqFileNameSize, int writeFlag);int sqFileReadIntoAt(SQFile *f, int count, int byteArrayIndex, int startIndex);int sqFileRenameOldSizeNewSize(int oldNameIndex, int oldNameSize, int newNameIndex, int newNameSize);int sqFileSetPosition(SQFile *f, int position);int sqFileSize(SQFile *f);int sqFileValid(SQFile *f);int sqFileWriteFromAt(SQFile *f, int count, int byteArrayIndex, int startIndex);int sqFileFlush(SQFile *f);/* directories */int dir_Create(char *pathString, int pathStringLength);int dir_Delete(char *pathString, int pathStringLength);int dir_Delimitor(void);int dir_Lookup(char *pathString, int pathStringLength, int index,	/* outputs: */	char *name, int *nameLength, int *creationDate, int *modificationDate,	int *isDirectory, int *sizeIfFile);int dir_PathToWorkingDir(char *pathName, int pathNameMax);int dir_SetMacFileTypeAndCreator(char *filename, int filenameSize, char *fType, char *fCreator);int dir_GetMacFileTypeAndCreator(char *filename, int filenameSize, char *fType, char *fCreator);/*** security traps ***//* directory access */int ioCanCreatePathOfSize(char* dirNameIndex, int dirNameSize);int ioCanListPathOfSize(char* dirNameIndex, int dirNameSize);int ioCanDeletePathOfSize(char* dirNameIndex, int dirNameSize);/* file access */int ioCanOpenFileOfSizeWritable(char* fileNameIndex, int fileNameSize, int writeFlag);int ioCanDeleteFileOfSize(char* fileNameIndex, int fileNameSize);int ioCanRenameFileOfSize(char* fileNameIndex, int fileNameSize);int ioCanGetFileTypeOfSize(char* fileNameIndex, int fileNameSize);int ioCanSetFileTypeOfSize(char* fileNameIndex, int fileNameSize);/* top level functions */int ioDisableFileAccess(void);int ioHasFileAccess(void);#ifdef DISABLE_SECURITY#define ioCanCreatePathOfSize(name, size) 1#define ioCanListPathOfSize(name, size) 1#define ioCanDeletePathOfSize(name, size) 1#define ioCanOpenFileOfSizeWritable(name, size, writeFlag) 1#define ioCanDeleteFileOfSize(name, size) 1#define ioCanRenameFileOfSize(name, size) 1#define ioCanGetFileTypeOfSize(name, size) 1#define ioCanSetFileTypeOfSize(name, size) 1#define ioDisableFileAccess() 1#define ioHasFileAccess() 1#endif /* DISABLE_SECURITY */'.! !!FilePluginSimulator methodsFor: 'as yet unclassified' stamp: 'ar 2/6/2001 17:54'!sqFileFlush: file	^interpreterProxy sqFileFlush: file! !!InterpreterSimulator methodsFor: 'file primitives' stamp: 'ar 2/6/2001 17:54'!sqFileFlush: file	^ file flush! !!InterpreterSupportCode class methodsFor: 'source files' stamp: 'ar 2/6/2001 17:56'!squeakFilePrimsFile	^ '#include "sq.h"#ifndef NO_STD_FILE_SUPPORT#include "FilePlugin.h"/***	The state of a file is kept in the following structure,	which is stored directly in a Squeak bytes object.	NOTE: The Squeak side is responsible for creating an	object with enough room to store sizeof(SQFile) bytes.	The session ID is used to detect stale file objects--	files that were still open when an image was written.	The file pointer of such files is meaningless.	Files are always opened in binary mode; Smalltalk code	does (or someday will do) line-end conversion if needed.	Writeable files are opened read/write. The stdio spec	requires that a positioning operation be done when	switching between reading and writing of a read/write	filestream. The lastOp field records whether the last	operation was a read or write operation, allowing this	positioning operation to be done automatically if needed.	typedef struct {		File	*file;		int		sessionID;		int		writable;		int		fileSize;		int		lastOp;  // 0 = uncommitted, 1 = read, 2 = write //	} SQFile;***//*** Constants ***/#define UNCOMMITTED	0#define READ_OP		1#define WRITE_OP	2#ifndef SEEK_SET#define SEEK_SET	0#define SEEK_CUR	1#define SEEK_END	2#endif/*** Variables ***/int thisSession = 0;extern struct VirtualMachine * interpreterProxy;int sqFileAtEnd(SQFile *f) {	/* Return true if the file''s read/write head is at the end of the file. */	if (!!sqFileValid(f)) return interpreterProxy->success(false);	return ftell(f->file) == f->fileSize;}int sqFileClose(SQFile *f) {	/* Close the given file. */	if (!!sqFileValid(f)) return interpreterProxy->success(false);	fclose(f->file);	f->file = NULL;	f->sessionID = 0;	f->writable = false;	f->fileSize = 0;	f->lastOp = UNCOMMITTED;}int sqFileDeleteNameSize(int sqFileNameIndex, int sqFileNameSize) {	char cFileName[1000];	int i, err;	if (sqFileNameSize >= 1000) {		return interpreterProxy->success(false);	}	/* copy the file name into a null-terminated C string */	sqFilenameFromString(cFileName, sqFileNameIndex, sqFileNameSize);	if (!!plugInAllowAccessToFilePath(cFileName, sqFileNameSize)) {		return interpreterProxy->success(false);	}	err = remove(cFileName);	if (err) {		return interpreterProxy->success(false);	}}int sqFileGetPosition(SQFile *f) {	/* Return the current position of the file''s read/write head. */	int position;	if (!!sqFileValid(f)) return interpreterProxy->success(false);	position = ftell(f->file);	if (position < 0) return interpreterProxy->success(false);	return position;}int sqFileInit(void) {	/* Create a session ID that is unlikely to be repeated.	   Zero is never used for a valid session number.	   Should be called once at startup time.	*/	thisSession = clock() + time(NULL);	if (thisSession == 0) thisSession = 1;	/* don''t use 0 */	return 1;}int sqFileShutdown(void) {	return 1;}int sqFileOpen(SQFile *f, int sqFileNameIndex, int sqFileNameSize, int writeFlag) {	/* Opens the given file using the supplied sqFile structure	   to record its state. Fails with no side effects if f is	   already open. Files are always opened in binary mode;	   Squeak must take care of any line-end character mapping.	*/	char cFileName[1001];	int i;	/* don''t open an already open file */	if (sqFileValid(f)) return interpreterProxy->success(false);	/* copy the file name into a null-terminated C string */	if (sqFileNameSize > 1000) {		return interpreterProxy->success(false);	}	sqFilenameFromString(cFileName, sqFileNameIndex, sqFileNameSize);	if (!!plugInAllowAccessToFilePath(cFileName, sqFileNameSize)) {		return interpreterProxy->success(false);	}	if (writeFlag) {		/* First try to open an existing file read/write: */		f->file = fopen(cFileName, "r+b");		if (f->file == NULL) {			/* Previous call fails if file does not exist. In that case,			   try opening it in write mode to create a new, empty file.			*/			f->file = fopen(cFileName, "w+b");			if (f->file !!= NULL) {				/* set the type and creator of newly created Mac files */				dir_SetMacFileTypeAndCreator((char *)sqFileNameIndex, sqFileNameSize, "TEXT", "R*ch");				}		}		f->writable = true;	} else {		f->file = fopen(cFileName, "rb");		f->writable = false;	}	if (f->file == NULL) {		f->sessionID = 0;		f->fileSize = 0;		return interpreterProxy->success(false);	} else {		f->sessionID = thisSession;		/* compute and cache file size */		fseek(f->file, 0, SEEK_END);		f->fileSize = ftell(f->file);		fseek(f->file, 0, SEEK_SET);	}	f->lastOp = UNCOMMITTED;}int sqFileReadIntoAt(SQFile *f, int count, int byteArrayIndex, int startIndex) {	/* Read count bytes from the given file into byteArray starting at	   startIndex. byteArray is the address of the first byte of a	   Squeak bytes object (e.g. String or ByteArray). startIndex	   is a zero-based index; that is a startIndex of 0 starts writing	   at the first byte of byteArray.	*/	char *dst;	int bytesRead;	if (!!sqFileValid(f)) return interpreterProxy->success(false);	if (f->writable && (f->lastOp == WRITE_OP)) fseek(f->file, 0, SEEK_CUR);  /* seek between writing and reading */	dst = (char *) (byteArrayIndex + startIndex);	bytesRead = fread(dst, 1, count, f->file);	f->lastOp = READ_OP;	return bytesRead;}int sqFileRenameOldSizeNewSize(int oldNameIndex, int oldNameSize, int newNameIndex, int newNameSize) {	char cOldName[1000], cNewName[1000];	int i, err;	if ((oldNameSize >= 1000) || (newNameSize >= 1000)) {		return interpreterProxy->success(false);	}	/* copy the file names into null-terminated C strings */	sqFilenameFromString(cOldName, oldNameIndex, oldNameSize);	sqFilenameFromString(cNewName, newNameIndex, newNameSize);	if (!!plugInAllowAccessToFilePath(cOldName, oldNameSize) ||		!!plugInAllowAccessToFilePath(cNewName, newNameSize)) {		return interpreterProxy->success(false);	}	err = rename(cOldName, cNewName);	if (err) {		return interpreterProxy->success(false);	}}int sqFileSetPosition(SQFile *f, int position) {	/* Set the file''s read/write head to the given position. */	if (!!sqFileValid(f)) return interpreterProxy->success(false);	fseek(f->file, position, SEEK_SET);	f->lastOp = UNCOMMITTED;}int sqFileSize(SQFile *f) {	/* Return the length of the given file. */	if (!!sqFileValid(f)) return interpreterProxy->success(false);	return f->fileSize;}int sqFileFlush(SQFile *f) {	/* Return the length of the given file. */	if (!!sqFileValid(f)) return interpreterProxy->success(false);	fflush(f);	return 1;}int sqFileValid(SQFile *f) {	return (		(f !!= NULL) &&		(f->file !!= NULL) &&		(f->sessionID == thisSession));}int sqFileWriteFromAt(SQFile *f, int count, int byteArrayIndex, int startIndex) {	/* Write count bytes to the given writable file starting at startIndex	   in the given byteArray. (See comment in sqFileReadIntoAt for interpretation	   of byteArray and startIndex).	*/	char *src;	int bytesWritten, position;	if (!!(sqFileValid(f) && f->writable)) return interpreterProxy->success(false);	if (f->lastOp == READ_OP) fseek(f->file, 0, SEEK_CUR);  /* seek between reading and writing */	src = (char *) (byteArrayIndex + startIndex);	bytesWritten = fwrite(src, 1, count, f->file);	position = ftell(f->file);	if (position > f->fileSize) {		f->fileSize = position;  /* update file size */	}	if (bytesWritten !!= count) {		interpreterProxy->success(false);	}	f->lastOp = WRITE_OP;	return bytesWritten;}#endif /* NO_STD_FILE_SUPPORT */'! !!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/6/2001 17:58'!primFlush: id	"Flush pending changes to the disk"	| p |	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>	"In some OS's seeking to 0 and back will do a flush"	p _ self position.	self position: 0; position: p! !!StandardFileStream methodsFor: 'read, write, position' stamp: 'ar 2/6/2001 17:59'!flush	"Flush pending changes"	^self primFlush: fileID! !