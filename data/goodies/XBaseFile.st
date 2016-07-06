"      NAME	XBaseFile
       AUTHOR	criter@tin.it (Antonio d'Avino - NAPLES (ITALY))
       URL	(none)
       FUNCTION	Creating and accessing xBase file (DBIII/IV, Clipper etc.)
       KEYWORDS	DBase sDBase DBIII DBIV CLIPPER
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	1.00
       DATE	16-Apr-98

SUMMARY

-----------------------A pure Smalltalk class giving a full access to xDBase files.  ---------------------------				Fully compatible with CLIPPER(TM) 5.xx and DB III/IV (TM) Files.									  V. 1.00  Feb 13, 1998.     				  Please, feel free to post your messages to andavino@tin.it.										NEXT STEPS* Index management* Visual xBase files maintenance tool													...... stay tuned !-----------------------A pure Smalltalk class giving a full access to xDBase files.  ---------------------------				Fully compatible with CLIPPER(TM) 5.xx and DB III/IV (TM) Files.									  V. 1.00  Feb 13, 1998.     				  Please, feel free to post your messages to criter@tin.it										NEXT STEPS* Index management* Visual xBase files maintenance tool

				Antonio d'Avino - NAPLES (ITALY)
"!
'From Squeak 1.3 of Jan 16, 1998 on 13 February 1998 at 12:30:10 pm'!
Object subclass: #XBaseFile
	instanceVariableNames: 'filterArray lastUpdate setDelete records fileName fileStream headerSize recordSize currentRecord deleted updated headerUpdated eof bof fieldsInfos fieldsContents version '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DataBase-Access'!
!XBaseFile commentStamp: 'AD 2/13/98 12:30' prior: 0!
-----------------------A pure Smalltalk class giving a full access to xDBase files.  ---------------------------
				Fully compatible with CLIPPER(TM) 5.xx and DB III/IV (TM) Files.
									  V. 1.00  Feb 13, 1998. 
    				  Please, feel free to post your messages to andavino@tin.it.


										NEXT STEPS

* Index management
* Visual xBase files maintenance tool

													...... stay tuned !!!


!XBaseFile methodsFor: 'Exiting' stamp: 'AD 2/13/98 11:20'!
close
	
	" Close xDBase file flushing last updates and release object "
	updated ifTrue: [ self saveRecord ].
	headerUpdated ifTrue: [ self saveHeaderInfo ].
	fileStream close.
!
]style[(71 106)f1b,f1! !


!XBaseFile methodsFor: 'Testing' stamp: 'AD 2/5/98 11:01'!
bof
	"test a beginning of file condition"

	^bof! !

!XBaseFile methodsFor: 'Testing' stamp: 'AD 2/5/98 11:02'!
eof

	"test an end of file condition"

	^eof!
]style[(38 7)f1b,f1! !

!XBaseFile methodsFor: 'Testing' stamp: 'AD 2/5/98 11:02'!
isDBFFile

	"tests if the open file is a .DBF file (signature byte=03)"

	fileStream position: 0.
	^(fileStream next asInteger = 3)
    

	! !

!XBaseFile methodsFor: 'Testing' stamp: 'AD 2/5/98 11:02'!
isDeleted

	"test a deleted condition for current record"

	^deleted!
]style[(58 10)f1b,f1! !

!XBaseFile methodsFor: 'Testing' stamp: 'AD 2/5/98 11:03'!
isUpdated

	" the currend record has been updated (return true) or not (return false) ?"
	^updated!
]style[(88 10)f1b,f1! !

!XBaseFile methodsFor: 'Testing' stamp: 'AD 2/13/98 12:12'!
version

	"answer current package version"

	^ version! !


!XBaseFile methodsFor: 'Positioning' stamp: 'AD 2/5/98 11:11'!
goBottom

	"set last record in file as current record. Deleted and Filter settings are influent"

	self goto: records;
		 skip: 0 direction: -1! !

!XBaseFile methodsFor: 'Positioning' stamp: 'AD 2/5/98 11:12'!
goto: rNumber

	"position file on record number rNumber. Deleted and Filter settings are NOT influent"

	| recordNumber |
     eof_false.
	bof_false.
	updated ifTrue: [ self saveRecord ].
	(rNumber<1) ifTrue: [recordNumber_1. bof_true. ]
				ifFalse: [recordNumber_rNumber].
	(recordNumber<=records) ifTrue: [self gotoRecord: (currentRecord_recordNumber).
									 self getRecord.
									]
							ifFalse: [ currentRecord_records+1.
									  eof_true.
									  self blankRecord.
									  deleted _ false.
									]
									! !

!XBaseFile methodsFor: 'Positioning' stamp: 'AD 2/5/98 11:11'!
goTop

	"set first record in file as current record. Deleted and Filter settings are influent"

	self goto: 1;
		 skip: 0 direction: 1! !

!XBaseFile methodsFor: 'Positioning' stamp: 'AD 2/5/98 11:13'!
skip

	"set next record in file as current record. Deleted and Filter settings are influent"
	^self skip: 1 direction: 1! !

!XBaseFile methodsFor: 'Positioning' stamp: 'AD 2/5/98 11:15'!
skip: aNumber

	"jump to next aNumber record. Jump maybe forward (aNumber > 0) or backward (aNumber <0). Deleted and Filter settings are influent."

	self skip: aNumber direction: ((aNumber abs) / aNumber)! !


!XBaseFile methodsFor: 'Initializing' stamp: 'AD 2/13/98 11:24'!
open
	"Open an existing .DBF file. This methods is called by class methods  new:  and 
	new:fields:. May be called directly to reopen a closed file"

	self setVersion.
     (StandardFileStream isAFileNamed: fileName) 
		ifFalse: [ ^self error: fileName, ': no such .DBF file' ]
		ifTrue: [ fileStream_StandardFileStream fileNamed: fileName.
     			 self isDBFFile 
					ifFalse: [ fileStream close.
							  ^ self error: fileName, ': is not a .DBF file' ]
					ifTrue: [updated _ false.
							setDelete_false.
							self getDBFInfos.
							self goto: 1.
							^self
						    ].
				]
					

	! !


!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 13:22'!
append

	"append a new, blank record"

	updated ifTrue: [ self saveRecord ].
	(records > 0) ifTrue: [self gotoRecord: records.
						 fileStream skip: recordSize.
						]
				ifFalse: [fileStream position: headerSize].
	records_records+1.
	currentRecord_records.
	self blankRecord;
		 saveRecord.
	headerUpdated_true.
	
	
	! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 11:22'!
at: fieldName

  "answer current content of field. Field name maybe a string or a symbol. It returns nil if fieldName is not in file. fieldName is case insenitive"
  ^self unformatField: fieldName! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 11:22'!
at: fieldName put: fieldValue

	" set field  fieldName value to fieldValue. Field name may be a string or a symbol. 
	  fieldValue MUST be of the correct type for the field. The methods returns current
	  field value or nil if fieldName is no in file. fieldName is case insensitive"

	| old  new |

	old_self unformatField: fieldName.
     new_self formatField: fieldName value: fieldValue.
	old isNil ifFalse: [ updated _ true. fieldsContents at: (fieldName asString asUppercase) put: new ].
	^old
	! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 11:23'!
delete

	"mark as deleted current record"

	deleted ifFalse: [ deleted_true. updated_true. ]! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 12:11'!
fieldDec: fieldName

	"returns  field decimal positions for field named fieldName 	(string or symbol, case  	insensitive). If field is not numeric this datum is not  	significant and usually 	zero. Method  returns nil if fieldName is not in file"

	| answ |
	answ_fieldsInfos at: (fieldName asString asUppercase) ifAbsent: [ nil ].
	^ answ isNil ifTrue: [ nil ] ifFalse: [ answ at: 2 ]

	! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 12:15'!
fieldInfos: fieldName

	"returns an array three items sized, containing infos for field named fieldName 	      	 (string or symbol, case insensitive).. First array item is
	 the field type, second the field length, third field decimal positions, fourth is
	 the field offset inside record. Method	 returns nil if fieldName is not in file"
	
	^fieldsInfos at: (fieldName asString asUppercase) ifAbsent: [ nil ].

	! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 12:16'!
fieldInfosDictionary

	"returns the field infos dictionary. Any key in dictionary is a field name, value  	is an array four items sized, containing field infos. . First array item is
	 the field type, second the field length, third field decimal positions, fourth is
	the field offset inside record."
	
	^fieldsInfos .

	! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 12:05'!
fieldLen: fieldName

	"returns field length for field named fieldName (string or
	 symbol, case insensitive). Method returns nil if fieldName is not in file"
	| answ |
	answ_fieldsInfos at: (fieldName asString asUppercase) ifAbsent: [ nil ].
	^ answ isNil ifTrue: [ nil ] ifFalse: [ answ at: 2 ]

	! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 12:02'!
fieldType: fieldName

	"returns a character describing field type for field named fieldName (string or
	 symbol, case insensitive). Character $C stands for Character field, $N for 
	 Numeric field, $L for logical field, $D for date field. Returns nil if fieldName
	 is not in file"
	| answ |
	answ_fieldsInfos at: (fieldName asString asUppercase) ifAbsent: [ nil ].
	^ answ isNil ifTrue: [ nil ] ifFalse: [ answ at: 1 ]

	! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 12:31'!
record

	"return current record number"
	^currentRecord! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 12:31'!
records

	"return records in file"
	^records! !

!XBaseFile methodsFor: 'Accessing' stamp: 'AD 2/5/98 11:23'!
restore

	"unmark as deleted current record"
	deleted ifTrue: [deleted_false.updated_true]! !


!XBaseFile methodsFor: 'Private' stamp: 'AD 1/30/98 17:24'!
blankRecord

	"put empty contents into field contents collection"
	
	| tp k val |

	updated _ false.
	fieldsInfos associationsDo: [ : a  | tp_a value at: 1.
						  k_a key.
                              (tp==$C) ifTrue: [val_self formatField: k value: ''].
						 (tp==$N) ifTrue: [val_self formatField: k value: 0].
						 (tp==$L) ifTrue: [val_false].
						 (tp==$D) ifTrue: [val_self formatField: k value: (Date newDay: 1 month: 1 year: 1)].
						 fieldsContents at: k put: val ].
! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/27/98 13:33'!
char2Num: aString
	"converts hex num in aString to integer"
	
	| aNum ix |

	aNum_0.
     ix_1.
   	aString do: [ : a | aNum_aNum + ((a  asInteger) * ix). ix_ix*256].
   	^aNum! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/13/98 12:04'!
create: fDictionary

	| pos  ky |
	self setVersion.
	fileStream_ StandardFileStream newFileNamed: fileName.
	lastUpdate_Date today.
	records_0.
	currentRecord_0.
	updated _ false.
	headerUpdated _ false.
	deleted_false.
	setDelete_false.
	pos_1.
	fieldsContents_Dictionary new.
	fieldsInfos_Dictionary new.
	fDictionary associationsDo: [ : a |  ky_a key asString asUppercase. 
									  ky_ky copyFrom: 1 to: (10 min: (ky size)).
									  (a value) at: 4 put: pos.  
									  fieldsInfos at: ky put: a value.
									  pos_pos+((a value) at:  2).
							   ].
	recordSize_pos.
	headerSize_34+((fieldsInfos size) * 32).
	self createHeaderInfo
	! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/12/98 13:39'!
createHeaderInfo

	"save .DBF file header info's"
     
	headerUpdated_false.
	fileStream 	position: 0;
			    	nextPut: (Character value: 3);
				nextPut: ((self num2Char: ((lastUpdate year) - 1900) size: 1) at: 1);
				nextPut: ((self num2Char: (lastUpdate monthIndex) size: 1) at: 1);
				nextPut: ((self num2Char: (lastUpdate dayOfMonth - 1900) size: 1) at: 1);
				nextPutAll: (self num2Char: records size: 4);
				nextPutAll: (self num2Char: headerSize size: 2);
     			nextPutAll: (self num2Char: recordSize size: 2).
	self saveFieldsInfos.
	fileStream nextPut: (Character value: 13);
				nextPut: (Character value: 0);
				nextPut: (Character value: 26).
     
     
     
! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/30/98 18:38'!
evaluateFilters

	| answ |
	answ_true.
	filterArray do: [ : a | answ_answ and: [ a value: self ]].
	^ answ not! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/30/98 16:48'!
formatField: fieldName value: v

	|  answ k  t l d  i dc is dcs cti sgn comma tmp |
	k_fieldName asString asUppercase.
	t_(fieldsInfos at: k ifAbsent: [ ^ nil ]) at: 1.
	l_(fieldsInfos at: k ) at: 2.
	d_(fieldsInfos at: k ) at: 3.
						 (t == $N) ifTrue: [ 	i_v asInteger.
											is_i abs asString.
											sgn_(i<0) ifTrue: ['-'] ifFalse: [''].
                                                     comma_(d>0) ifTrue: ['.'] ifFalse: [''].
                                                     cti_((tmp_(l - d - (is size) - (sgn size) - (comma size))) > 0) ifTrue: [String new: tmp withAll:'0'] ifFalse: [''].
											(d>0) ifTrue:
											[	dc_(v - i) abs.
												dcs_dc asFloat asString.
												dcs_((dcs copyFrom: 3 to: dcs size), (String new: d withAll: $0)) copyFrom: 1 to: d.
											]
											ifFalse: [ dcs_''].
											answ_(sgn,cti,is,comma,dcs).
											answ_answ copyFrom: ((answ size) - l + 1) to: (answ size)										 ].
						 (t == $L) ifTrue: [answ_v ifTrue: [$T] ifFalse: [$F] ].
						 (t == $D) ifTrue: [ answ_v year asString.
										   tmp_'0',(v monthIndex asString).
										   tmp_tmp copyFrom: (tmp size - 1) to: tmp size.
										   answ_answ,tmp.
										   tmp_'0',(v day asString).
										   tmp_tmp copyFrom: (tmp size - 1) to: tmp size.
										   answ_answ,tmp.
										].
						(t == $C) ifTrue: [ answ_(v, (String new: l withAll: $  )) copyFrom: 1 to: l].
^ answ

! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/28/98 12:07'!
getDBFInfos
	"get all infos about open DBF file"

	self  getHeaderInfo;
		 getFieldsInfos.
	fieldsContents _ Dictionary new.
	^self! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/28/98 12:32'!
getFieldsInfos

	"get field infos on fieldsInfos array"

     | pos name type len dec fpos  |

	pos_32.
     fieldsInfos_Dictionary new.
     fpos_1.
     [ pos < (headerSize - 2)] whileTrue: [fileStream position: pos. name_(fileStream upTo: 0 asCharacter). 
										fileStream position: pos+11. type_fileStream next.
										fileStream skip: 4.
										dec_0.
										( type == $C ) ifTrue:  [len_self char2Num: (fileStream next: 2)]
													  ifFalse:  [len_self char2Num: (fileStream next) asString.
															    dec_self char2Num: (fileStream next) asString.
															   ].
										fieldsInfos at: name put: (Array with: type with: len with: dec with: fpos).
										fpos_fpos+len.
									     pos_pos+32.
   								        ].! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/5/98 12:33'!
getHeaderInfo
	"get .DBF file header info's"
     | d m y |
	headerUpdated_false.
	fileStream position: 1.
     y_(self char2Num: (fileStream next) asString).
     m_(self char2Num: (fileStream next) asString).
     d_(self char2Num: (fileStream next) asString).

     lastUpdate_Date newDay: d month: m year: y.
     records_self char2Num: (fileStream next: 4).
     headerSize_self char2Num: (fileStream next: 2).
     recordSize_self char2Num: (fileStream next: 2).
     
! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/29/98 17:22'!
getRecord

	"get current record contents"

	| tmp str k v |

	tmp_fileStream next: recordSize.
	str_ReadStream on: tmp from: 1 to: recordSize.
	((tmp at: 1) == $  ) ifFalse: [ deleted _ true ]
					   ifTrue: [ deleted _ false ].
	fieldsInfos associationsDo: [ : a |  k_a key.
								  v_a value.
								  str position: (v at: 4).
								  fieldsContents at: k  put: (str next: (v at: 2)).
							].											
				updated _ false.! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/5/98 12:24'!
gotoRecord: recordNumber
	
	"position currentRecord to recordNumber"
     
	((recordNumber <= records) and: [ recordNumber>0]) ifFalse: [ self error: 'Record number outside file bounds' ]
 							  ifTrue: [ fileStream position: headerSize+(((currentRecord_recordNumber) - 1) * recordSize)].
             ! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/5/98 13:19'!
num2Char: aNum size: aSize

	"converts aNum hex num to aString"
	
	| tmp answ |

	 answ_ String new: aSize withAll: $  .
	 tmp_aNum.
   	 1 to: aSize do: [ : ix |  answ at: ix put: (Character value: (tmp \\ 256)). 							tmp_tmp//256 ].
   	^answ!
]style[(28 218)f1b,f1! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/12/98 13:35'!
saveFieldsInfos

	"get field infos on fieldsInfos array"

     | pos  |

	pos_32.
	fileStream position: 32.
     fieldsInfos associationsDo: [ : a |fileStream 	position: pos; 
							   			  		nextPutAll: ((a key asUppercase), (Character value:0) asString);
												position: pos+11;
												nextPut: ((a value) at: 1);
												skip: 4.
												(((a value) at: 1) == $C) ifTrue:
													[  fileStream nextPutAll: (self num2Char: ((a value) at: 2) size:2) ]
																		ifFalse:
													[ fileStream nextPut: ((self num2Char: ((a value) at: 2) size:1) at: 1);
																 nextPut: ((self num2Char: ((a value) at: 3) size:1) at: 1)
													].
												pos_pos+32.
   								        ].
									   fileStream position: pos.! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/13/98 11:19'!
saveHeaderInfo

	"save .DBF file header info's"
     
	headerUpdated_false.
	fileStream position: 4;
			   nextPutAll: (self num2Char: records size: 4).
     
     
! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/29/98 18:38'!
saveRecord

	| str tmp |
	
	self gotoRecord: currentRecord.
	tmp_String new: recordSize withAll: $ .
	deleted ifTrue: [tmp at: 1 put: $* ].
     str_WriteStream on: tmp from: 1 to: recordSize.
	fieldsContents associationsDo: [ : a | str position: ((fieldsInfos at: (a key)) at: 4).
									    str nextPutAll: (fieldsContents at: a key).
                                        ].
	fileStream nextPutAll: tmp.
     updated _ false.! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 1/27/98 12:27'!
setName: aString
     "Set the .DBF file name to aString"
	| ext |

	ext_''.
	(aString includesSubstring: '.' caseSensitive: true)	ifFalse: [ ext_'.DBF' ].
	fileName_(aString, ext)
! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/13/98 12:13'!
setVersion

	version_'XBaseFile Class Package V. 1.00 by Antonio d''Avino andavino@tin.it'! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/5/98 11:10'!
skip: aNumber direction: ofs

	self goto: (currentRecord + aNumber).
	[(((filterArray isNil not and: [self evaluateFilters]) or: [setDelete and: [deleted]]) and: [ eof not]) and: [bof not]] whileTrue: 
				[self goto: currentRecord + ofs ].! !

!XBaseFile methodsFor: 'Private' stamp: 'AD 2/5/98 11:17'!
unformatField: fieldName

	|  k  t answ str1 y m dy |
	k_fieldName asString asUppercase.
	t_(fieldsInfos at: k ifAbsent: [ ^ nil ]) at: 1.
						  (t == $N) ifTrue: [ answ_(fieldsContents at: k) asNumber ].
						 (t == $L) ifTrue: [(answ_(fieldsContents at: k) == $T) ].
						 (t == $D) ifTrue: [ str1_ReadStream on: (fieldsContents at: k) from: 1 to: 8.
										   y_( str1 next: 4) asInteger.
										   m_( str1 next: 2) asInteger.
										   dy_(str1 next: 2) asInteger.
										   answ_Date newDay: dy month: m year: y.
										].
						(t == $C) ifTrue: [ answ_fieldsContents at: k ].
^ answ

! !


!XBaseFile methodsFor: 'Settings' stamp: 'AD 2/5/98 11:24'!
noFilter

	"remove any filter setting for file"
	filterArray_nil! !

!XBaseFile methodsFor: 'Settings' stamp: 'AD 1/30/98 16:57'!
setDeleted

	"inquiry the deleted record filtering status"
	^setDelete! !

!XBaseFile methodsFor: 'Settings' stamp: 'AD 2/5/98 11:25'!
setDeleted: status

	"set (status == true) or reset (status == false) deleted record filter status"

	setDelete_status! !

!XBaseFile methodsFor: 'Settings' stamp: 'AD 2/5/98 11:31'!
setFilter: aBlock

	"set a new filter condition. Any preceding filter condition is lost. aBlock must be a
	 string describing a Smalltalk block, returning  a true or false value. If returned 
	 value is true record is acceptd, if false record is rejected. Current dbFile object may 
	 be referred in aBlock as 'selfDB'. See 'test' class method for a sample of using 
	 setFilter message"

	filterArray_OrderedCollection new.
     filterArray add: (Compiler evaluate: '[ : selfDB | ',(aBlock asString), ']').! !

!XBaseFile methodsFor: 'Settings' stamp: 'AD 2/5/98 11:33'!
setFilterAdditive: aBlock

	"Append a new filter condition. Filter conditions are chained by a logical AND. See
	 remark of setFilter method for a description of aBlock format"

	filterArray isNil ifTrue: [filterArray_OrderedCollection new].
     filterArray add: (Compiler evaluate: '[ : selfDB | ',(aBlock asString), ']').! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

XBaseFile class
	instanceVariableNames: ''!

!XBaseFile class methodsFor: 'Instance Creation' stamp: 'AD 2/5/98 11:35'!
new: fileName

	"create a DBFile object open on an existing .DBF file named fileName. fileName may 	 	 be a string or a symbol."

	^(super basicNew setName: fileName asString) open.
    	! !

!XBaseFile class methodsFor: 'Instance Creation' stamp: 'AD 2/13/98 11:34'!
new: fileName fields: fieldsDictionary

	"create a DBFile object creating a .DBF file named fileName. fileName may be a string or a symbol. 
	FieldsDictionary is a Dictionary whose key is the field name as string and
	the value an Array of 4 dimensions: 1st position is a character indicating the field type
													($C -> Character, $N Numeric, $L Logical, $D Date). 
										2nd  is field length. 
										3rd the field is decimal positions ( is not-significant if field is not Numeric).  
										4th position may be left nil"

	^(super basicNew setName: fileName asString) create: fieldsDictionary.
    	! !


!XBaseFile class methodsFor: 'Class Testing' stamp: 'AD 2/13/98 12:15'!
test 

	"A DBFile class test method"

	| a b |
     b_Dictionary new.
     Transcript showOnNewLine: 'Building new test xBase file ...'.
     b at: 'abc' put: (Array with: $C with: 5 with: 0 with: nil).
	a_XBaseFile new: #test fields: b.
     1 to: 10 do: [ : ix | a append;
	   					at: #abc put: (ix asString)
                  ].
      Transcript show: ' Done'.
      Transcript showOnNewLine: 'Records in file ', ( a records asString).
      Transcript showOnNewLine: 'Setting filter on field ABC. Filter expression ''((selfDB at: #abc) asNumber  == 2) not '' ...'.
	 a setFilter: '((selfDB at: #abc) asNumber  == 2) not '.
      Transcript show: ' Done'.
    
  Transcript showOnNewLine: 'Start 1st test: Skipping forward from top ... '.
	 a goTop.
  
	 [ a eof or: [a bof]] whileFalse: [Transcript showOnNewLine: (a record asString), ' -- ', (a at: #abc) . a skip ].
	 Transcript show: ' Done'.
	 Transcript showOnNewLine: 'Start 2st test: Skipping backward from bottom ... '.
	 a goBottom.
  
	 [ a eof or: [a bof]] whileFalse: [Transcript showOnNewLine: (a record asString), ' -- ', (a at: #abc) . a skip: -1 ].
	 Transcript show: ' Done'.
	 Transcript showOnNewLine: 'Closing xBase test file and exiting ...' .
  	 a close.
	 Transcript show: ' Done'.


		! !

