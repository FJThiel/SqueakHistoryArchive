'From Squeak3.3alpha of 15 February 2002 [latest update: #4839] on 20 April 2002 at 9:25:01 am'!"Change Set:		HTMLTable2-tkDate:			11 April 2002Author:			Ted KaehlerRevamped for tables where the rows don't all have the same number of columns.  Takes the widest row and pads others with empty strings.More special characters added to asUnHtml.New comment for Class Number.Smalltalk vmPath gave the wrong answer when the VM is enclosed in a Macintosh application package.  Now it is peeled back to the user's folder."!!Number commentStamp: 'tk 4/20/2002 08:32' prior: 0!Class Number holds the most general methods for dealing with numbers. Subclasses Float, Fraction, and Integer, and their subclasses, provide concrete representations of a numeric quantity.All of Number's subclasses participate in a simple type coercion mechanism that supports mixed-mode arithmetic and comparisons.  It works as follows:  If	self<typeA> op: arg<typeB>fails because of incompatible types, then it is retried in the following guise:	(arg adaptTypeA: self) op: arg adaptToTypeA.This gives the arg of typeB an opportunity to resolve the incompatibility, knowing exactly what two types are involved.  If self is more general, then arg will be converted, and viceVersa.  This mechanism is extensible to any new number classes that one might wish to add to Squeak.  The only requirement is that every subclass of Number must support a pair of conversion methods specific to each of the other subclasses of Number.!!HtmlTable methodsFor: 'accessing' stamp: 'tk 3/23/2002 15:17'!asArrayOfData	"Return an Array2D of the table, removing all html.  This in only the text and numbers that the user would see on a web page.  Remove all comments and formatting.  Width is the widest row, and others are padded with empty strings."	| cc array2D widths |	cc _ contents select: [:ent | ent isTableRow].	widths _ cc contents collect: [:row | 		row contents count: [:ent | ent isTableDataItem]].	array2D _ Array2D width: (widths max) height: cc size.	cc withIndexDo: [:row :rowNum |		array2D atRow: rowNum put: 			(row asArrayOfData padded: #right to: array2D width with: '')].	^ array2D! !!String methodsFor: 'converting' stamp: 'tk 4/13/2002 09:13'!asUnHtml	"Strip out all Html stuff (commands in angle brackets <>) and convertthe characters &<> back to their real value.  Leave actual cr and tab asthey were in text."	| in out char rest did |	in _ ReadStream on: self.	out _ WriteStream on: (String new: self size).	[in atEnd] whileFalse:		[in peek = $<			ifTrue: [in unCommand] 	"Absorb <...><...>"			ifFalse: [(char _ in next) = $&						ifTrue: [rest _ in upTo: $;.								did _ out position.								rest = 'lt' ifTrue: [out nextPut: $<].								rest = 'gt' ifTrue: [out nextPut: $>].								rest = 'amp' ifTrue: [out nextPut: $&].								rest = 'deg' ifTrue: [out nextPut: $�].								rest = 'quot' ifTrue: [out nextPut: $"].								did = out position ifTrue: [									self error: 'unknown encoded HTML char'.									"Please add it to this method"]]						ifFalse: [out nextPut: char]].		].	^ out contents! !!SystemDictionary methodsFor: 'image, changes name' stamp: 'tk 4/20/2002 09:22'!vmPath	"Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented.  If we are inside a Macintosh Application Package, go up the the enclosing directory."	"Smalltalk vmPath"	| path up |	path _ self vmPathBasic.	[up _ false.	#('Contents:' 'MacOSClassic:' 'MacOS:' '.app:') do: [:end |		(path endsWith: end) ifTrue: ["inside a Mac package"			path _ path allButLast: end size.			[path last == $:] whileFalse: [path _ path allButLast].			up _ true]].	 up] whileTrue.	^ path! !!SystemDictionary methodsFor: 'image, changes name' stamp: 'tk 4/20/2002 09:13'!vmPathBasic	"Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented."	"Smalltalk vmPath"	<primitive: 142>	^ ''! !