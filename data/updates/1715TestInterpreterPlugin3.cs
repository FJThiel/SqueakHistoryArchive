'From Squeak2.7alpha of 28 October 1999 [latest update: #1698] on 11 December 1999 at 8:40:55 pm'!"Change Set:		TestInterpreterPlugin3Date:			11 December 1999Author:			Andrew C. GreenbergFixes a bug pointed out by Stephan Rudolph, where primitives without specifications ended with an uncompilable 'return self'"!!TestTMethod methodsFor: 'named primitives' stamp: 'acg 12/9/1999 20:32'!extractPrimitiveDirectives	"Save selector in fullSelector and args in fullArgs.  Scan top-level statements for a directive of the form:		self				primitive: 	<string>or		self			primitive:	<string>			parameters: <list of class names>or		self			primitive:	<string>			parameters: <list of class names>			receiver: <class name>or an assignment of that expression to a local, and manipulate the state and parse tree accordingly."	parseTree setStatements: (Array streamContents:		[:sStream |			parseTree statements do:				[:stmt |				 (self primitiveDirectiveWasHandled: stmt on: sStream)					ifFalse: [sStream nextPut: stmt]]]).	isPrimitive 		ifTrue:			[export _ true.			 parseTree 				setStatements: self buildNamedPrimitiveProlog, 								parseTree statements.			 self fixUpReturns.			 self replaceSizeMessages.			 ^true]		ifFalse: [self removeFinalSelfReturn].	^false! !