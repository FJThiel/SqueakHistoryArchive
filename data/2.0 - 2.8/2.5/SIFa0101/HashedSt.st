'From Squeak 2.5 of August 6, 1999 on 27 September 1999 at 4:49:32 pm'!
"Change Set:		QuotedStringSymbolsPatch
Date:			31 December 1998
Author:			Stefan Matthias Aust (sma@kiel.netsurf.de)

Add support for quoted symbols (#'abc'). I'd love to see this
change getting incorporated into the base system. It slightly
extends the system without breaking compatibility.

 ******
 * $Name:  $
 * $Id: HashedSt.st 1.1 1999-10-12 07:43:23-05 Harmon Alpha $
 * $Log: HashedSt.st $
 * Revision 1.1  1999-10-12 07:43:23-05  Harmon
 * Initial revision
 *
 ******"!


!Scanner methodsFor: 'multi-character scans' stamp: 'sma 12/31/1998 11:25'!
xLitQuote
	"UniqueStrings and vectors: #(1 (4 5) 2 3) #ifTrue:ifFalse:.
	 For ##x answer #x->nil.  For ###x answer nil->#x."

	| start |
	self step. "litQuote"
	self scanToken.
	tokenType = #leftParenthesis
		ifTrue: 
			[start _ mark.
			self scanToken; scanLitVec.
			tokenType == #doIt
				ifTrue: [mark _ start.
						self offEnd: 'Unmatched parenthesis']]
		ifFalse: 
			[(#(word keyword colon ) includes: tokenType) 
				ifTrue:
					[self scanLitWord]
				ifFalse:
					[(tokenType==#literal)
						ifTrue:
							[(token isMemberOf: Association)
								ifTrue: "###word"
									[token _ nil->token key].
							(token isMemberOf: Symbol)
								ifTrue: "##word"
									[token _ token->nil]]
						ifFalse:
							[tokenType==#string ifTrue: [token _ token asSymbol]]]].
	tokenType _ #literal

"	#(Pen)
	#Pen
	#'Pen'
	##Pen
	###Pen
"! !


