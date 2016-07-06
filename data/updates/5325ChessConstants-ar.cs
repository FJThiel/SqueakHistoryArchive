'From TeaSqueak3.2 of 19 September 2002 [latest update: #401] on 18 May 2003 at 6:22:43 pm'!"Change Set:		ChessConstantsPoolDate:			18 May 2003Author:			Andreas RaabRewrite ChessConstants as declarative pool.(removed refs to ChessPlayer which is no longer in the image -dew)"Smalltalk at: #ChessConstantsOBSOLETEPOOL put: ChessConstants.Smalltalk removeKey: #ChessConstants.!SharedPool subclass: #ChessConstants	instanceVariableNames: ''	classVariableNames: 'Bishop BishopMoves CastlingDisableAll CastlingDisableKingSide CastlingDisableQueenSide CastlingDone CastlingEnableKingSide CastlingEnableQueenSide EmptySquare King KingMoves Knight KnightMoves Pawn PieceCenterScores PieceValues Queen Rook RookMoves '	poolDictionaries: ''	category: 'Morphic-Games-Chess'!!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:16'!initialize	"ChessConstants initialize"	self initializePieceConstants.	self initializeCastlingConstants.	self initializePieceValues.	self initializeMoves.	self initializeCenterScores.! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:15'!initializeBishopMoves	"ChessPlayer initialize"	| index moveList1 moveList2 moveList3 moveList4 px py |	BishopMoves _ Array new: 64 withAll: #().	0 to: 7 do:[:j|		0 to: 7 do:[:i|			index _ (j * 8) + i + 1.			moveList1 _ moveList2 _ moveList3 _ moveList4 _ #().			1 to: 7 do:[:k|				px _ i + k. py _ j - k.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList1 _ moveList1 copyWith: (py * 8) + px + 1].				px _ i - k. py _ j - k.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList2 _ moveList2 copyWith: (py * 8) + px + 1].				px _ i + k. py _ j + k.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList3 _ moveList3 copyWith: (py * 8) + px + 1].				px _ i - k. py _ j + k.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList4 _ moveList4 copyWith: (py * 8) + px + 1].			].			BishopMoves at: index put: {moveList1. moveList2. moveList3. moveList4}.		].	].! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:09'!initializeCastlingConstants	CastlingDone _ 1.	CastlingDisableKingSide _ 2.	CastlingDisableQueenSide _ 4.	CastlingDisableAll _ CastlingDisableQueenSide bitOr: CastlingDisableKingSide.	CastlingEnableKingSide _ CastlingDone bitOr: CastlingDisableKingSide.	CastlingEnableQueenSide _ CastlingDone bitOr: CastlingDisableQueenSide.! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:16'!initializeCenterScores	"ChessPlayer initialize"	PieceCenterScores _ Array new: 6.	1 to: 6 do:[:i| PieceCenterScores at: i put: (ByteArray new: 64)].	PieceCenterScores at: Knight put:		#(			-4	0	0	0	0	0	0	-4			-4	0	2	2	2	2	0	-4			-4	2	3	2	2	3	2	-4			-4	1	2	5	5	2	2	-4			-4	1	2	5	5	2	2	-4			-4	2	3	2	2	3	2	-4			-4	0	2	2	2	2	0	-4			-4	0	0	0	0	0	0	-4		).	PieceCenterScores at: Bishop put:		#(			-2	-2	-2	-2	-2	-2	-2	-2			-2	0	0	0	0	0	0	-2			-2	0	1	1	1	1	0	-2			-2	0	1	2	2	1	0	-2			-2	0	1	2	2	1	0	-2			-2	0	1	1	1	1	0	-2			-2	0	0	0	0	0	0	-2			-2	-2	-2	-2	-2	-2	-2	-2		).	PieceCenterScores at: Queen put:		#(			-3	0	0	0	0	0	0	-3			-2	0	0	0	0	0	0	-2			-2	0	1	1	1	1	0	-2			-2	0	1	2	2	1	0	-2			-2	0	1	2	2	1	0	-2			-2	0	1	1	1	1	0	-2			-2	0	0	0	0	0	0	-2			-3	0	0	0	0	0	0	-3		).! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:15'!initializeKingMoves	"ChessPlayer initialize"	| index px py moveList |	KingMoves _ Array new: 64 withAll: #().	0 to: 7 do:[:j|		0 to: 7 do:[:i|			index _ (j * 8) + i + 1.			moveList _ #().			#( (-1 -1) (0 -1) (1 -1) (-1 0) (1 0) (-1 1) (0 1) (1 1)) do:[:spec|				px _ i + spec first.				py _ j + spec last.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList _ moveList copyWith: (py * 8) + px + 1]].			KingMoves at: index put: moveList		].	].! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:14'!initializeKnightMoves	"ChessPlayer initialize"	| index px py moveList |	KnightMoves _ Array new: 64 withAll: #().	0 to: 7 do:[:j|		0 to: 7 do:[:i|			index _ (j * 8) + i + 1.			moveList _ #().			#( (-2 -1) (-1 -2) (1 -2) (2 -1) (-2 1) (-1 2) (1 2) (2 1)) do:[:spec|				px _ i + spec first.				py _ j + spec last.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList _ moveList copyWith: (py * 8) + px + 1]].			KnightMoves at: index put: moveList		].	].! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:11'!initializeMoves	"ChessPlayer initialize"	self initializeKnightMoves.	self initializeRookMoves.	self initializeBishopMoves.	self initializeKingMoves.! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:09'!initializePieceConstants	EmptySquare := 0.	Pawn := 1.	Knight := 2.	Bishop := 3.	Rook := 4.	Queen := 5.	King := 6.! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:14'!initializePieceValues	PieceValues _ Array new: 6.	PieceValues at: Pawn put: 100.	PieceValues at: Knight put: 300.	PieceValues at: Bishop put: 350.	PieceValues at: Rook put: 500.	PieceValues at: Queen put: 900.	PieceValues at: King put: 2000.! !!ChessConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:14'!initializeRookMoves	"ChessPlayer initialize"	| index moveList1 moveList2 moveList3 moveList4 px py |	RookMoves _ Array new: 64 withAll: #().	0 to: 7 do:[:j|		0 to: 7 do:[:i|			index _ (j * 8) + i + 1.			moveList1 _ moveList2 _ moveList3 _ moveList4 _ #().			1 to: 7 do:[:k|				px _ i + k. py _ j.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList1 _ moveList1 copyWith: (py * 8) + px + 1].				px _ i. py _ j + k.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList2 _ moveList2 copyWith: (py * 8) + px + 1].				px _ i - k. py _ j.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList3 _ moveList3 copyWith: (py * 8) + px + 1].				px _ i. py _ j - k.				((px between: 0 and: 7) and:[py between: 0 and: 7]) ifTrue:[					moveList4 _ moveList4 copyWith: (py * 8) + px + 1].			].			RookMoves at: index put: {moveList1. moveList2. moveList3. moveList4}.		].	].! !ChessConstants initialize!"Postscript:Rebind users of ChessConstants"Smalltalk allClassesDo:[:aClass|	(aClass sharedPools includes: ChessConstantsOBSOLETEPOOL) ifTrue:[		Compiler evaluate: (aClass definition copyReplaceAll: 'OBSOLETEPOOL' with: '').	].].Smalltalk removeKey: #ChessConstantsOBSOLETEPOOL.!