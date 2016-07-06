'From Squeak2.9alpha of 12 June 2000 [latest update: #3021] on 16 November 2000 at 12:57:07 pm'!"Change Set:		ScriptorChanges7Date:			16 November 2000Author:			Dan IngallsQuick fix to SC6 which introduced a bug that proliferated insertion arrows."!!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/16/2000 13:02'!addTempVar: aMorph 	"know we are a block inside a MethodNode" 	"(aMorph nodeClassIs: TempVariableNode) is known to be true."	| tempHolder ii tt var nn tempMorph |	tempHolder _ (ii _ owner submorphIndexOf: self) = 1				ifFalse: [tt _ owner submorphs at: ii - 1.						(tt isSyntaxMorph and: [tt parseNode class == MethodTempsNode])					 		ifTrue: [tt] ifFalse: [nil]]				ifTrue: [nil].	nn _ aMorph parseNode key.	"name"	tempHolder ifNil: [		tempMorph _ owner addRow: #tempVariable on: MethodTempsNode new.		owner addMorph: tempMorph inFrontOf: self.		tempMorph color: tempMorph color darker.		aMorph parseNode name: nn key: nn code: nil.		aMorph parseNode asMorphicSyntaxIn: tempMorph.		tempMorph cleanupAfterItDroppedOnMe.		^ true].	tempHolder ifNotNil: [		tempHolder submorphsDo:			[:m | m isSyntaxMorph and: [m parseNode key = nn ifTrue: [^ false]]].		aMorph parseNode name: nn key: nn code: nil.		var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.		var layoutInset: 1.		var addMorphBack: (StringMorph contents: nn).		var cleanupAfterItDroppedOnMe.		^ true]! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/16/2000 12:24'!mouseMove: evt	| dup selection |	owner isSyntaxMorph ifFalse: [^ self].	self currentSelectionDo:		[:innerMorph :mouseDownLoc :outerMorph |		(evt cursorPoint dist: mouseDownLoc) > 4 ifTrue:			["If drag 5 pixels, then tear off a copy of outer selection."			selection _ outerMorph ifNil: [self].			evt hand attachMorph: (dup _ selection duplicate).			dup align: dup topLeft with: evt hand position + self cursorBaseOffset.			self setSelection: nil.			(self firstOwnerSuchThat: [:m | m isSyntaxMorph and: [m nodeClassIs: BlockNode]])				ifNotNilDo: [:m | "Activate enclosing block."							m startStepping]]].! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/16/2000 12:54'!step	self trackCaret.	super step! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/16/2000 12:52'!showCaretAt: location	| newArrow |	self valueOfProperty: #caretMorph ifPresentDo:		[:arrow | ^ arrow align: arrow bounds leftCenter with: location].	newArrow _ (PolygonMorph vertices: {0@0. 12@-9. 12@-5. 20@-5. 20@5. 12@5. 12@9}			color: Color yellow borderWidth: 1 borderColor: Color yellow darker)			on: #mouseEnterDragging send: #mouseEnterDragging:inCaret: to: self;			on: #mouseLeaveDragging send: #mouseLeaveDragging:inCaret: to: self.	self setProperty: #caretMorph toValue: newArrow.	self addCaretFront: newArrow.	newArrow align: newArrow bounds leftCenter with: location! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/16/2000 12:51'!trackCaret	| hand i pt localPt sub |	hand _ self primaryHand.	(hand lastEvent redButtonPressed and: [(self hasOwner: hand) not]) ifTrue:		[localPt _ self globalPointToLocal: hand position.		(i _ self insertionIndexForCaret: localPt) = 0 ifFalse: [			sub _ submorphs at: i.			pt _ localPt y < sub center y				ifTrue: [sub topLeft + (i=2 ifTrue: [-3@2] ifFalse: [-3@-2])]				ifFalse: [sub bottomLeft + (-3@-2)].			^ self showCaretAt: pt]].	self stopStepping; hideCaret.! !