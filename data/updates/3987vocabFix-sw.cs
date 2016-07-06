'From Squeak3.1alpha of 28 February 2001 [latest update: #3986] on 5 May 2001 at 1:51:01 am'!"Change Set:		vocabFix-swDate:			5 May 2001Author:			Scott WallaceFixes bug that kept balloon help from showing up on non-slot-like methods in viewers."!!CategoryViewer methodsFor: 'entries' stamp: 'sw 5/5/2001 01:24'!phraseForCommandFrom: aMethodInterface	"Answer a phrase for the non-slot-like command represented by aMethodInterface - classic tiles"	| aRow resultType cmd names argType argTile selfTile aPhrase balloonTextSelector stat inst aDocString universal tileBearingHelp |	aDocString _ aMethodInterface documentationOrNil.	names _ scriptedPlayer class namedTileScriptSelectors.	resultType _ aMethodInterface resultType.	cmd _ aMethodInterface selector.	(universal _ scriptedPlayer isUniversalTiles)		ifTrue:			[aPhrase _ scriptedPlayer universalTilesForInterface: aMethodInterface]		ifFalse: [cmd numArgs == 0			ifTrue:				[aPhrase _ PhraseTileMorph new setOperator: cmd					type: resultType					rcvrType: #player]			ifFalse:				["only one arg supported in classic tiles, so if this is fed				with a selector with > 1 arg, results will be very strange"				argType _ aMethodInterface typeForArgumentNumber: 1.				aPhrase _ PhraseTileMorph new setOperator: cmd					type: resultType					rcvrType: #player					argType: argType.				argTile _ self tileForArgType: argType.				argTile position: aPhrase lastSubmorph position.				aPhrase lastSubmorph addMorph: argTile]].	(scriptedPlayer slotInfo includesKey: cmd)		ifTrue: [balloonTextSelector _ #userSlot].	(scriptedPlayer belongsToUniClass and: [scriptedPlayer class includesSelector: cmd])		ifTrue:			[aDocString ifNil:				[aDocString _ (scriptedPlayer class userScriptForPlayer: scriptedPlayer selector: cmd) documentationOrNil].			aDocString ifNil:				[balloonTextSelector _ #userScript]].	tileBearingHelp _ universal ifTrue: [aPhrase submorphs second] ifFalse: [aPhrase operatorTile]. 	aDocString		ifNotNil:			[tileBearingHelp setBalloonText: aDocString]		ifNil:			[tileBearingHelp balloonTextSelector: (balloonTextSelector ifNil: [cmd])].	aPhrase markAsPartsDonor.	cmd == #emptyScript ifTrue:		[aPhrase setProperty: #newPermanentScript toValue: true.		aPhrase setProperty: #newPermanentPlayer toValue: scriptedPlayer.		aPhrase submorphs second setBalloonText: 'drag and drop to add a new script'].	universal ifFalse:		[selfTile _ self tileForSelf.		selfTile position: aPhrase firstSubmorph position.		aPhrase firstSubmorph addMorph: selfTile].	aRow _ ViewerRow newRow borderWidth: 0; color: self color.	aRow elementSymbol: cmd asSymbol.	aRow addMorphBack: (ScriptingSystem tryButtonFor: aPhrase).	aRow addMorphBack: (Morph new extent: 4@2; beTransparent).	aRow addMorphBack: (self infoButtonFor: cmd).	aRow addMorphBack: aPhrase.	(names includes: cmd) ifTrue:		[aPhrase userScriptSelector: cmd.		aPhrase beTransparent.		aRow addMorphBack: AlignmentMorph newVariableTransparentSpacer.		aRow addMorphBack: (stat _ (inst _ scriptedPlayer scriptInstantiationForSelector: cmd) statusControlMorph).		inst updateStatusMorph: stat].	aRow beSticky; disableDragNDrop.	^ aRow! !