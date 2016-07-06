'From Squeak3.7alpha of 11 September 2003 [latest update: #5707] on 19 February 2004 at 5:02:53 pm'!"Change Set:		SpeakerDelayFix-nkDate:			19 February 2004Author:			Ned KonzEnhancement and fix for Speaker>>say:- start speaking immediately- allows adding # characters inside the argument to Speaker>>say:each # character adds 200 msec of delay.So the second one of these has a 1 second delay between the 2 phrases:Speaker man say: ' Hello how are you? what''s for lunch?'.Speaker man say: ' Hello how are you? ##### what''s for lunch?'.Also makes the KlattFrameMorph look a little nicer."!!KlattFrameMorph methodsFor: 'initialization' stamp: 'nk 2/19/2004 16:55'!initialize	super initialize.	self listDirection: #topToBottom.	self layoutInset: 6; cellInset: 4.	self hResizing: #shrinkWrap; vResizing: #shrinkWrap.! !!KlattFrameMorph methodsFor: 'initialization' stamp: 'nk 2/19/2004 16:58'!newSliderForParameter: parameter target: target min: min max: max description: description	| r slider m |	r _ AlignmentMorph newRow.	r color: self color; borderWidth: 0; layoutInset: 0.	r hResizing: #spaceFill; vResizing: #shrinkWrap; extent: 5@20; wrapCentering: #center; cellPositioning: #leftCenter; cellInset: 4@0.	slider _ SimpleSliderMorph new		color: (Color r: 0.065 g: 0.548 b: 0.645);		extent: 120@2;		target: target;		actionSelector: (parameter, ':') asSymbol;		minVal: min;		maxVal: max;		adjustToValue: (target perform: parameter asSymbol).	r addMorphBack: slider.	m _ StringMorph new contents: parameter, ': '; hResizing: #rigid.	r addMorphBack: m.	m _ UpdatingStringMorph new		target: target; getSelector: parameter asSymbol; putSelector: (parameter, ':') asSymbol;		width: 60; growable: false; floatPrecision: (max - min / 100.0 min: 1.0); vResizing: #spaceFill; step.	r addMorphBack: m.	r setBalloonText: description.	^ r! !!Speaker methodsFor: 'playing' stamp: 'nk 2/19/2004 16:50'!numberSignDelay	"Answer the number of milliseconds that a # symbol in the string given to say: will generate."	^200! !!Speaker methodsFor: 'playing' stamp: 'nk 2/19/2004 16:50'!say: aString 	"aString may contain characters and punctuation.	You may also include the # symbol in aString;	for each one of these, a 200msec delay will be generated."	| events stream string token delay |	stream := ReadStream				on: ((aString						copyReplaceAll: '-'						with: ' '						asTokens: false)						findTokens: '?# '						keep: '?#').	string := ''.	delay := 0.	[stream atEnd]		whileFalse: [token := stream next.			token = '#'				ifTrue: [ self voice playSilenceMSecs: self numberSignDelay.					delay := delay + self numberSignDelay ]				ifFalse: [string := string , ' ' , token.					(token = '?' or: [stream atEnd])						ifTrue: [							events := CompositeEvent new.							events addAll: (self eventsFromString: string).							events playOn: self voice delayed: delay.							delay := delay + (events duration * 1000).							string _ ''  ]]].	self voice flush! !