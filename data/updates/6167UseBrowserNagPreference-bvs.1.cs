'From Squeak3.7beta of ''1 April 2004'' [latest update: #5967] on 20 July 2004 at 3:44:07 pm'!!Browser methodsFor: 'class comment pane' stamp: 'bvs 7/20/2004 15:42'!noCommentNagString	^ Preferences browserNagIfNoClassComment		ifTrue: [Text string: 'THIS CLASS HAS NO COMMENT!!' translated attribute: TextColor red]		ifFalse: ['']		! !!Browser methodsFor: 'class functions' stamp: 'bvs 7/20/2004 15:40'!classCommentText	"return the text to display for the comment of the currently selected class"	| theClass |	theClass _ self selectedClassOrMetaClass.	theClass ifNil: [ ^''].	^ theClass hasComment		ifTrue: [  theClass comment  ]		ifFalse: [ self noCommentNagString ]! !