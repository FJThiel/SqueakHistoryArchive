'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6527] on 5 January 2005 at 6:24:15 pm'!"Change Set:		SharedQueuePeek-tprDate:			5 January 2005Author:			tim@sumeru.stanford.eduThe comment for SharedQueue>peek claims that the method will suspend a sender if the queue is empty. A simple test shows this to be crap.It is pretty obvious the comment/code was copied from ShareQueue>next and not well edited."!!SharedQueue methodsFor: 'accessing' stamp: 'tpr 1/5/2005 18:22'!peek	"Answer the object that was sent through the receiver first and has not 	yet been received by anyone but do not remove it from the receiver. If 	no object has been sent, return nil"	| value |	accessProtect		critical: [readPosition >= writePosition					ifTrue: [readPosition _ 1.							writePosition _ 1.							value _ nil]					ifFalse: [value _ contentsArray at: readPosition]].	^value! !