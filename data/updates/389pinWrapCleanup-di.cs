'From Squeak 2.2 of Sept 23, 1998 on 6 November 1998 at 2:47:17 pm'!!SequenceableCollection methodsFor: 'accessing' stamp: 'di 11/6/1998 14:32'!atPin: index 	"Return the index'th element of me if possible.	Return the first or last element if index is out of bounds."	index < 1 ifTrue: [^ self first].	index > self size ifTrue: [^ self last].	^ self at: index! !!SequenceableCollection methodsFor: 'accessing' stamp: 'di 11/6/1998 14:42'!atWrap: index 	"Return the index'th element of me.  If index is out of bounds, let it wrap around from the end to the beginning until it is in bounds."	(index < 1 or: [index > self size]) ifTrue:		[^ self at: (index - 1 \\ self size) + 1].	^ self at: index! !!SequenceableCollection methodsFor: 'accessing' stamp: 'di 11/6/1998 14:46'!atWrap: index put: value	"Store value into the index'th element of me.  If index is out of bounds, let it wrap around from the end to the beginning until it is in bounds."	(index < 1 or: [index > self size]) ifTrue:		[^ self at: (index - 1 \\ self size) + 1 put: value].	^ self at: index put: value! !Object removeSelector: #atPin:!Object removeSelector: #atWrap:!Object removeSelector: #atWrap:put:!Path removeSelector: #atPin:!Path removeSelector: #atWrap:!Interval removeSelector: #atPin:!Interval removeSelector: #atWrap:!MappedCollection removeSelector: #atPin:!MappedCollection removeSelector: #atWrap:!OrderedCollection removeSelector: #atPin:!OrderedCollection removeSelector: #atWrap:!String removeSelector: #atPin:!String removeSelector: #atWrap:!Text removeSelector: #atPin:!Text removeSelector: #atWrap:!