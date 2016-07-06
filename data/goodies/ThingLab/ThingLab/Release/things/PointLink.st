ThingLabObject subclass: #PointLink
	instanceVariableNames: 'node target'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Prototypes'!

PointLink prototype parts: 'node target'.
PointLink prototype field: #node replaceWith: #Node clonePrototype.
PointLink prototype field: #target replaceWith: 20@30!

PointLink prototype inserters:  #('node'  'target')!

Constraint owner: PointLink prototype
	rule: 'node value = target'
	methods:  #('node primitiveSet.value: target'
		'self primitiveSet.target: node value')!

PointLink compile: 'showPicture: medium
	| bic |
	bic _ Pen newOnForm: medium.  bic color: Color lightGray; defaultNib: 2;
		place: node location; goto: target.
	node showPicture: medium'
classified: 'showing'.
