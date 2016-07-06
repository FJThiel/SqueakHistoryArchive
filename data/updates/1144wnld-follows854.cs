'From Squeak 2.3 of January 14, 1999 on 20 April 1999 at 1:53:21 pm'!"Change Set:		wnld-quickref-jspDate:			20 April 1999Author:			Jeff PierceAdds a quick reference pane to the Wonderland editor."!TabbedPalette subclass: #WonderlandEditorTabs	instanceVariableNames: 'myWonderland myScriptEditor myActorViewer myQuickReference '	classVariableNames: ''	poolDictionaries: ''	category: 'Wonderland-Morphs'!Workspace subclass: #WonderlandQuickReference	instanceVariableNames: 'myTextEditor '	classVariableNames: 'HelpText '	poolDictionaries: ''	category: 'Wonderland-Morphs'!!WonderlandEditorTabs methodsFor: 'initialization' stamp: 'jsp 4/20/1999 12:09'!initializeWith: aWonderland	"Initialize the editor with the Wonderland."	myWonderland _ aWonderland.	myScriptEditor _ (WonderlandScriptEditor new).	self addPanel: (myScriptEditor getMorph).	(self tabsMorph submorphs at: 1) tabSelected.	myScriptEditor setBindings: (myWonderland getNamespace).	myActorViewer _ WonderlandActorViewer new.	self addPanel: myActorViewer.	myQuickReference _ (WonderlandQuickReference new).	self addPanel: (myQuickReference getMorph).! !!WonderlandQuickReference methodsFor: 'initialize-release' stamp: 'jsp 4/20/1999 12:08'!initialize	"Create a modified workspace as our script editor"	super initialize.	myTextEditor _ (PluggableTextMorph on: self text: #contents accept: #acceptContents:			readSelection: nil menu: #codePaneMenu:shifted:).	myTextEditor name: 'Quick Reference'.	myTextEditor scrollBarOnLeft: false.	myTextEditor extent: 500@350.	myTextEditor color: (Color r: 0.815 g: 0.972 b: 0.878).! !!WonderlandQuickReference methodsFor: 'accessing' stamp: 'jsp 4/20/1999 12:12'!contents	"Return the help text"	^ HelpText.! !!WonderlandQuickReference methodsFor: 'accessing' stamp: 'jsp 4/20/1999 12:10'!getMorph	"Returns the text editor morph"	^ myTextEditor.! !!WonderlandQuickReference class methodsFor: 'class initialization' stamp: 'jsp 4/20/1999 13:52'!initialize	"Initialize the help text."	HelpText _ 'This Quick Reference tab lists the basic commands and command combinations.Names in every Wonderland:	scene - the scene containing all the actors	scheduler - the animation scheduler	camera - the default camera	cameraWindow - the morph the default camera renders into	ground - the ground plane	w - the WonderlandWonderlandActors=====================This section lists the commands for actors broken down by category.Movement------------Useful constants:	direction: left, right, up, down, forward, back	duration: rightNow, eachFrame	style: gently, abruptly, beginGently, endGently	position: asIs	location: onTopOf, below, beneath, inFrontOf, inBackOf, behind, toLeftOf,			toRightOf, onFloorOf, onCeilingOfmove: <direction>move: <direction> distance: <number>move: <direction> distance: <number>move: <direction> distance: <number> duration: <number>move: <direction> distance: <number> duration: <number> style: <style>move: <direction> asSeenBy: <actor>move: <direction> distance: <number> asSeenBy: <actor>move: <direction> distance: <number> duration: <number> asSeenBy: <actor>move: <direction> distance: <number> duration: <number> asSeenBy: <actor> style: <style>move: <direction> speed: <number>move: <direction> speed: <number> for: <number>move: <direction> speed: <number> until: <condition>move: <direction> speed: <number> asSeenBy: <actor>move: <direction> speed: <number> asSeenBy: <actor> for: <number>move: <direction> speed: <number> asSeenBy: <actor> until: <condition>For moveTo, the position may be a { right. up. forward } triple or an <actor>. The triple values may be a number or asIs (ex. { asIs. 0. asIs } ).moveTo: <position>moveTo: <position> duration: <number>moveTo: <position> duration: <number> style: <style>moveTo: <position> asSeenBy: <actor>moveTo: <position> duration: <number> asSeenBy: <actor>moveTo: <position> duration: <number> asSeenBy: <actor> style: <style>moveTo: <position> eachFrameFor: <number>moveTo: <position> eachFrameUntil: <condition>moveTo: <position> asSeenBy: <actor> eachFrameFor: <number>moveTo: <position> asSeenBy: <actor> eachFrameUntil: <condition>moveTo: <position> speed: <number>moveTo: <position> speed: <number> asSeenBy: <actor> nudge: <direction>nudge: <direction> distance: <number>nudge: <direction> distance: <number> duration: <number>nudge: <direction> distance: <number> duration: <number> style: <style>place: <location> object: <actor>place: <location> object: <actor> duration: <number>place: <location> object: <actor> duration: <number> style: <style>place: <location> object: <actor> eachFrameFor: <number>place: <location> object: <actor> eachFrameUntil: <condition>Turning---------Useful constants	direction: left, right, up, down, forward, back	style: gently, abruptly, beginGently, endGently	duration: rightNow, eachFramealignWith: <actor>alignWith: <actor> duration: <number>alignWith: <actor> duration: <number> style: <style>For the pointAt command, a target may be an <actor>, a { right. up. forward } trip, or a x@y pixel value.pointAt: <target>pointAt: <target> duration: <number>pointAt: <target> duration: <number> style: <style>pointAt: <target> duration: <number> eachFrameFor: <number>pointAt: <target> duration: <number> eachFrameUntil: <condition>roll: <direction>roll: <direction> turns: <number> roll: <direction> turns: <number> duration: <number>roll: <direction> turns: <number> duration: <number> style: <style>roll: <direction> asSeenBy: <actor>roll: <direction> turns: <number> asSeenBy: <actor>roll: <direction> turns: <number> duration: <number> asSeenBy: <actor>roll: <direction> turns: <number> duration: <number> asSeenBy: <actor> style: <style>roll: <direction> turns: <number> speed: <number>roll: <direction> turns: <number> speed: <number> asSeenBy: <actor>roll: <direction> speed: <number>roll: <direction> speed: <number> for: <number>roll: <direction> speed: <number> until: <condition>roll: <direction> speed: <number> asSeenBy: <actor>roll: <direction> speed: <number> asSeenBy: <actor> for: <number>roll: <direction> speed: <number> asSeenBy: <actor> until: <condition>standUpstandUpWithDuration: <number>standUpWithDuration: <number> style: <style>turn: <direction>turn: <direction> turns: <number> turn: <direction> turns: <number> duration: <number>turn: <direction> turns: <number> duration: <number> style: <style>turn: <direction> asSeenBy: <actor>turn: <direction> turns: <number> asSeenBy: <actor>turn: <direction> turns: <number> duration: <number> asSeenBy: <actor>turn: <direction> turns: <number> duration: <number> asSeenBy: <actor> style: <style>turn: <direction> turns: <number> speed: <number>turn: <direction> turns: <number> speed: <number> asSeenBy: <actor>turn: <direction> speed: <number>turn: <direction> speed: <number> for: <number>turn: <direction> speed: <number> until: <condition>turn: <direction> speed: <number> asSeenBy: <actor>turn: <direction> speed: <number> asSeenBy: <actor> for: <number>turn: <direction> speed: <number> asSeenBy: <actor> until: <condition>For turnTo, the orientation may be a rotation about the  { right. up. forward } axes or an <actor>. The triple values may be a number or asIs (ex. { asIs. 0. asIs } ).turnTo: <orientation>turnTo: <orientation> duration: <number>turnTo: <orientation> duration: <number> style: <style>turnTo: <orientation> asSeenBy: <actor>turnTo: <orientation> duration: <number> asSeenBy: <actor>turnTo: <orientation> duration: <number> asSeenBy: <actor> style: <style>turnTo: <orientation> eachFrameFor: <number>turnTo: <orientation> eachFrameUntil: <condition>turnTo: <orientation> asSeenBy: <actor> eachFrameFor: <number>turnTo: <orientation> asSeenBy: <actor> eachFrameUntil: <condition>turnTo: <orientation> speed: <number>turnTo: <orientation> speed: <number> asSeenBy: <actor>Moving and Turning------------------------Useful constants:	target: asIs	duration: rightNow, eachFrame	style: gently, abruptly, beginGently, endGentlyThe target for setPointOfView may be another actor or a { rpos. upos. fpos. rrot. urot. frot } sextuple.setPointOfView: <target>setPointOfView: <target> duration: <number>setPointOfView: <target> duration: <number> style: <style>setPointOfView: <target> asSeenBy: <actor>setPointOfView: <target> duration: <number> asSeenBy: <actor>setPointOfView: <target> duration: <number> asSeenBy: <actor> style: <style>setPointOfView: <target> eachFrameFor: <number>setPointOfView: <target> eachFrameUntil: <condition>setPointOfView: <target> asSeenBy: <actor> eachFrameFor: <number>setPointOfView: <target> asSeenBy: <actor> eachFrameUntil: <condition>Resizing---------Useful constants:	dimension: topToBottom, leftToRight, frontToBackA resize amount may be a <number> for uniform scaling or a { right. up. forward } triple for non-uniform resizing.resize: <amount>resize: <amount> duration: <number>resize: <amount> duration: <number> style: <style>resize: <amount> eachFrameFor: <number>resize: <amount> eachFrameUntil: <condition>resizeLikeRubber: <amount> dimension: <dimension>resizeLikeRubber: <amount> dimension: <dimension> duration: <number>resizeLikeRubber: <amount> dimension: <dimension> duration: <number> style: <style>The size for a setSize are the desired { leftToRight. toptoBottom. frontToBack } dimensions.setSize: <size>setSize: <size> duration: <number>setSize: <size> duration: <number> style: <style>Waiting---------waitwait: <number>Actor Hierarchy-------------------becomeChildOf: <actor>becomePartbecomeFirstClassPlaying Sounds------------------playSound: ''soundfile.wav''Changing Color------------------A color may be a named color (red, green, blue, orange, yellow, periwinkle, etc) or a { red. green. blue } triple.setColor: <color>setColor: <color> duration: <number>setColor: <color> duration: <number> style: <style>Changing Visibility----------------------hideshowsetVisibility: <number>setVisibility: <number> duration: <number>setVisibility: <number> duration: <number> style: <style>Changing Texture---------------------setTexture: ''texturefile.bmp''Animated State-----------------pauseresumestopAdding Reactions--------------------Useful constants:	event types: keyPress, leftMouseDown, leftMouseUp, leftMouseClick,			rightMouseDown, rightMouseUp, rightMouseClickaddResponse: <block> to: <eventType>removeResponse: <response> to: <eventType>respondWith: <block> to: <eventType>Destruction-------------destroydestroy: <duration>Wonderlands===============Creating----------makeActormakeActorFrom: ''actorFile''makeCameraComposing Animations--------------------------doTogether and doInOrder both accept an ordered collection of animations.doTogether: { animations }doInOrder: { animations }'.! !WonderlandQuickReference initialize!