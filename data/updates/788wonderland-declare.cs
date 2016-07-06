'From Squeak 2.3 of January 14, 1999 on 2 April 1999 at 2:23:11 pm'!Smalltalk at: #WonderlandConstants put: Dictionary new.
Object subclass: #AbstractAnimation
	instanceVariableNames: 'startTime endTime duration state direction loopCount undoable myScheduler myWonderland pausedInterval animatedObject '
	classVariableNames: 'Finished Forward Infinity Paused Reverse Running Stopped Waiting '
	poolDictionaries: ''
	category: 'Wonderland-Time'!
Object subclass: #Action
	instanceVariableNames: 'actionTask paused affectedObject lifetime stopCondition myScheduler '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
Object subclass: #Alarm
	instanceVariableNames: 'alarmTask alarmTime myScheduler '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
AbstractAnimation subclass: #Animation
	instanceVariableNames: 'startState endState proportionDone getStartStateFunction getEndStateFunction updateFunction styleFunction '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
Animation subclass: #AbsoluteAnimation
	instanceVariableNames: 'lastStartState '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
AbstractAnimation subclass: #CompositeAnimation
	instanceVariableNames: 'children childLoopCounts '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
Object subclass: #Hierarchical
	instanceVariableNames: 'myParent myChildren '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Objects'!
Object subclass: #Interpolateable
	instanceVariableNames: 'value '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Misc'!
Interpolateable class
	instanceVariableNames: 'value '!
CompositeAnimation subclass: #ParallelAnimation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
Animation subclass: #RelativeAnimation
	instanceVariableNames: 'getReverseStateFunction '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
Object subclass: #Scheduler
	instanceVariableNames: 'currentTime elapsedTime speed alarmList actionList animationList lastSystemTime isRunning schedulerProcess '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
CompositeAnimation subclass: #SequentialAnimation
	instanceVariableNames: 'index '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Time'!
Object subclass: #UndoAction
	instanceVariableNames: 'wrappedAction '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoAnimation
	instanceVariableNames: 'wrappedAnimation '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoByStopping
	instanceVariableNames: 'stoppableItem myUndoStack '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoColorChange
	instanceVariableNames: 'theActor originalColor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoPOVChange
	instanceVariableNames: 'theActor originalPOV '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoPositionChange
	instanceVariableNames: 'theActor originalPosition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoRotationChange
	instanceVariableNames: 'theActor originalRotation '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoShowHide
	instanceVariableNames: 'theActor undoingHide '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoSizeChange
	instanceVariableNames: 'theActor originalSize '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoTextureChange
	instanceVariableNames: 'theActor originalTexture '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #UndoVisibilityChange
	instanceVariableNames: 'theActor originalVisibility '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Undo'!
Object subclass: #Wonderland
	instanceVariableNames: 'myScheduler myNamespace sharedMeshDict sharedTextureDict myUndoStack defaultCamera cameraList lightList sceneObject scriptEditor '
	classVariableNames: ''
	poolDictionaries: 'WonderlandConstants '
	category: 'Wonderland-Core'!
Hierarchical subclass: #WonderlandActor
	instanceVariableNames: 'myName myWonderland myMesh myTexture myMaterial myColor composite scaleMatrix hidden firstClass '
	classVariableNames: ''
	poolDictionaries: 'WonderlandConstants '
	category: 'Wonderland-Objects'!
StringHolder subclass: #WonderlandActorBrowser
	instanceVariableNames: 'myWonderland myListMorph actorListIndex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Morphs'!
WonderlandActor subclass: #WonderlandCamera
	instanceVariableNames: 'perspective viewMatrix myMorph drawSceneBackground '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Objects'!
ImageMorph subclass: #WonderlandCameraControls
	instanceVariableNames: 'myCamera myScheduler myUndoStack moveAction '
	classVariableNames: ''
	poolDictionaries: 'WonderlandConstants '
	category: 'Wonderland-Morphs'!
Morph subclass: #WonderlandCameraMorph
	instanceVariableNames: 'myCamera myWonderland myControls '
	classVariableNames: ''
	poolDictionaries: 'WonderlandConstants '
	category: 'Wonderland-Morphs'!
TabbedPalette subclass: #WonderlandEditor
	instanceVariableNames: 'myWonderland myScriptEditor myActorBrowser '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Morphs'!
B3DIndexedTriangleMesh subclass: #WonderlandMesh
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Mesh'!
Morph subclass: #WonderlandMorph
	instanceVariableNames: 'myWonderland myScheduler '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Morphs'!
Hierarchical subclass: #WonderlandScene
	instanceVariableNames: 'myWonderland myColor '
	classVariableNames: ''
	poolDictionaries: 'WonderlandConstants '
	category: 'Wonderland-Objects'!
Workspace subclass: #WonderlandScriptEditor
	instanceVariableNames: 'myTextEditor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wonderland-Morphs'!
Object subclass: #WonderlandStyle
	instanceVariableNames: 'instVarName1 instVarName2 '
	classVariableNames: 'ClassVarName1 ClassVarName2 '
	poolDictionaries: ''
	category: 'Wonderland-Misc'!
Object subclass: #WonderlandUndoStack
	instanceVariableNames: 'theStack stackIsOpen maxStackDepth '
	classVariableNames: ''
	poolDictionaries: 'WonderlandConstants '
	category: 'Wonderland-Undo'!
Object subclass: #WonderlandVerifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'WonderlandConstants '
	category: 'Wonderland-Misc'!

