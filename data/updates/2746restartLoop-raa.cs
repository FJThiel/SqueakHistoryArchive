'From Squeak2.9alpha of 17 July 2000 [latest update: #2772] on 29 September 2000 at 1:33:49 pm'!"Change Set:		restartLoopDate:			29 September 2000Author:			Bob ArningReiterate the removal of <exitFlag>, then ensure world loop restart (in the postscript)."!Model subclass: #Project	instanceVariableNames: 'world changeSet transcript parentProject previousProject displayDepth viewSize thumbnail nextProject guards projectParameters isolatedHead inForce version urlList environment lastDirectory lastSavedAtSeconds '	classVariableNames: 'CurrentProject GoalFreePercent GoalNotMoreThan UIProcess '	poolDictionaries: ''	category: 'System-Support'!"Postscript:Cause world loop restart since <exitFlag> was removed."World isMorph ifTrue: [	WorldState addDeferredUIMessage: [Project spawnNewProcessAndTerminateOld: true].].!