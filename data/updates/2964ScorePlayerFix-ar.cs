'From Squeak2.9alpha of 13 June 2000 [latest update: #3008] on 15 November 2000 at 11:41:38 am'!"Change Set:		ScorePlayerFix-arDate:			15 November 2000Author:			Andreas RaabFixes the menu in score players."!!ScorePlayerMorph methodsFor: 'menu' stamp: 'ar 11/15/2000 11:34'!invokeMenu	"Invoke a menu of additonal functions for this WaveEditor."	| aMenu |	aMenu _ MenuMorph new defaultTarget: self.	aMenu add: 'reload instruments' target: AbstractSound selector: #updateScorePlayers.	aMenu add: 'open a MIDI file' action: #openMIDIFile.	scorePlayer midiPort		ifNil: [			aMenu add: 'play via MIDI' action: #openMIDIPort]		ifNotNil: [			aMenu add: 'play via built in synth' action: #closeMIDIPort.			aMenu add: 'new MIDI controller' action: #makeMIDIController:].	aMenu popUpInWorld: self world.! !