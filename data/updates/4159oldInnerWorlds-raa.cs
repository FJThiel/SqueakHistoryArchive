'From Squeak3.1alpha of 5 February 2001 [latest update: #4158] on 18 June 2001 at 11:25:46 am'!"Change Set:		oldInnerWorldsDate:			18 June 2001Author:			Bob ArningSince the old inner world (world-in-a-window) has not kept pace with recent developments, remove its item in the world/open menu to avoid temptation."!!TheWorldMenu methodsFor: 'construction' stamp: 'RAA 6/18/2001 11:24'!openMenu        "Build the open window menu for the world."        | menu |        menu _ self menu: 'open...'.        self fillIn: menu from: {                {'browser' . { Browser . #openBrowser} }.                {'package browser' . { PackagePaneBrowser . #openBrowser} }.                {'method finder' . { self . #openSelectorBrowser} }.                {'workspace' . {self . #openWorkspace} }.                {'file list' . {self . #openFileList} }.                {'file...' . { FileList . #openFileDirectly} }.                {'transcript' . {self . #openTranscript} }.                "{'inner world' . { WorldWindow . #test1} }."                nil.                {'simple change sorter' . {self . #openChangeSorter1} }.                {'dual change sorter' . {self . #openChangeSorter2} }.                nil.                {'email reader' . {self . #openEmail} }.                {'web browser' . { Scamper . #openAsMorph} }.                {'IRC chat' . {self . #openIRC} }.                nil.        }.        self mvcProjectsAllowed ifTrue: [                self fillIn: menu from: { {'mvc project' . {self. #openMVCProject} } }        ].        ^self fillIn: menu from: {                 {'morphic project' . {self. #openMorphicProject} }.        }.! !