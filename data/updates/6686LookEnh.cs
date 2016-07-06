'From Squeak3.9alpha of 4 July 2005 [latest update: #6678] on 13 August 2005 at 4:27:26 pm'!"Change Set:		NewLookDate:			13 August 2005Author:			Marcus und AdrianResetings and preferences for the new look"!"Postscript:"| resizeOnAllSides showSplitterHandles fastSplitterResize |	resizeOnAllSides _ Preferences valueOfFlag: #resizeOnAllSides ifAbsent: false.	Preferences		addPreference: #resizeOnAllSides		category: #windows		default: resizeOnAllSides		balloonHelp: 'When this feature is turned on it will allow windows to be resized on on sides. If not enabled then the resizing of windows will only be accessible from the bottom right corner'.	showSplitterHandles _ Preferences valueOfFlag: #showSplitterHandles ifAbsent: true.	Preferences		addPreference: #showSplitterHandles		category: #windows		default: showSplitterHandles		balloonHelp: 'When this feature is turned on it will show handles on the splitter between panes. Resizing the panes will require you to grab the handles. If not enabled then the entire splitter can be used to resize and no handles will be displayed'.	fastSplitterResize  _ Preferences valueOfFlag: #fastSplitterResize ifAbsent: false.	Preferences		addPreference: #fastSplitterResize		category: #windows		default: fastSplitterResize		balloonHelp: 'When this feature is turned on it will speed up the resizing of panes when using the splitter by eliminating the animation of pane size modifications'.SystemWindow initialize.MenuMorph initialize.Cursor initialize.Preferences setParameter: #menuColor to: Color lightGray muchLighter.Preferences setPreference: #alternativeWindowBoxesLook toValue: false.Preferences setPreference: #gradientMenu toValue: true.Preferences setPreference: #gradientScrollBars toValue: false.SystemProgressMorph reset.!