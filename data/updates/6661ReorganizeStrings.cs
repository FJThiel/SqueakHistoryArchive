'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6643] on 12 April 2005 at 5:31:36 pm'!"Change Set:		ReorganizeStringsDate:			12 April 2005Author:			Andreas RaabMove String and subclasses to their own category."SystemOrganization addCategory: 'Collections-Strings' before: 'Collections-Text'.!Magnitude subclass: #Character	instanceVariableNames: 'value'	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit UppercaseBit'	poolDictionaries: ''	category: 'Collections-Strings'!Collection subclass: #CharacterSet	instanceVariableNames: 'map'	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Support'!ArrayedCollection subclass: #String	instanceVariableNames: ''	classVariableNames: 'AsciiOrder CaseInsensitiveOrder CaseSensitiveOrder CSLineEnders CSNonSeparators CSSeparators HtmlEntities LowercasingTable Tokenish UppercasingTable'	poolDictionaries: ''	category: 'Collections-Strings'!String variableByteSubclass: #ByteString	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Strings'!String variableWordSubclass: #MultiString	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Strings'!String subclass: #Symbol	instanceVariableNames: ''	classVariableNames: 'NewSymbols OneCharacterSymbols SymbolTable'	poolDictionaries: ''	category: 'Collections-Strings'!Symbol variableByteSubclass: #ByteSymbol	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Strings'!Symbol variableWordSubclass: #MultiSymbol	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Strings'!