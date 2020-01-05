Class {
	#name : #SymbolTable,
	#superclass : #Object,
	#instVars : [
		'counts',
		'functionScope',
		'classScope'
	],
	#category : #Ex1
}

{ #category : #'as yet unclassified' }
SymbolTable >> define: name and: type and: kind [
	"comment stating purpose of message"
	| number myKind|
	( (kind = 'argument') | (kind = 'local') ) 
		ifTrue: [ kind = 'argument' ifTrue: [ number := counts at: 'argument'. counts at: 'argument' put: ((counts at: 'argument')+1). ].
					 kind = 'local' ifTrue: [ number := counts at: 'local'. counts at: 'local' put: ((counts at: 'local')+1). ].
					 functionScope at: name put: ({ type . kind . number } asArray).
					 ].
	( (kind = 'static' )| (kind = 'field') ) 
		ifTrue: [ kind = 'static' ifTrue: [ number := counts at: 'static'. counts at: 'static' put: ((counts at: 'static')+1). myKind := 'static'.] .
					 kind = 'field' ifTrue: [ number := counts at: 'this'. counts at: 'this' put: ((counts at: 'this')+1). myKind := 'this'.].
					 classScope at: name put: ({ type . myKind  . number } asArray).	
					]

]

{ #category : #accessing }
SymbolTable >> indexOf: name [
	| result |
	result := 'none'.
	(functionScope keys includes: name) ifTrue: [ result := ((functionScope at: name ) at: 3) asString .].
	(classScope  keys includes: name) ifTrue: [ result := ((classScope  at: name ) at: 3) asString .].
	^result.
]

{ #category : #initialization }
SymbolTable >> initialize [
	"Subclasses should redefine this method to perform initializations on instance creation" 
	counts := Dictionary new.
	counts at: 'static' put: 0.
	counts at: 'this' put: 0.
	counts at: 'argument' put: 0.
	counts at: 'local' put: 0.
	functionScope := Dictionary new.
	classScope := Dictionary new.
	
]

{ #category : #'as yet unclassified' }
SymbolTable >> kindOf: name [
	| result |
	result := 'none'.
	(functionScope keys includes: name) ifTrue: [ result := ((functionScope at: name ) at: 2) asString .].
	(classScope  keys includes: name) ifTrue: [ result := ((classScope  at: name ) at: 2) asString .].
	^result.
]

{ #category : #'as yet unclassified' }
SymbolTable >> startSubroutine [
	functionScope := Dictionary new.
	counts at: 'argument' put: 0.
	counts at: 'local' put: 0.
	
]

{ #category : #private }
SymbolTable >> typeOf: name [
	| result |
	result := 'none'.
	(functionScope keys includes: name) ifTrue: [ result := ((functionScope at: name ) at: 1) asString .].
	(classScope  keys includes: name) ifTrue: [ result := ((classScope  at: name ) at: 1) asString .].
	^result.
]

{ #category : #'as yet unclassified' }
SymbolTable >> varCount: kind [
	 ^ counts at: kind.
]
