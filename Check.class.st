Class {
	#name : #Check,
	#superclass : #Object,
	#instVars : [
		'x'
	],
	#category : #Ex1
}

{ #category : #'as yet unclassified' }
Check >> f [
	"comment stating purpose of message"
^x.
]

{ #category : #'as yet unclassified' }
Check >> factorial: n [
	"returns n!"
	|tmp |
	n = 0 ifTrue: [ ^1 ].
	tmp := n-1.
	^ n * (self factorial: tmp)
]

{ #category : #'as yet unclassified' }
Check >> paramPassing: a and: b [
 Transcript show: 'first argument: ', a asString , 'second argument: ', b asString .
]

{ #category : #printing }
Check >> print: str [
	" prints str to Transcript"
	Transcript show: str.
]

{ #category : #accessing }
Check >> read2: file [
	"comment stating purpose of message"
	|line |
	line := file  nextLine.
	Transcript show: line.

]

{ #category : #'as yet unclassified' }
Check >> readCheck: path [
	"comment stating purpose of message"
	| p fileref streamRead line cr|
	cr:=Character cr.
	cr:= cr asString.
	p := FileLocator home / path.
	fileref := p asFileReference .
	streamRead := fileref readStream  .
	line := streamRead  nextLine.
	Transcript show: line.
		Transcript show: cr.
	self read2: streamRead .
	
	
]

{ #category : #'as yet unclassified' }
Check >> sum: x and: y [
  "returns x+y"
	^x+y.
]
