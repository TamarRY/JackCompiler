Class {
	#name : #Global,
	#superclass : #Object,
	#instVars : [
		'counter'
	],
	#category : #Ex1
}

{ #category : #'as yet unclassified' }
Global >> getCounter [
	"increments the counter and returns the previous value"

	| temp |
	temp:=counter.
	counter:=counter+1.
	^temp.
]

{ #category : #initialization }
Global >> initialize [
	"Subclasses should redefine this method to perform initializations on instance creation" 
	counter:=0.
]
