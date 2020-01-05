Class {
	#name : #VMWriter,
	#superclass : #Object,
	#category : #Ex1
}

{ #category : #writing }
VMWriter >> writeArithmetic: command [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := command asString ,cr.
	^result
]

{ #category : #writing }
VMWriter >> writeCall: name and: nArgs [
	|result cr |
	cr := Character cr.
	cr := cr asString .
	result := 'call ',name asString ,' ',nArgs asString,cr.
	^result
]

{ #category : #writing }
VMWriter >> writeFunction: name and: nLocals [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := 'function ',name asString ,' ', nLocals asString ,cr.
	^result
]

{ #category : #writing }
VMWriter >> writeGoto: label [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := 'goto ',label asString ,cr.
	^result
]

{ #category : #writing }
VMWriter >> writeIf: label [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := 'if-goto ',label asString ,cr.
	^result
]

{ #category : #writing }
VMWriter >> writeLabel: label [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := 'label ',label asString ,cr.
	^result
]

{ #category : #writing }
VMWriter >> writePop: segment and: index [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := 'pop ', segment asString ,' ', index asString ,cr.
	^result
]

{ #category : #writing }
VMWriter >> writePush: segment and: index [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := 'push ', segment asString ,' ', index asString ,cr.
	^result
]

{ #category : #writing }
VMWriter >> writeReturn [
	|result cr|
	cr := Character cr.
	cr := cr asString .
	result := 'return ',cr.
	^result
]
