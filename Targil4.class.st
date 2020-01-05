Class {
	#name : #Targil4,
	#superclass : #Object,
	#instVars : [
		'counter'
	],
	#category : #Ex1
}

{ #category : #'as yet unclassified' }
Targil4 >> ParseDoStatement: read [
	|cr result|
	cr := Character cr.
	cr := cr asString.
	result := '<doStatement>',cr,(read at: counter) "do",cr.
	counter := counter + 1.
	result := result, (self parseSubRoutineCall: read) asString ,(read at: counter) asString";",cr.
	counter := counter + 1.
	result := result,'</doStatement>',cr.
	^result . 
	
]

{ #category : #'as yet unclassified' }
Targil4 >> ParseExpression: read [
	| line cr result ops word|
	ops := #(' + ' ' - ' ' * ' ' / ' ' &amp; ' ' | ' ' &gt; ' ' &lt; ' ' = ') asArray .
	cr := Character cr.
	cr := cr asString.
	result := '<expression>',cr.
	result := result,( self parseTerm: read) asString .
	
	[ line := read at: counter .
	word := self getXMLElement: line.ops includes: word ] 
			whileTrue: [ result := result,line,cr.counter := counter+1.result := result,(self parseTerm: read) asString.].
	result := result,'</expression>',cr.
	^result.
	
]

{ #category : #'as yet unclassified' }
Targil4 >> ParseIfStatement: read [
| cr line result word|
cr := Character cr.
cr := cr asString .
result := '<ifStatement>',cr,(read at: counter) "if",cr. 
counter := counter + 1.
result := result,(read at: counter) "(",cr.
counter := counter +1.
result := result,(self ParseExpression: read) asString . " expression"
result := result,(read at: counter) ")",cr.
counter := counter + 1.
result := result,(read at: counter) "{",cr.
counter  := counter + 1.
result := result, (self ParseStatements: read) asString ,(read at: counter ) "}",cr .
counter := counter + 1.
line := read at: counter.
word := self getXMLElement: line.
word = ' else ' ifTrue: [ result := result,(read at: counter) "else",cr.counter := counter + 1.result := result,(read at: counter)"{",cr.
								 counter  := counter + 1. result := result,(self ParseStatements: read),(read at: counter)"}",cr. counter := counter + 1. 
								].
result := result,'</ifStatement>',cr.
^result.
]

{ #category : #'as yet unclassified' }
Targil4 >> ParseReturnKey: read [
	|cr result line word|
	cr := Character cr.
	cr := cr asString.
	result := '<returnStatement>',cr,(read at: counter) "return",cr.
	counter := counter + 1.
	line := read at: counter.
	word := self getXMLElement: line.
	word = ' ; ' ifFalse: [ result := result,(self ParseExpression: read) asString .line := read at: counter.].
	result := result,line,cr,'</returnStatement>',cr.
	counter := counter + 1.
	^result.
]

{ #category : #'as yet unclassified' }
Targil4 >> ParseStatement: read [
| str line cr result|
result := ''.
cr := Character cr.
cr := cr asString .
line := read at: counter .
str := self getXMLElement: line.
str = ' let ' ifTrue: [ result := result, (self parseLetStatement: read) asString . ].
str = ' if ' ifTrue: [ result := result, (self ParseIfStatement: read) asString .  ].
str = ' while ' ifTrue: [ result := result,cr,(self parseWhileStatement: read) asString . ].
str = ' do ' ifTrue: [ result := result,cr,(self ParseDoStatement: read) asString . ].
str = ' return ' ifTrue: [  result := result,cr,(self ParseReturnKey: read) asString . ].
^result.
]

{ #category : #'as yet unclassified' }
Targil4 >> ParseStatements: read [
| cr result line element|
cr := Character cr.
cr := cr asString.
result:= '<statements> '.
line := read at: counter .
element := self getXMLElement: line.
[ element = ' } ' ] whileFalse: [ result := result,(self ParseStatement: read) asString ,cr. line := read at: counter. element := self getXMLElement: line. ].
result := result,'</statements>',cr.
^result.
]

{ #category : #accessing }
Targil4 >> createTokenXML: path [
| cr symbols p jacks c |
cr := Character cr.
cr := cr asString .
symbols := #( '{' '}' '(' ')' '[' ']' '.' ',' ';' '+' '-' '*' '/' '&' '|' '<' '>' '=' '~') asArray .
p := FileLocator home / path.
jacks := p allChildren select: [ :arg2 | arg2 basename endsWith: 'jack' ].
c:= jacks size .
[ c = 0 ]
whileFalse: 
[ 
|fName streamWrite split fileRef streamRead| 
fName:= (jacks at: c ) basename .split:= $. split: fName. fName := (split at: 1), 'T.xml'. streamWrite :=(FileLocator home / path / fName ) writeStream.
fileRef := (jacks at: c) asFileReference . streamRead := fileRef readStream .
c := c-1.
streamWrite nextPutAll: '<tokens>',cr .
[ streamRead atEnd  ]
whileFalse: 
[
|line buff currChar lineSize i lineBeginning toWrite|
line := streamRead nextLine. lineSize := line size. buff := ''. i := 1. 
line size >=2 ifTrue:[lineBeginning := (line at: 1) asString ,(line at: 2) asString .].
[ ((lineBeginning = '//') | (lineBeginning = '/*') ) & (streamRead atEnd not)  ] whileTrue: [ line := streamRead nextLine. lineSize := line size. line size >=2 ifTrue:[lineBeginning := (line at: 1) asString ,(line at: 2) asString .].].
"we got here only if the line is not a comment"
toWrite := ''.
[ i = (lineSize + 1)  ]
whileFalse: 
[
currChar := line at: i.
(currChar = Character space) | ( currChar = Character tab  ) ifTrue: [ toWrite := toWrite , (self tokenize: buff). buff := ''.]
ifFalse: [ (symbols includes: (currChar asString) ) 
ifTrue: [ toWrite := toWrite , (self tokenize: buff). toWrite := toWrite , (self tokenize: currChar asString) . buff :='' ]
ifFalse: [ currChar = $" ifTrue: [ |s| toWrite := toWrite , (self tokenize: buff). buff :=''. i := i+1. s:=''.
[(line at: i) = $"] whileFalse: [ s:=s,(line at: i)asString.i:=i+1. ].
toWrite := toWrite ,'<stringConstant> ',s,' </stringConstant>',cr. ] 
ifFalse: [ buff:= buff,currChar asString.]].
].
i := i+1.
  ].
streamWrite nextPutAll: toWrite.
].
streamWrite nextPutAll: '</tokens>'.
streamWrite close.
streamRead close.
].



]

{ #category : #'as yet unclassified' }
Targil4 >> getTokenType: line [
	"comment stating purpose of message"
	| result tmp|
"line:='<identifier> className </identifier>'."
tmp:= $> split: line.
tmp:= tmp at: 1.
tmp:= $< split: tmp.
result := tmp at: 2.
^result.
]

{ #category : #'as yet unclassified' }
Targil4 >> getXMLElement: str [
| result split temp |
split := $> split: str.
temp := split at: 2.
split := $< split: temp.
result := split at: 1.
^result.
]

{ #category : #'as yet unclassified' }
Targil4 >> getXMLElementTrim: str [
	| result |
	result := self getXMLElement: str.
	result := result trimRight .
	result := result trimLeft .
	^result.
]

{ #category : #accessing }
Targil4 >> main: path [
	"comment stating purpose of message"

	| |
	
	self createTokenXML: path.
	self parser: path.
	
	
	
]

{ #category : #parsing }
Targil4 >> parseClass: read [ 
	"comment stating purpose of message"
	| cr result line word id tmp |
	cr:=Character cr.
	cr:= cr asString.
	counter := 2. 
	result := '<class>', cr,(read at: counter) asString ,cr. "<identifier> class </identifier>"
	counter :=counter +1.
	result:= result,(read at: counter) asString,cr. "<identifier> className </identifier>"
	counter :=counter +1.
	result:= result,(read at: counter) asString,cr. "<symbol> { </symbol>"
	counter := counter + 1.
	tmp:= counter .
	line:= read at: (counter )   . " static | field| constructor| function |method  }"
	word:= self getXMLElement: line.
	[(word =' static ') | (word= ' field ') ]
		whileTrue: [ result := result , (self parseClassVarDec: read) asString,cr.  line := (read at: counter) asString ." static | field| int| char |boolean |    className | }" word:= self getXMLElement: line. ] .
	"id:= self getTokenType: line."
	[ (word =' constructor ')| (word=' function ')|(word=' method ') ]
		whileTrue: [  result := result, (self parseSubroutineDec: read) asString ,cr.  line:= (read at: counter) asString . "int| char |boolean | className | }"
			word:= self getXMLElement: line. "id= self getTokenType: line." ].
	line:= (read at: counter) asString  . "symbol> } </symbol>"
		result := result, line,  cr, '</class>',cr.
	^result.
	

	


]

{ #category : #parsing }
Targil4 >> parseClassVarDec: read [
	"comment stating purpose of message"
   |  cr result line word |
	cr:=Character cr.
	cr:= cr asString.
	result:= '<classVarDec>',cr.
	line:= read at: counter . "static | field"
	result := result , line, cr. 
	counter := counter +1. line:= read at: counter. "type"
	result := result , line,cr.
	counter := counter +1. line:= read at: counter. "varName"
	result := result , line,cr.
	counter := counter + 1.
	"word:= self getXMLElement: (read at: counter+1). " ", | ;"
	[ ( self getXMLElement: (read at: counter)) = ' , ' ]
		whileTrue: [ line:= read at: counter. "," result := result , line,cr.
			counter := counter +1. line:= read at: counter. "varName"result := result , line,cr. counter := counter + 1. ].
	line:= read at: counter. ";"
	counter := counter + 1.
	result := result , line,cr,'</classVarDec>',cr.
	^result .
	
	
]

{ #category : #parsing }
Targil4 >> parseExpressionList: read [
	|cr result line word|
	cr := Character cr.
	cr := cr asString.
	result := '<expressionList>',cr.
	line := read at: counter.
	word := self getXMLElement: line.
	word = ' ) ' ifFalse: [ result := result,(self ParseExpression: read) asString .
								   line := read at: counter. word := self getXMLElement: line.
									[ word = ' , ' ] whileTrue: [ result := result,line,cr.counter := counter + 1.result := result,(self ParseExpression: read) asString.
																			line := read at: counter. word := self getXMLElement: line.] ].
	
	result := result,'</expressionList>',cr.
	^result.
]

{ #category : #parsing }
Targil4 >> parseLetStatement: read [
| result cr line str |
result := '<letStatement>'.
cr := Character cr.
cr := cr asString.
result := result,(read at: counter ),cr. "let"
counter := counter +1.
result := result,(read at: counter ),cr. "identifier=varName"
counter := counter + 1.
line := read at: counter. " [ or ="
str := self getXMLElement: line.
"counter := counter +1."
str = ' [ ' ifTrue: [ result:=result,line,cr.counter := counter +1.result:=result,(self ParseExpression: read) asString,cr. result := result,(read at: counter ) "]",cr. counter := counter +1. line := read at: counter . "="  ] .
result := result,cr, line.
counter := counter + 1.
result := result,cr, (self ParseExpression: read) asString ,cr.
line := read at: counter . " ;"
result := result,cr,line,'</letStatement>',cr.
counter := counter + 1. 
^result.
]

{ #category : #parsing }
Targil4 >> parseParameterList: read [
	"comment stating purpose of message"
| cr result line word id|
	cr:=Character cr.
	cr:= cr asString.
	result:= '<parameterList>',cr.
	line := read at: counter.
	word := self getXMLElement: line.
	id := self getTokenType: line.
	( (word=' int ') | (word = ' char ') | (word = ' boolean ') | (id = 'identifier'))
	ifTrue: [ counter := counter + 1.
	result := result,line,cr."type"
	line := read at: counter . counter := counter + 1.
	result := result,line,cr."varName"
	[ line := read at: counter. word:= self getXMLElement: line. word = ' , ']
		whileTrue: [ result:=result,line,cr",".counter := counter + 1.
						result := result,(read at: counter) asString ,cr"type".counter := counter + 1.
						result := result,(read at: counter) asString"varName",cr.counter:=counter + 1. ]. ].
	
	result:= result , '</parameterList>',cr.
	^result .
	
]

{ #category : #parsing }
Targil4 >> parseSubRoutineCall: read [
	|cr result line word|
	cr := Character cr.
	cr := cr asString.
	line := read at: counter.
	result := line,cr. "identifier" 
	counter := counter+1.
	line := read at: counter.
	word := self getXMLElement: line.
	word = ' ( ' ifTrue: [ result := result,line,cr"(". counter := counter + 1.
									result := result,(self parseExpressionList: read) asString,(read at: counter) asString")",cr.counter := counter + 1.^result. ].
	word = ' . ' ifTrue: [ result:=result,line,cr.counter := counter + 1."."
									result := result,(read at: counter)"subRoutinName",cr.counter := counter+1.result := result,(read at: counter) asString "(",cr.counter := counter + 1.
									result := result,(self parseExpressionList: read) asString,(read at: counter) asString")",cr.counter := counter+1.^result. ].
								
]

{ #category : #parsing }
Targil4 >> parseSubroutineBody: read [
	| cr result1 line word |
	cr:=Character cr.
	cr:= cr asString.
	result1:= '<subroutineBody>',cr.
	line:= read at: counter . "{"
	counter := counter + 1.
	result1 := result1 , line, cr.
	line := read at: counter. word :=(self getXMLElement: line).
	[  word = ' var ' ]
		whileTrue: [ |tmp| tmp := self parseVarDec: read.result1 := result1 ,tmp, cr. line := read at: counter. word :=(self getXMLElement: line).].
	result1 := result1 , (self ParseStatements: read) asString,cr.
	line:= read at: counter . "}"
	counter := counter + 1.
	result1 := result1 , line, cr, '</subroutineBody>',cr.
	^result1 .
	
]

{ #category : #parsing }
Targil4 >> parseSubroutineDec: read [
	|  cr result line word id  |
	cr:=Character cr.
	cr:= cr asString.
	result:= '<subroutineDec>',cr.
	line:= read at: counter . "constructor | function | method"
	result := result , line, cr.
	counter := counter +1. line:= read at: counter . "void | type"
	result := result , line, cr.
	counter := counter +1. line:= read at: counter . "subroutine name"
	result := result , line, cr.
	counter := counter +1. line:= read at: counter . "("
	result := result , line, cr.
	counter := counter + 1.
	result := result,( self parseParameterList: read).
	line:= read at: counter ")". counter := counter + 1.
	result := result , line, cr, (self parseSubroutineBody: read) asString, '</subroutineDec>',cr.
	^result .
	
	

	
]

{ #category : #parsing }
Targil4 >> parseTerm: read [
	|cr result line type word nextLine|
	cr := Character cr.
	cr := cr asString.
	result := '<term>',cr.
	line := read at: counter .
	type := self getTokenType: line.
	word := self getXMLElement: line.
	type = 'integerConstant' ifTrue: [ result := result,line,cr,'</term>',cr.counter:=counter+1.^result. ].
	type = 'stringConstant' ifTrue: [ result := result,line,cr,'</term>',cr.counter:=counter+1.^result ].
	type = 'keyword' ifTrue: [ result:=result,line,cr,'</term>',cr.counter := counter+1.^result. ].
	((word = ' - ' )|( word = ' ~ ')) ifTrue: [ result := result,line,cr.counter := counter + 1. result := result, (self parseTerm: read) asString ,'</term>',cr.^result ].
	word = ' ( ' ifTrue: [ result:=result,line,cr.counter:=counter+1.result:=result,(self ParseExpression: read) asString.result:=result,(read at: 								counter) asString")".result:=result,'</term>',cr.counter:=counter+1.^result ].
	nextLine := read at: (counter+1).
	nextLine := self getXMLElement: nextLine .
	nextLine = ' [ ' ifTrue: [ result := result,line"varName",cr.counter:=counter+1. result:=result,(read at:counter) asString "[",cr.counter :=counter+1.
											result:=result,(self ParseExpression: read) asString ,(read at: counter) asString "]",cr,'</term>',cr.counter := counter+1.^result.].
	(nextLine = ' ( ' | nextLine = ' . ') ifTrue: [ result:=result,(self parseSubRoutineCall: read) asString ,'</term>',cr.^result. ].
	result := result,line"varName alone",cr,'</term>',cr.
	counter:=counter+1.
	^result.
]

{ #category : #parsing }
Targil4 >> parseVarDec:read [
	"comment stating purpose of message"
| cr result line word |
	cr:=Character cr.
	cr:= cr asString.
	result:= '<varDec>',cr.
	line:= read at: counter . "var"
	result := result , line, cr.
	counter := counter +1. line:= read at: counter . "type"
	result := result , line, cr.
	counter := counter +1. line:= read at: counter . "var name"
	counter := counter + 1.
	result := result , line, cr.
	[ line := read at: counter .( self getXMLElement: line) asString =' , ' ]
		whileTrue: [   counter := counter +1.result := result , line,cr.
						 line:= read at: counter. "varName" counter := counter +1.result := result , line,cr. ].
	result := result , line,cr,'</varDec>',cr.
	counter := counter + 1.
	^result .	
	
]

{ #category : #parsing }
Targil4 >> parseWhileStatement: read [
	|cr result line |
	cr := Character cr.
	cr := cr asString .
	result := '<whileStatement>',cr,(read at: counter)"while",cr.
	counter := counter + 1.
	result := result,(read at: counter)"(",cr. counter := counter + 1.
	result := result , (self ParseExpression: read) asString .
	result := result,(read at: counter) asString  ")",cr. counter := counter + 1.
	result := result,(read at: counter) asString "{",cr. counter := counter + 1.
	result := result,(self ParseStatements: read) asString ,(read at: counter) "}",cr. counter := counter + 1.
	result := result,'</whileStatement>'.
	^result.
]

{ #category : #accessing }
Targil4 >> parser: path [
		|result   xmlt p s|
		p := FileLocator home / path.
      xmlt:= p allChildren select: [ :arg2 | arg2 basename endsWith: 'T.xml' ].
		s:= xmlt size .
		[ s=0 ]
			whileFalse:[
				|fName split streamWrite read n|
				fName:= (xmlt  at: s) basename .split:= $T split: fName. fName := (split at: 1), '.xml'.
				streamWrite :=(FileLocator home / path / fName ) writeStream.
				n:=path,'\' ,((xmlt  at: s) basename ).
				read:=  (StandardFileStream oldFileNamed: n ) contents.
				read:= Character cr split: read.
				result := self parseClass: read.
				streamWrite nextPutAll: result .
				s:=s-1.
				streamWrite close .			
				].
	
	
	
	

		
	
		
]

{ #category : #accessing }
Targil4 >> parser: fileToken and: oldPath [
		|result read path|
		path:=oldPath asString , '\',fileToken asString .
		read:=  (StandardFileStream oldFileNamed: path) contents.
		result:= self parseClass: read.
		^result .
		
		
]

{ #category : #parsing }
Targil4 >> tokenize: str [
| cr result symbols keywords|
cr := Character cr.
cr := cr asString .
result := ''.
symbols := #( '{' '}' '(' ')' '[' ']' '.' ',' ';' '+' '-' '*' '/' '&' '|' '<' '>' '=' '~') asArray .
keywords  := #( 'class' 'constructor' 'function' 'method' 'field' 'static' 'var' 'int' 'char' 'boolean' 'void' 'true' 'false' 'null' 'this' 'let' 'do' 'if' 'else' 'while' 'return') asArray .
(keywords includes: str) ifTrue: [ result := '<keyword> ',str asString,' </keyword>',cr. ^result ].
(str asString isAllDigits ) ifTrue: [ result := '<integerConstant> ',str asString, ' </integerConstant>',cr. ^result ].
(symbols includes: str) ifTrue: [ 
result := '<symbol> '. str = '<' ifTrue: [ result := result,'&lt;',' </symbol>',cr. ^result ].
str asString = '>' ifTrue: [ result := result,'&gt;',' </symbol>',cr.^result ].
str asString = '&' ifTrue: [ result := result ,'&amp;',' </symbol>',cr. ^result  ].
result := result,str asString ,' </symbol>',cr. ^result ].
(str size > 0 ) ifTrue: [result := '<identifier> ',str asString ,' </identifier>',cr.].
^result .											

]
