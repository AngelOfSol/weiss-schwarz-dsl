          p       type error: mismatched types
	defines.w:12:15
	        (card true
	              ^^^^ expected: i32, found: bool   �       	error: nom error:
	<editor>:4:8
	(print [1 1 true)
	       ^ nom error

	error: expected ")":
	<editor>:4:8
	(print [1 1 true)
	       ^ expected ")"
   �       	error: nom error:
	<editor>:4:17
	(print [1 1 true)
	                ^ nom error

	error: expected "]":
	<editor>:4:17
	(print [1 1 true)
	                ^ expected "]"
   �       parsing error: nom error
	<editor>:4:17
	(print [1 1 true)
	                ^ nom error

parsing error: expected "]"
	<editor>:4:17
	(print [1 1 true)
	                ^ expected "]"
   p       type error: mismatched types
	defines.w:12:15
	        (card true
	              ^^^^ expected: i32, found: bool   s       type error: mismatched types
--> defines.w:12:15
	        (card true
	              ^^^^ expected: i32, found: bool   t       type error: mismatched types
 --> defines.w:12:15
	        (card true
	              ^^^^ expected: i32, found: bool   t       type error: mismatched types
 --> defines.w:12:15
	        (card true
	              ^^^^ expected: i32, found: bool   v       type error: mismatched types
 --> defines.w:12:15
|
|        (card true
|              ^^^^ expected: i32, found: bool   |       type error: mismatched types
 --> defines.w:12:15
  |
  |        (card true
  |              ^^^^ expected: i32, found: bool   �       parsing error: nom error
	<editor>:4:17
	(print [1 1 true)
	                ^ nom error

parsing error: expected "]"
	<editor>:4:17
	(print [1 1 true)
	                ^ expected "]"
   �       parsing error: nom error
 --> <editor>:4:17
  |
  |(print [1 1 true)
  |                ^ nom error

parsing error: expected "]"
 --> <editor>:4:17
  |
  |(print [1 1 true)
  |                ^ expected "]"
   �       parsing error: nom error
 --> <editor>:4:17
  |
  | (print [1 1 true)
  |                 ^ nom error

parsing error: expected "]"
 --> <editor>:4:17
  |
  | (print [1 1 true)
  |                 ^ expected "]"
   ~       type error: mismatched types
 --> defines.w:12:15
  |
  |         (card true
  |               ^^^^ expected: i32, found: bool   {       type error: invalid array
 --> <editor>:4:8
  |
  | (print [1 1 true]
  |        ^^^^^^^^^^ found multiple types: i32, bool   }       type error: invalid array
 --> <editor>:4:8
  | 
  | (print [1 1 true]
  |        ^^^^^^^^^^ found multiple types: i32, bool
   }       type error: invalid array
 --> <editor>:4:8
  | 
  | (print [1 1 true]
  |        ^^^^^^^^^^ found multiple types: i32, bool
   }       type error: invalid array
 --> <editor>:4:8
  | 
  | (print [1 1 true]
  |        ^^^^^^^^^^ found multiple types: i32, bool
   �       parsing error: nom error
 --> <editor>:4:17
  | 
  | )
  |                 ^ nom error

parsing error: expected "]"
 --> <editor>:4:17
  | 
  | )
  |                 ^ expected "]"
   �       parsing error: nom error
 --> <editor>:4:17
  | 
  | (print [1 1 true)
  |                 ^ nom error

parsing error: expected "]"
 --> <editor>:4:17
  | 
  | (print [1 1 true)
  |                 ^ expected "]"
>       (include "externs.w")
(include "defines.w")

(print [1 1 true)*      	load 2: i32
	load 2: i32
	load 2: i32
	load 2: i32
	call +
	call +
	call +
	return
'draw:
	store @player-1
	load hand: zone
	load 1: i32
	call card
	call move
	unload @player-1
	return
'draw2:
	store @player-1
	load-ref @player-1
	call draw
	load-ref @player-1
	call draw
	unload @player-1
	return                                                                                                                                                                          	           
                  2           4          3                     	                                 2       3       4                                                                                    
                           