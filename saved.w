          ~       type error: mismatched types
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
          missing heap value name: print          invalid fn name "print"   .       attempted to pop an argument on an empty stack           1: i32 1: i32           3: i32   1      parsing error: nom error
 --> <editor>:4:34
  | 
  | (print (let ((x (fn (x) (+ x 1)) (x 2)))
  |                                  ^^^^^^^ nom error

parsing error: expected ")"
 --> <editor>:4:34
  | 
  | (print (let ((x (fn (x) (+ x 1)) (x 2)))
  |                                  ^^^^^^^ expected ")"
   -      parsing error: nom error
 --> <editor>:4:34
  | 
  | (print (let ((x (fn (x) (+ x 1)) (x 2))
  |                                  ^^^^^^ nom error

parsing error: expected ")"
 --> <editor>:4:34
  | 
  | (print (let ((x (fn (x) (+ x 1)) (x 2))
  |                                  ^^^^^^ expected ")"
   �      parsing error: nom error
 --> <editor>:4:41
  | 
  | (print (let ((x (fn (x) (+ x 1))) (x 2))
  |                                          nom error

parsing error: expected "("
 --> <editor>:4:41
  | 
  | (print (let ((x (fn (x) (+ x 1))) (x 2))
  |                                          expected "("

parsing error: nom error
 --> <editor>:4:41
  | 
  | (print (let ((x (fn (x) (+ x 1))) (x 2))
  |                                          nom error
   �      parsing error: nom error
 --> <editor>:4:41
  | 
  | (print (let ((x (fn (x) (+ x 1))) (x 2)))
  |                                         ^ nom error

parsing error: expected "("
 --> <editor>:4:41
  | 
  | (print (let ((x (fn (x) (+ x 1))) (x 2)))
  |                                         ^ expected "("

parsing error: nom error
 --> <editor>:4:41
  | 
  | (print (let ((x (fn (x) (+ x 1))) (x 2)))
  |                                         ^ nom error
           3: i32           label@x
3: i32                        i       (include "externs.w")
(include "defines.w")

(seq
	(let ((x (fn () 1))) (x))
	(let ((x (fn () 2))) (x))
)�      	load draw
	store @draw
	load draw2
	store @draw2
	load x
	store @x
	call x
	unload @x
	load x#0
	store @x
	call x#0
	unload @x
	unload @draw
	unload @draw2
	return
'draw:
	store @player
	load hand: zone
	load 1: i32
	call card
	call move
	unload @player
	return
'draw2:
	store @player
	load-ref @player
	call draw
	load-ref @player
	call draw
	unload @player
	return
'x:
	load 1: i32
	return
'x#0:
	load 2: i32
	return                                                  
                                                                                                             	                                        3          2           4                                                                 2       3       4                  	                                                       
                           