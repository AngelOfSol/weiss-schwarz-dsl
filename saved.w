          ~       type error: mismatched types
 --> <editor>:4:2
  | 
  | (card 
  |  ^^^^ expected: fn(i32, i32) -> @1, found: fn(i32) -> card
   �       type error: mismatched types
 --> externs.w:1:17
  | 
  | (extern card fn(i32
  |                 ^^^ expected: bool, found: i32
   �       type error: mismatched types
 --> externs.w:1:17
  | 
  | (extern card fn(i32
  |                 ^^^ expected: bool, found: i32
   �       type error: mismatched types
 --> externs.w:1:17
  | 
  | (extern card fn(i32
  |                 ^^^ expected: bool, found: i32
   �       type error: mismatched types
 --> externs.w:1:17
  | 
  | (extern card fn(i32
  |                 ^^^ expected: bool, found: i32
   �       type error: mismatched types
 --> externs.w:1:17
  | 
  | (extern card fn(i32
  |                 ^^^ expected: bool, found: i32
   �       type error: mismatched types
 --> externs.w:1:17
  | 
  | (extern card fn(i32
  |                 ^^^ expected: bool, found: i32
   �       type error: mismatched types
 --> externs.w:1:17
  | 
  | (extern card fn(i32
  |                 ^^^ expected: bool, found: i32
   �       type error: mismatched types
 --> <editor>:4:7
  | 
  | (card (if true true true)
  |       ^^^^^^^^^^^^^^^^^^^ expected: i32, found: bool
   ~       type error: mismatched types
 --> <editor>:4:2
  | 
  | (card 
  |  ^^^^ expected: fn(i32) -> card, found: fn(i32, i32) -> @1
   ~       type error: mismatched types
 --> <editor>:4:2
  | 
  | (card 
  |  ^^^^ expected: fn(i32) -> card, found: fn(i32, i32) -> @1
   �       type error: mismatched types
 --> <editor>:4:1
  | 
  | (card 1 2)
  | ^^^^^^^^^^ expected: fn(i32) -> card, found: fn(i32, i32) -> @1
   x       type error: mismatched types
 --> <editor>:4:7
  | 
  | (card card
  |       ^^^^ expected: i32, found: fn(i32) -> card
   �       type error: mismatched types
 --> <editor>:4:12
  | 
  | (if true 1 card
  |            ^^^^ expected: i32, found: fn(i32) -> card
   �       type error: mismatched types
 --> <editor>:4:12
  | 
  | (if true 1 card
  |            ^^^^ expected: i32, found: fn(i32) -> card
   �       type error: mismatched types
 --> <editor>:4:12
  | 
  | (if true 1 card
  |            ^^^^ expected: i32, found: fn(i32) -> card
   �       type error: mismatched types
 --> <editor>:4:1
  | 
  | (card true 2)
  | ^^^^^^^^^^^^^ expected: fn(i32) -> card, found: fn(bool, i32) -> @1
   m       type error: mismatched types
 --> <editor>:4:7
  | 
  | (card true
  |       ^^^^ expected: i32, found: bool
               �       type error: mismatched types
 --> <editor>:5:7
  | 
  | (card false
  |       ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (card deck
  |       ^^^^ expected: i32, found: zone
E       (include "externs.w")
(include "defines.w")

(card deck)
(card false)7       	load 4: i32
	call card
	load 5: i32
	call card
	return                                                                                                                                              
                            	                             4          3          2                                 2       3       4                  	           
                                                                                                        