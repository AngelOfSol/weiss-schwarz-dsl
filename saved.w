                  1: card           1: card           1: card           1: card           1: card           1: card           1: card   P       type error: invalid type
code:9:22
	expected: fn() -> @8
	found: fn(@8) -> card            1: card           1: card           1: card           1: card           1: card   8       symbol error: invalid symbol: recursive at line 9, col 9   8       symbol error: invalid symbol: recursive at line 7, col 9   8       symbol error: invalid symbol: recursive at line 7, col 9   8       symbol error: invalid symbol: recursive at line 7, col 9           1: i32           1: i32 2: i32   <       type error: multiple array types
code:11:8
	found: i32, bool�       (extern card fn(i32) -> card)
(extern print fn(T) -> T)
(extern move fn(card, zone) -> zone)
(extern some fn(T) -> ?T)
(extern or_default fn(?T, T) -> T)

(define recursive (fn ()
	(if true
		1 (recursive))))

(print [true 2])�       	load 1: i32
	load 2: i32
	load 2: array_length
	print
	return
'recursive:
	load true: bool
	jump-if-true 'if-true-0
	call recursive
	jump 'end-if-0
'if-true-0:
	load 1: i32
'end-if-0:
	return                                                                                                                                                    
                                 	                  4          3          2                                                       
                                 2       3       4                                                   	                           