                  1: card           1: card           1: card           1: card           1: card           1: card           1: card           1: card           1: card           1: card           1: card           1: card           1: card           1: card   P       type error: invalid type
code:9:22
	expected: fn() -> @8
	found: fn(@8) -> card            1: card           1: card           1: card           1: card           1: card      (extern card fn(i32) -> card)
(extern print fn(T) -> T)
(extern move fn(card, zone) -> zone)
(extern some fn(T) -> ?T)
(extern or_default fn(?T, T) -> T)

(define right (fn (x: T) -> U (left 3)))

(define left (fn (y) (card 1)))

(print
	(seq (card 2) (right 1))
)�       	load 2: i32
	call card
	load 1: i32
	call right
	print
	return
'right:
	store @x-0
	load 3: i32
	call left
	unload @x-0
	return
'left:
	store @y-1
	load 1: i32
	call card
	unload @y-1
	return                                                             
                                                           	                                                                               3          2           4                     	                      2       3       4       
                                                                                                                   