          @       type error: invalid type
code:11:34
	expected: bool
	found: i32    @       type error: invalid type
code:14:11
	expected: bool
	found: i32    @       type error: invalid type
code:14:11
	expected: bool
	found: i32    ?       type error: invalid type
code:15:8
	expected: bool
	found: i32     
       deck: zone   0       symbol error: invalid fn: seq  at line 11, col 1           1: i32
2: i32           1: i32
2: i32           1: i32
2: i32                
       hand: zone   @       type error: invalid type
code:9:42
	expected: zone
	found: card     
       hand: zone   ?       type error: invalid type
code:9:41
	expected: i32
	found: bool    ?       type error: invalid type
code:9:41
	expected: i32
	found: bool     
       hand: zone    
       hand: zone   ?       type error: invalid type
code:14:9
	expected: zone
	found: i32    ?       type error: invalid type
code:15:9
	expected: zone
	found: i32     
       hand: zone*      (extern card fn(i32) -> card)
(extern print fn(T) -> T)
(extern move fn(card, zone) -> zone)
(extern some fn(T) -> ?T)
(extern or_default fn(?T, T) -> T)

(define draw (fn () -> zone (move (card 1) hand)))

(define draw2 (fn () -> zone (seq (card 1) (draw))))


(print
	(seq
		(draw)
		(draw2)
	)
)�       	call draw
	call draw2
	print
	return
'draw:
	load hand: zone
	load 1: i32
	call card
	call move
	return
'draw2:
	load 1: i32
	call card
	call draw
	return                                                                                                                              	                                 
                                        2           3          4                                2       3       4       
                                                       	                                                                       