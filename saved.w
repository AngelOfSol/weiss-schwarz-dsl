                                   
       hand: zone    
       hand: zone    
       hand: zone    
       deck: zone    
       deck: zone    
       deck: zone    
       hand: zone    
       hand: zone    
       hand: zone    
       hand: zone    
       hand: zone    
       deck: zone    
       deck: zone    
       deck: zone   @       type error: invalid type
code:11:15
	expected: bool
	found: i32    ?       type error: invalid type
code:9:31
	expected: bool
	found: i32    @       type error: invalid type
code:11:15
	expected: bool
	found: i32     
       deck: zone      (extern card fn(i32) -> card)
(extern print fn(T) -> T)
(extern move fn(card, zone) -> zone)
(extern some fn(T) -> ?T)
(extern or_default fn(?T, T) -> T)

(define draw (fn (x y: bool) (move (card 2) deck)))

(define draw2 (fn (x) (draw x x)))

(print (draw2 true))�       	load true: bool
	call draw2
	print
	return
'draw:
	store @x-0
	store @y-1
	load deck: zone
	load 2: i32
	call card
	call move
	unload @x-0
	unload @y-1
	return
'draw2:
	store @x-2
	load-ref @x-2
	load-ref @x-2
	call draw
	unload @x-2
	return                                                                                                                                              
           	                                              3          4          2                                            
                                 2       3       4                                                   	                                      