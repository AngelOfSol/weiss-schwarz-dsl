          �      Parsing Failure: VerboseError { errors: [(LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Nom(Tag)), (LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Context("expected \")\""))] }   �      Parsing Failure: VerboseError { errors: [(LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Nom(Tag)), (LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Context("expected \")\""))] }   �      Parsing Failure: VerboseError { errors: [(LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Nom(Tag)), (LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Context("expected \")\""))] }   �      Parsing Failure: VerboseError { errors: [(LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Nom(Tag)), (LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Context("expected \")\""))] }   �       	error: nom error:
	<editor>:4:10
	(fn (x) 2
	         ^ nom error

	error: expected ")":
	<editor>:4:10
	(fn (x) 2
	         ^ expected ")"
   �      Parsing Failure: VerboseError { errors: [(LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Nom(Tag)), (LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Context("expected \")\""))] }   �      Parsing Failure: VerboseError { errors: [(LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Nom(Tag)), (LocatedSpan { offset: 43, line: 3, fragment: "move (card 1) hand)))\r\n(define draw2 \r\n    (fn (player) \r\n        (seq \r\n            (draw player) \r\n            (draw player))))", extra: "defines.w" }, Context("expected \")\""))] }   �       	error: nom error:
	<editor>:5:4
			(move (card 1) hand)))
	   ^ nom error

	error: expected ")":
	<editor>:5:4
			(move (card 1) hand)))
	   ^ expected ")"
   �       	error: nom error:
	<editor>:3:28
	(define draw (fn (player) (move (card 1) hand)))
	                           ^ nom error

	error: expected ")":
	<editor>:3:28
	(define draw (fn (player) (move (card 1) hand)))
	                           ^ expected ")"
                                                                                                                                    @       (include "externs.w")
(include "defines.w")

(+ (+ (+ 2 2) 2) 2)*      	load 2: i32
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
	return                                                  
                                                  	                                                                                                   3          4          2                                 	                      
                                            2       3       4                                                                   