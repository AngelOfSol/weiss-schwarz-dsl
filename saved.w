                  2: card           2: card           2: card           2: card
2: card           2: card
2: card           2: card
2: card           2: card
2: card           2: card
2: card           2: card
2: card           2: card
2: card           2: card
2: card   �       Parsing Error: Error { input: LocatedSpan { offset: 303, line: 13, fragment: "(fn (x) (print x) 2)\n", extra: "<editor>" }, code: OneOf }   q       Parsing Error: Error { input: LocatedSpan { offset: 324, line: 14, fragment: "", extra: "<editor>" }, code: Tag }           2: i32           2: i32   �       Parsing Error: Error { input: LocatedSpan { offset: 9, line: 1, fragment: "\"externs.w\")\n\n(define omega (fn () 2))\n(define omega2 (fn () 3))\n\n(print 2)\n", extra: "<editor>" }, code: Tag }   +      Parsing Error: VerboseError { errors: [(LocatedSpan { offset: 9, line: 1, fragment: "\"externs.w\")\n\n(define omega (fn () 2))\n(define omega2 (fn () 3))\n\n(print 2)\n", extra: "<editor>" }, Nom(Tag)), (LocatedSpan { offset: 0, line: 1, fragment: "(include \"externs.w\")\n\n(define omega (fn () 2))\n(define omega2 (fn () 3))\n\n(print 2)\n", extra: "<editor>" }, Nom(Alt)), (LocatedSpan { offset: 0, line: 1, fragment: "(include \"externs.w\")\n\n(define omega (fn () 2))\n(define omega2 (fn () 3))\n\n(print 2)\n", extra: "<editor>" }, Nom(Many1))] }   &      Parsing Error: VerboseError { errors: [(LocatedSpan { offset: 255, line: 8, fragment: "", extra: "externs.w" }, Nom(Tag)), (LocatedSpan { offset: 255, line: 8, fragment: "", extra: "externs.w" }, Nom(Alt)), (LocatedSpan { offset: 255, line: 8, fragment: "", extra: "externs.w" }, Nom(Many1))] }           2: i32           2: i327       (include "externs.w")
(include "defines.w")

(print 2)
�       	load 2: i32
	print
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
	return                                                                                                                                                               	                      
                  4          3          2                                 2       3       4                  	                                                                  
                                                 