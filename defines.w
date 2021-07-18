(define draw 
    (fn (player) 
        (move (card 1) hand)))
(define draw2 
    (fn (player) 
        (seq 
            (draw player) 
            (draw player))))