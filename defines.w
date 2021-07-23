(define map
    (fn (list: [T] f: fn(T) -> U) -> [U]
        (if
            (is_empty? list)
            []
            (cons (f (head list)) (map (tail list) f)))))
(define filter
    (fn (list: [T] f: fn(T) -> bool) -> [T]
        (if 
            (is_empty? list)
            []
            (let ((value (head list))
                (filtered (filter (tail list) f)))
                (if 
                    (f value)
                    (cons value filtered)
                    filtered)))))