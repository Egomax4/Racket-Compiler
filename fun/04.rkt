(define (hopup [curstep:Integer] [stepnum:Integer] [target:Integer]):Integer
    (if (eq? curstep target) 
        stepnum 
        (begin
            (set! curstep (+ curstep 5))
            (if (< curstep target)
                (hopup curstep (+ 1 stepnum) target)
                (hopdown curstep (+ 1 stepnum) target)))))

(define (hopdown [curstep:Integer] [stepnum:Integer] [target:Integer]):Integer
    (if (eq? curstep target) 
        stepnum 
        (begin
            (set! curstep (- curstep 2))
            (if (< curstep target)
                (hopup curstep (+ 1 stepnum) target)
                (hopdown curstep (+ 1 stepnum) target)))))

(hopup 0 0 12)