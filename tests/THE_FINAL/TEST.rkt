(define (even [n : Integer]) : Boolean
  (if (<= n 0)
      (eq? n 0)
      (odd (- n 1))))

(define (odd [n : Integer]) : Boolean
  (if (<= n 0)
      #f
      (even (- n 1))))

(define (dec [x : Integer]) : Integer
  (- x 2))

(define (check_loop [v : (Vector Integer Boolean (Integer -> Integer) (Vector Integer Integer Integer Integer))]) : Boolean
  (let ([e0 (vector-ref (vector-ref v 3) 0)])
    (let ([e1 (vector-ref (vector-ref v 3) 1)])
      (let ([e2 (vector-ref (vector-ref v 3) 2)])
        (let ([e3 (vector-ref (vector-ref v 3) 3)])
          (if (and (> e0 0) (even e0))
              #t
              (if (and (> e1 0) (even e1))
                  #t
                  (if (and (> e2 0) (even e2))
                      #t
                      (if (and (> e3 0) (even e3))
                          #t
                          #f)))))))))

(define (process [v : (Vector Integer Boolean (Integer -> Integer) (Vector Integer Integer Integer Integer))])
  : (Vector Integer Boolean (Integer -> Integer) (Vector Integer Integer Integer Integer))
  (let ([flag #f])
    (begin
      (vector-set! v 0 (+ (vector-ref v 0) 10))
      (vector-set! v 1 (not (vector-ref v 1)))
      (let ([newflag (check_loop v)])
        (begin
          (vector-set! v 1 newflag)
          v)))))

(let ([a (read)])
  (let ([c1 (read)])
    (let ([c2 (read)])
      (let ([c3 (read)])
        (let ([c4 (read)])
          (let ([inner (vector c1 c2 c3 c4)])
            (let ([outer (vector a #f dec inner)])
              (let ([outer2 (process outer)])
                (let ([val (vector-ref outer2 0)])
                  (if (or (vector-ref outer2 1) (>= val 15))
                      (dec val)
                      4))))))))))