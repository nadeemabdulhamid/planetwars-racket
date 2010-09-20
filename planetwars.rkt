#lang racket

(require lang/posn)

; PlanetID = integer
; PlayerID = integer

(define-struct planet
  (id          ; PlanetID
   owner       ; PlayerID
   ship-count  ; integer
   growth-rate ; integer
   posn        ; (posn float float)
   ) #:transparent #:mutable)

(define-struct fleet
  (owner       ; PlayerID
   ship-count  ; integer
   source      ; PlanetID
   destination ; PlanetID
   total-turns ; integer
   turns-left  ; integer
   ) #:transparent #:mutable)

(define-struct planetwars
  (planets    ; [listof planet]
   fleets     ; [listof fleet]
   ) #:transparent #:mutable)




;;=============================================================

#;(define M
#<<XYZ
P 0    0    1 34 2  # Player one's home planet.
P 7    9    2 34 2  # Player two's home planet.
P 3.14 2.71 0 15 5  # A neutral planet with real-number coordinates.

F 1 15 0 1 12 2     # Player one has sent some ships to attack player two.
F 2 28 1 2  8 4     # Player two has sent some ships to take over the neutral planet.
XYZ
  )



; change-owner : planet PlayerID -> planet
(define (change-owner p o)
  (struct-copy planet p [owner o]))

; change-ship-count : planet integer -> planet
(define (change-ship-count p c)
  (struct-copy planet p [ship-count c]))

; add-ships : planet integer -> planet
(define (add-ships p amt)
  (struct-copy planet p [ship-count (+ (planet-ship-count p) amt)]))

(define (remove-ships p amt)
  (struct-copy planet p [ship-count (- (planet-ship-count p) amt)]))

; select-planets : planetwars PlayerID -> (listof planet)
(define (select-planets pw id)
  (filter (λ(p) (= id (planet-owner p))) (planetwars-planets pw)))

; my-planets : planetwars -> (listof planet)
(define (neutral-planets pw) (select-planets pw 0))
(define (my-planets pw) (select-planets pw 1))
(define (enemy-planets pw) (select-planets pw 2))

(define (others-planets pw)   ; not my planets
  (filter (λ(p) (not (= 1 (planet-owner p)))) (planetwars-planets pw)))

(define (select-fleets pw id)
  (filter (λ(f) (= id (fleet-owner f))) (planetwars-fleets pw)))

(define (my-fleets pw) (select-fleets pw 1))
(define (enemy-fleets pw) (select-fleets pw 2))

(define (count-ships pw id)
  (apply +
         (append
          (map planet-ship-count (select-planets pw id))
          (map fleet-ship-count (select-fleets pw id)))))

(define (my-ships pw) (count-ships pw 1))
(define (enemy-ships pw) (count-ships pw 2))


; dist : posn posn -> number
(define (dist p1 p2)
  (let ([dx (- (posn-x p1) (posn-x p2))]
        [dy (- (posn-y p1) (posn-y p2))])
    (sqrt (+ (* dx dx) (* dy dy)))))

; planet-distance : planetwars PlanetID PlanetID -> int
; Returns the distance between two planets, rounded up to the next highest
; integer. This is the number of discrete time steps it takes to get
; between the two planets.
(define (planet-distance pw p1 p2)
  (let ([src (list-ref (planetwars-planets pw) p1)]
        [dest (list-ref (planetwars-planets pw) p2)])
    (ceiling (dist (planet-posn src) (planet-posn dest)))))


; issue-order : (planet or PlanetID) (planet or PlanetID) number -> void
(define (issue-order src dest num)
  (printf "~a ~a ~a~n" 
          (if (planet? src) (planet-id src) src)
          (if (planet? dest) (planet-id dest) dest)
          (inexact->exact (floor num)))
  (flush-output))

(define (finish-turn)
  (printf "go~n")
  (flush-output))

; is-alive? : planetwars PlayerID -> boolean
(define (is-alive? pw pid)
  (not (and (zero? (length (select-planets pw pid)))
            (zero? (length (select-fleets pw pid))))))

; winner : planetwars -> number
; If the game is not yet over (ie: at least two players have planets or
; fleets remaining), returns -1. If the game is over (ie: only one player
; is left) then that player's number is returned. If there are no
; remaining players, then the game is a draw and 0 is returned.
(define (winner pw)
  (let ([x (my-ships pw)]
        [y (enemy-ships pw)])
    (if (zero? x) 
        (if (zero? y) 0 2)
        (if (zero? y) 1 -1))))


; parse-game-state : string -> (planetwars or #f)
(define (parse-game-state s)
  (let/cc OUT
    (let* ([lines (regexp-split #rx"\n" s)]
           [plines (filter (λ(s) (and (not (zero? (string-length s)))
                                      (char=? #\P (string-ref s 0)))) lines)]
           [flines (filter (λ(s) (and (not (zero? (string-length s)))
                                      (char=? #\F (string-ref s 0)))) lines)]
           [pline->planet 
            (λ(s id)
              (let ([tokens (regexp-split #rx" +" s)])
                (if (< (length tokens) 6) (OUT #f)
                    (make-planet id    ; implicitly generated
                                 (string->number (fourth tokens)) ; owner
                                 (string->number (fifth tokens))  ; ships
                                 (string->number (sixth tokens))  ; growth
                                 (make-posn (string->number (second tokens))
                                            (string->number (third tokens))))
                    )))]
           [fline->fleet
            (λ(s)
              (let ([tokens (regexp-split #rx" +" s)])
                (if (< (length tokens) 7) (OUT #f)
                    (apply make-fleet
                           (map string->number (take (rest tokens) 6)))))
              )]
           )
      (make-planetwars
       (map pline->planet plines (build-list (length plines) values))
       (map fline->fleet flines)
       ))))


(provide (all-defined-out))



