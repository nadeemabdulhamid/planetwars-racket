#lang racket

(require "planetwars.rkt")

#|
java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt "./racket mybot.rkt" "java -jar example_bots/DualBot.jar" | java -jar tools/ShowGame.jar

|#


; do-turn : planetwars -> void
(define (do-turn pw)
  ;(display pw) (newline)
  (unless (< 2 (length (my-fleets pw)))
    
    (let (; find my strongest planet
          [my-strongest   ; planet
           (foldl
            (λ(p best-so-far)
              (if (or (not best-so-far)
                      (> (planet-ship-count p) (planet-ship-count best-so-far)))
                  p best-so-far))
            #f (my-planets pw))]
          
          ; find weakest enemy or neutral planet
          [weakest-other
           (foldl
            (λ(p best-so-far)
              (if (or (not best-so-far)
                      (< (planet-ship-count p) (planet-ship-count best-so-far)))
                  p best-so-far))
            #f (others-planets pw))]
          )
      (when (and my-strongest weakest-other)
        ; send half the ships from my strongest planet to the weakest planet
        ; that I do not own
        (issue-order my-strongest weakest-other
                     (floor (/ (planet-ship-count my-strongest) 2))))
      )))
    


(define (main)
  (let LOOP ([map-data ""])
    (let ([current-line (read-line)])
      (unless (eof-object? current-line)
        (if (string=? current-line "go")
            (let ([pw (parse-game-state map-data)])
              ;(display map-data)(newline)
              (do-turn pw)
              (finish-turn)
              (LOOP "")
              )
            (LOOP (string-append map-data current-line "\n"))))
      )))

(main)

