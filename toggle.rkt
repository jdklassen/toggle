#lang racket

(require pltcod/libtcod)

; size of characters, square
(define char-size 64.0)

; location of light
; #:transparent for better comparison
(struct light (x y) #:transparent)

; size of board
(define x-dim 5)
(define y-dim 5)

; board, light in set <-> light on
(define board (mutable-set))

; make a bunch of moves from 'won state' to guarantee solvability
(define (randomize-board iters)
  (cond
    [(< 0 iters) (click (random 5) (random 5))
	   (randomize-board (- iters 1))]))

; 224 = `char-checkbox-unset - not sure why that fails
(define (draw-map console)
  (tcod:console-clear console)
  (set-for-each board (lambda (light)
		     (tcod:console-put-char-ex console
		       (light-x light) (light-y light) 224 tcod:orange tcod:yellow)))
  (tcod:console-flush))

(define (on-map light)
  (let ([x (light-x light)]
	[y (light-y light)])
    (and (>= x 0) (< x x-dim) (>= y 0) (< y y-dim))))

(define (light-neighbours x y)
  (filter on-map (list
		 (light x y)
		 (light (- x 1) y)
		 (light (+ x 1) y)
		 (light x (- y 1))
		 (light x (+ y 1)))))

(define (toggle light)
  (cond
    [(set-member? board light) (set-remove! board light)]
    [else (set-add! board light)]))

(define (click x y)
  (map toggle (light-neighbours x y)))

; use flag to only catch the click on depress
(define mouse-state #f)
(define (mouse)
  (let* ([m (tcod:mouse-get-status)]
	 ; divide out to find position on board
	 [x (exact-truncate (/ (tcod:mouse-x m) char-size))]
	 [y (exact-truncate (/ (tcod:mouse-y m) char-size))])
    (cond
      [(and (tcod:mouse-lbutton-pressed? m) (not mouse-state)) (click x y)])
    (set! mouse-state (tcod:mouse-lbutton-pressed? m))))

(define (quit?)
  (tcod:console-is-key-pressed? `key-escape))

(define (won)
  (set-empty? board))

(define (draw-game-over console)
  (tcod:console-clear console)
  (tcod:console-print console 1 2 "You")
  (tcod:console-print console 1 3 "Won!")
  (tcod:console-flush))

; check for clearing game over screen
(define (touched?)
  (let* ([m (tcod:mouse-get-status)]
	 [res (or (quit?)
		  (and (tcod:mouse-lbutton-pressed? m) (not mouse-state)))])
    (set! mouse-state (tcod:mouse-lbutton-pressed? m))
    res))

; game loop after won
(define (game-over console)
  (tcod:sys-check-for-event `any)
  (draw-game-over console)
  (cond
    [(touched?) #f]
    [else (game-over console)]))

; main game loop
(define (game-loop console)
  (tcod:sys-check-for-event `any)
  (mouse)
  (draw-map console)
  (cond
    [(tcod:console-is-window-closed?) #f]; never seems to fire?
    [(quit?) #f]
    ; after winning, transition to game-over loop
    [(won) (game-over console)]
    [else (game-loop console)]))

; manage deletion of console
(define (manage-console x y title f)
  ;(tcod:console-set-custom-font "terminal.png" `font-layout-ascii-in-row 8 8)
  (tcod:console-init-root x y title #f `sdl)
  (tcod:sys-set-fps 30)
  (f #f) ; root console is null pointer is #f
  (tcod:console-delete #f))

(randomize-board (* 2 16 16))
(manage-console x-dim y-dim "Toggle" game-loop)

