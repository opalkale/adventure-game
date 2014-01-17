;;;  Data for adventure game.  This file is adv-world.scm

;; Opal Kale - CS61AS-ca
;; Kevin Chen - CS61AS-an

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define Soda (instantiate place 'Soda))
(define BH-Office (instantiate place 'BH-Office))
(define MJC-Office (instantiate place 'MJC-Office))
(define art-gallery (instantiate place 'art-gallery))
(define Pimentel (instantiate place 'Pimentel))
(define 61A-Lab (instantiate place '61A-Lab))
(define Sproul-Plaza (instantiate place 'Sproul-Plaza))
(define Telegraph-Ave (instantiate place 'Telegraph-Ave))
(define Noahs (instantiate place 'Noahs))
(define Intermezzo (instantiate place 'Intermezzo))
(define Haas (instantiate place 'Haas-Business-School))
(define s-h (instantiate place 'sproul-hall))


(can-go Soda 'up art-gallery)
(can-go art-gallery 'down Soda)
(can-go art-gallery 'west BH-Office)
(can-go BH-Office 'east art-gallery)
(can-go art-gallery 'east MJC-Office)
(can-go MJC-office 'west art-gallery)
(can-go Soda 'down 61A-Lab)
(can-go 61A-Lab 'up Soda)
(can-go Soda 'south Pimentel)
(can-go Pimentel 'north Soda)
(can-go Pimentel 'south Haas)
(can-go Haas 'north Pimentel)
(can-go Haas 'west s-h)
(can-go s-h 'east Haas)
(can-go Sproul-Plaza 'east s-h)
(can-go s-h 'west Sproul-Plaza)
(can-go Sproul-Plaza 'north Pimentel)
(can-go Sproul-Plaza 'south Telegraph-Ave)
(can-go Telegraph-Ave 'north Sproul-Plaza)
(can-go Telegraph-Ave 'south Noahs)
(can-go Noahs 'north Telegraph-Ave)
(can-go Noahs 'south Intermezzo)
(can-go Intermezzo 'north Noahs)

;; Some people.
; MOVED above the add-entry-procedure stuff, to avoid the "The computers
; seem to be down" message that would occur when hacker enters 61a-lab
; -- Ryan Stejskal

(define Brian (instantiate person 'Brian BH-Office))
(define hacker (instantiate person 'hacker 61A-lab))
(define nasty (instantiate thief 'nasty sproul-plaza))

(define (bh-office-exit)
  (print "What's your favorite programming language?")
  (let ((answer (read)))
    (if (eq? answer 'scheme)
	(print "Good answer, but my favorite is Logo!")
	(begin (newline) (bh-office-exit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additions from A Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sproul-hall-exit ;not sure
  (let ((counter 1))
    (lambda ()
      (if (<= counter 3)
        (begin (set! counter (+ 1 counter)) (error "You can check out any time you'd like, but you can never leave"))
        (set! counter 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ask s-h 'add-entry-procedure
 (lambda () (print "Miles and miles of students are waiting in line...")))
(ask s-h 'add-exit-procedure sproul-hall-exit)
(ask BH-Office 'add-exit-procedure bh-office-exit)
(ask Noahs 'add-entry-procedure
 (lambda () (print "Would you like lox with it?")))
(ask Noahs 'add-exit-procedure
 (lambda () (print "How about a cinnamon raisin bagel for dessert?")))
(ask Telegraph-Ave 'add-entry-procedure
 (lambda () (print "There are tie-dyed shirts as far as you can see...")))
(ask 61A-Lab 'add-entry-procedure
 (lambda () (print "The computers seem to be down")))
(ask 61A-Lab 'add-exit-procedure
 (lambda () (print "The workstations come back to life just in time.")))

;; Some things.

(define my-bagel (instantiate bagel)) ;; modified to match proper food instantiation in B-part2
(ask Noahs 'appear my-bagel)

(define my-coffee (instantiate coffee)) ;; modified
(ask Intermezzo 'appear my-coffee)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additions from Parts 1 and 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Foothills (instantiate place 'foothills))
(define Kirin (instantiate place 'kirin))

(can-go Soda 'east Foothills)
(can-go Foothills 'west Soda)
(can-go Kirin 'south Soda)
(can-go Soda 'north Kirin)

(define my-potstickers (instantiate potstickers)) ;; modified
(ask Kirin 'appear my-potstickers)

(define Kevin (instantiate person 'Kevin Foothills))

(define computer (instantiate thing 'Durer))
(ask 61a-lab 'appear computer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additions from A Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define jail (instantiate place 'jail))
