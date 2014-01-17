;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

;; Opal Kale - CS61AS-ca
;; Kevin Chen - CS61AS-an

(define-class (place name)
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '())
   (property-obj (instantiate basic-object)) ) ;; added by B-part1
  (initialize
   (ask property-obj 'put 'place? #t) ) ;; added by B-part1
  ;;; BUILT-IN METHODS ;;;
  (method (type) 'place) 
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()         
	  (cdr pair))))
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    (ask new-thing 'change-place self) ;; added by B-part1
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (map (lambda (a_person) (ask a_person 'notice new-person)) people)  ;; added by A-part1
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)
  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared)
  ;;; NEW METHODS ;;;
  (method (may-enter? person) ;; A-part1
    #t)
  (default-method ;; B-part1
    (apply ask property-obj message args))
  )

(define-class (person name place)
  (instance-vars
   (possessions '())
   (saying "")
   (property-obj (instantiate basic-object)) ) ;; added by B-part1
  (initialize
   (ask place 'enter self)
   (ask self 'put 'money 100) ;; added by A-part2
   (ask property-obj 'put 'strength 100) ;; added by B-part1
   (ask property-obj 'put 'person? #t) ) ;; added by B-part1
  ;;; BUILT-IN METHODS ;;;
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing) ;; modified by B-part1 and B-part2
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  (else (let ((take-success (lambda (new-thing)
				      (set! possessions (cons new-thing possessions))
				      (ask new-thing 'change-possessor self)
				      (announce-take name new-thing)
				      'taken))
		      (possessor (ask thing 'possessor)))
		  (if (equal? possessor 'no-one)
		      (take-success thing)
		      (let ((reply (ask thing 'may-take? self)))
			(if reply
			    (begin
			      (ask possessor 'lose thing)
			      (have-fit possessor)
			      (take-success reply))
			    (begin
			      (display (word "Cannot take " (ask thing 'name) " from " (ask possessor 'name) '!))
			      (newline)))) )))) )
  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction) ;; modified by A-part1
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
             (error "Can't go" direction))
            (else
               (if (equal? (ask new-place 'may-enter? self) #f) 
                  #f
                 (begin
                   (ask place 'exit self)
                   (announce-move name place new-place)
		   (for-each
                    (lambda (p)
                      (ask place 'gone p)
                      (ask new-place 'appear p))
                    possessions)
                   (set! place new-place)
                   (ask new-place 'enter self)
                   #t
                 ))))))
  ;;; NEW METHODS ;;;
  (method (take-all) ;; B-part1
    (let ((thing-list (filter
		       (lambda (thing)
			 (not (equal? (ask thing 'possessor) self)))
		       (ask place 'things))))
      (map (lambda (x) (ask self 'take x)) thing-list) ))
  (method (get-money number) ;; A-part2
    (ask self 'put 'money (+ (ask self 'money) number)))
  (method (pay-money number)
    (if (>= (ask self 'money) number) ;; A-part2
      (begin 
        (ask self 'put 'money (- (ask self 'money) number))
        #t)
        #f))
  (method (buy name-of-food) ;; A-part2
    (let ((sold-food (ask (ask self 'place) 'sell self name-of-food)))
      (if (equal? sold-food #f)
        (print "Can't buy this food item")
        (ask self 'take sold-food))))
  (method (go-directly-to new-place) ;; A-part2
     (ask place 'exit self)
     (announce-move name place new-place)
		 (for-each
         (lambda (p)
           (ask place 'gone p)
           (ask new-place 'appear p)) possessions)
          (set! place new-place)
          (ask new-place 'enter self)
          #t)
  (method (eat) ;; B-part2
    (let ((foods (filter edible? possessions)))
      (map (lambda (food)
	     (let ((cur-str (ask self 'strength))
		   (cal (ask food 'calories)))
	       (ask self 'put 'strength (+ cur-str cal)))
	     (set! possessions (delete food possessions))
	     (ask place 'gone food))
	   foods)))
  (default-method ;; B-part1
    (apply ask property-obj message args))
  )

;; New thing class from 2E

(define-class (thing name)
  (instance-vars
   (possessor 'no-one)
   (place 'no-where) ;; added by B-part1
   (property-obj (instantiate basic-object)) ) ;; added by B-part1
  (initialize
   (ask property-obj 'put 'thing? #t) ) ;; added by B-part1
  (method (change-possessor new-possessor)
    (set! possessor new-possessor) )
  (method (change-place new-place) ;; added by B-part1
    (set! place new-place) )
  (method (may-take? reciever) ;; added by B-part2
    (if (equal? possessor 'no-one)
	(error "Thing has no possessor")
	(let ((pos-str (ask possessor 'strength))
	      (rec-str (ask reciever 'strength)))
	  (if (> rec-str pos-str)
	      self
	      #f))))
  (default-method ;; added by B-part1
    (apply ask property-obj message args))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if (not record)
        #f
        (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if (not record)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))
        (set-cdr! record value)))
  'ok)

(define (make-table)
  (list '*table*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Procedures from A Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (locked-place name)
  (parent (place name))
  (instance-vars (locked #t))   
  (method (may-enter? person)
      (if (equal? locked #t)
      #f
      #t))
  (method (unlock)
    (set! locked #f)))
(define-class (garage name)
  (parent (place name))
  (instance-vars (garage_table (make-table)))

  (method (park thing)  
    (if (memq thing (ask self 'things))
      (let ((t (instantiate ticket)))
        (ask self 'appear t)
        (ask (ask thing 'possessor) 'take t)
        (insert! (ask t 'serial_num) thing garage_table)
        (ask (ask thing 'possessor) 'lose thing))
        (error "The car isn't actually in the garage!")))
  
  (method (unpark ticket_1)
    (if (equal? 'ticket (ask ticket_1 'name))
        (begin 
          (define sn (ask ticket_1 'serial_num))
          (if (lookup sn garage_table)
            (begin
              (ask (ask ticket_1 'possessor) 'take (lookup sn garage_table))
              (insert! sn #f garage_table)
              (ask (ask ticket_1 'possessor) 'lose ticket_1))
            (error "Can't unpark a car that was never parked!")))
    (error "I only take tickets!"))))


(define (ticket? item)
  (if (equal? 'ticket (ask item 'name))
    item
    #f))
        
(define-class (ticket)
  (parent (thing 'ticket))
  (class-vars (num_tickets 0))
  (instance-vars (serial_num num_tickets))
  (initialize (set! num_tickets (+ 1 num_tickets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Procedures from B PART 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-class (basic-object)
  (instance-vars
   (properties (make-table)) )
  (method (put key value)
    (insert! key value properties) )
  (default-method
    (lookup message properties) )
  )

(define-class (hotspot name password)
  (parent (place name))
  (instance-vars
   (connected-laptops '()) )
  (method (appear thing)
    (usual 'appear thing)
    (if (ask thing 'laptop?)
	(begin
	  (display "Enter hotpost password: ")
	  (flush)
	  (let ((pass-attempt (read)))
	    (ask self 'connect thing pass-attempt) ))))
  (method (gone thing)
    (set! connected-laptops (delete thing connected-laptops))
    (usual 'gone thing) )
  (method (connect laptop pass-attempt)
    (cond ((not (ask laptop 'laptop?)) (error "Not a laptop"))
	  ((equal? pass-attempt password)
	   (set! connected-laptops (cons laptop connected-laptops)) )
	  (else (display "Wrong password!")) ))
  (method (surf laptop url)
    (if (memq laptop connected-laptops)
	(system (string-append "lynx " url))
	(error "Laptop not connected to hotspot") ))
  )

(define-class (laptop)
  (parent (thing 'laptop))
  (initialize
   (ask self 'put 'laptop? #t) )
  (method (connect password)
    (let ((place (ask self 'place)))
      (ask place 'connect self password) ))
  (method (surf url)
    (let ((place (ask self 'place)))
      (ask place 'surf self url) ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Procedures from A Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (restaurant name food-class price)
  (parent (place name))
  (method (menu)
    (list (ask (instantiate food-class) 'name) price)) 
  (method (sell person name-of-food)
    (if (equal? (ask (instantiate food-class) 'name) name-of-food)
        (if (or (equal? (ask person 'type) 'police) (ask person 'pay-money price))
            (let ((x (instantiate food-class)))
                  (ask self 'appear x)
                   x)
            #f)
         #f))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Procedures from B PART 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (food name calories)
  (parent (thing name))
  (initialize
   (ask self 'put 'edible? #t))
  )

(define-class (bagel)
  (parent (food 'bagel 50)))
(define-class (coffee)
  (parent (food 'coffee 25)))
(define-class (potstickers)
  (parent (food 'potstickers 75)))

(define (edible? thing) ;; modified from original
  (ask thing 'edible?))

(define-class (police name initial-place)
  (parent (person name initial-place))
  (initialize
   (ask self 'put 'strength 1000))
  (method (type) 'police)
  (method (notice person)
    (if (equal? (ask person 'type) 'thief)
	(begin
	  (display "Crime does not pay!")
	  (map (lambda (thing)
		 (ask self 'take thing))
	       (ask person 'possessions))
	  (ask person 'go-directly-to jail)) ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (initialize
   (ask self 'put 'strength 200)) ;; added by B-part2
  (method (type) 'thief)
  (method (notice person) ;; modified by A-part2
    (if (eq? behavior 'run)
      (if (null? (ask (usual 'place) 'exits))
        (begin 
          (set! behavior 'steal)
          (print "Don't try to leave! You can't because you're in go-jail"))
	      (ask self 'go (pick-random (ask (usual 'place) 'exits))))   
	    (let ((food-things
	           (filter (lambda (thing)
			     (and (edible? thing)
			          (not (eq? (ask thing 'possessor) self))))
		           (ask (usual 'place) 'things))))
	      (if (not (null? food-things))
	          (begin
	           (ask self 'take (car food-things))
	           (set! behavior 'run)
	           (ask self 'notice person)) ))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))

(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj) ;; modified by B-part1
  (and (procedure? obj)
       (ask obj 'person?) ))

(define (thing? obj) ;; modified by B-part1
  (and (procedure? obj)
       (ask obj 'thing?) ))

(define (name obj) (ask obj 'name)) ;; added by 2F
(define (inventory obj) ;; added by 2F
  (if (person? obj)
      (map name (ask obj 'possessions))
      (map name (ask obj 'things)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COMMON TRANSCRIPTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1

;(define Foothills (instantiate place 'foothills))
;(define Kirin (instantiate place 'kirin))

;(can-go Soda 'east Foothills)
;(can-go Foothills 'west Soda)
;(can-go Kirin 'south Soda)
;(can-go Soda 'north Kirin)

;(define potstickers (instantiate thing 'Potstickers))
;(ask Kirin 'appear Potstickers)

;(define Kevin (instantiate person 'Kevin Foothills))

;(ask Kevin 'go 'west) ;; foothills -> soda
;(ask Kevin 'go 'north) ;; soda -> kirin
;(ask Kevin 'take potstickers) ;; potstickers taken
;(ask Kevin 'go 'south) ;; kirin -> soda
;(ask Kevin 'go 'up) ;; soda -> art-gallery
;(ask Kevin 'go 'west) ;; art-gallery -> BH-office
;(ask Kevin 'lose potstickers) ;; potstickers dropped
;(ask Brian 'take potstickers) ;; Brian takes potstickers
;(ask Kevin 'go 'east) ;; BH-office -> art-gallery
;(ask Kevin 'go 'down) ;; art-gallery -> soda
;(ask Kevin 'go 'down) ;; soda -> 61A-lab

;; 2A

;; Brian is an instance of the object Person, or more specifically, a procedure

;; 2B

;; Place objects understand the following messages:
;; directions-and-neighbors -- returns pairs containing a direction and a neighboring place
;; things -- returns list of things in place
;; people -- returns list of people in place
;; entry-procs -- returns list of procedures to be executed upon entry of person
;; exit-procs -- returns list of procedures to be executed upon exit of a person
;; (type) -- returns type of object, place
;; (neighbors) -- returns list of neighboring places
;; (exits) -- returns list of possible directions to travel
;; (look-in <direction>) -- returns place in the direction specified
;; (appear <new-thing>) -- creates a thing, new-thing, in the place
;; (enter <new-person>) -- enters a person, new-person, to the place
;; (gone <thing>) -- removes a thing from the place
;; (exit <person>) -- makes a person leave the place
;; (new-neighbor <direction> <neighbor>) -- add a new neighboring place
;; (add-entry-procedure <proc>) -- add a procedure to be executed upon entry of a person
;; (add-exit-procedure <proc>) -- add a procedure to be executed upon exit of a person
;; (remove-entry-procedure <proc>) -- remove an entry-procedure
;; (remove-exit-procedure <proc>) -- remove an exit-procedure
;; (clear-all-procs) -- remove all entry- and exit-procedures

;; 2C 

;; when the can-go procedure is called, the Peoples-park instance is stored in the neighbors-and-directions list for Telegraph-ave. Once Brian has moved to Peoples-park, (ask Brian 'place) would return an object, or more specifically, a procedure. (ask (ask Brian 'place) 'name) would then return "Peoples-park", the name variable of the instance. However, calling (ask Peoples-park 'appear bagel) would return an error because the instance was never bound to a variable name, and so cannot be called.

;; 2D

;;(define computer (instantiate thing 'Durer))
;;(ask 61a-lab 'appear computer) ;; is the correct implementation
;;(ask 61a-lab 'appear 'Durer) ;; is incorrect as Durer is only the name of the computer, not the instance itself
;;(ask 61a-lab 'appear Durer) ;; the variable Durer is not defined
;;(computer 'name) ;; will return a procedure, (lambda () name). If ((computer 'name)) were called instead, "Durer" would be returned.

;; PERSON A TRANSCRIPTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transcript for A.3
;(define Opal (instantiate person 'Opal Pimentel))
;(ask Opal 'go-directly-to s-h)
;(ask Opal 'go 'west) ;error message
;(ask Opal 'go 'west) ;error message
;(ask Opal 'go 'west) ;error message
;(ask Opal 'go 'west) ;Opal moved from sproul-hall to sproul-plaza
;(define Katie (instantiate person 'Katie Pimentel))
;(ask Katie 'go-directly-to s-h)
;(ask Katie 'go 'west) ;error message
;(ask Katie 'go 'west) ;error message
;(ask Katie 'go 'west) ;error message
;(ask Katie 'go 'west) ;Katie moved from sproul-hall to sproul-plaza

;; Transcript for A.4a

;(define singer (instantiate person 'rick sproul-plaza))
;(ask singer 'set-talk "My funny valentine, sweet comic valentine")
;(define preacher (instantiate person 'preacher sproul-plaza)) ;"My funny valentine, sweet comic valentine"
;(ask preacher 'set-talk "Praise the Lord")
;(ask singer 'go 'north)
;(ask singer 'go 'south)
;(ask singer 'go 'west) ;now in Sproul Hall, "Miles and miles of students are waiting in line..."
;(ask preacher 'go 'east) ;now in Sproul Hall, "My funny valentine, sweet comic valentine" "Miles and miles of students are waiting in line..."
;(define Opal (instantiate person 'Opal sproul-plaza))
;(ask Opal 'go-directly-to s-h) ;now in Sproul Hall, "Praise the Lord" "My funny valentine, sweet comic valentine" "Miles and miles of students are waiting in line..."
;(define street-person (instantiate person 'harry telegraph-ave))
;(ask street-person 'set-talk "Brother, can you spare a buck")
;(ask street-person 'go-directly-to s-h) "Praise the Lord" "My funny valentine, sweet comic valentine" "Miles and miles of students are waiting in line..."

;; Transcript for A.4b 

;(define Opals_Room (instantiate locked-place 'Opals_Room))
;(define Ken (instantiate person 'Ken Pimentel))
;(can-go Pimentel 'up Opals_Room)
;(ask Ken 'go 'up) ;return false
;(ask Opals_Room 'unlock) 
;(ask Ken 'go 'up) ;returns true, and "ken moved from pimentel to opals_room"
;(define Opal (instantiate person 'Opal Soda)) ;creating an unlocked place
;(define Bathroom (instantiate place 'Bathroom))
;(can-go Soda 'west Bathroom)
;(ask Opal 'go 'west) ;returns true

;; Transcript for A.5
;(define my_garage (instantiate garage 'my_garage))
;(define Opal (instantiate person 'Opal Pimentel))
;(define my_car (instantiate thing 'my_car))
;(ask Pimentel 'appear my_car)
;(ask Opal 'take my_car)
;(can-go Pimentel 'up my_garage)
;(ask Opal 'go 'up)
;(ask my_garage 'park my_car)
;(ask my_garage 'unpark (car (filter ticket? (ask Opal 'possessions))))

;; Transcript for A.6a
;(define Opal (instantiate person 'Opal Pimentel))
;(ask Opal 'go-directly-to Soda)
;(define Andrew (instantiate thief 'Andrew Pimentel))
;(ask Andrew 'go-directly-to jail)

;; Transcript for A.6b
;(define Opal (instantiate person 'Opal Pimentel))
;(define Andrew (instantiate person 'Andrew Pimentel))
;(define Kelsey (instantiate thief 'Kelsey Pimentel))
;(ask Kelsey 'put 'strength 200)
;(ask Opal 'go-directly-to jail)
;(ask Kelsey 'go-directly-to jail)
;(ask Andrew 'go-directly-to jail)
;(define Mary (instantiate person 'Mary Pimentel))
;(ask Mary 'put 'strength 50)
;(define pizza (instantiate thing 'pizza))
;(ask Pimentel 'appear pizza)
;(ask Mary 'take pizza)
;(ask Mary 'go-directly-to jail)
;(define Opi (instantiate person 'Opi Pimentel))
;(ask Opi 'go-directly-to jail)

;; Transcript for A.7a
;(define Opal (instantiate person 'Opal Pimentel))
;(ask Opal 'get-money 50) ;should be 150 in account
;(ask Opal 'pay-money 100) ;should be 50 in account and return #t
;(ask Opal 'pay-money 51) ;should return #f

;; Transcript for A.7b
;(define-class (bagel) (parent (food 'bagel)))
;(define Noahs (instantiate restaurant 'Noahs bagel 0.50))
;(ask Noahs 'menu)
;(define Opal (instantiate person 'Opal Noahs))
;(ask Noahs 'sell Opal 'bagel) ;returns #[closure arglist=(message) 47763c] (a food object)
;(define Amanda (instantiate person 'Amanda Noahs))
;(ask Noahs 'sell Amanda 'coffee) ;returns #f, can't buy coffee at a place that sells bagels
;(define-class (milk) (parent (food 'milk)))
;(define Peets (instantiate restaurant 'Peets milk 150)) ;expensive milk!
;(define Gary (instantiate person 'Gary Peets))
;(ask Peets 'sell Gary milk)

;; Transcript for A.8
;(define-class (bagel) (parent (food 'bagel)))
;(define Noahs (instantiate restaurant 'Noahs bagel 0.50))
;(ask Noahs 'menu)
;(define Opal (instantiate person 'Opal Noahs))
;(ask Noahs 'sell Opal 'bagel) ;returns #[closure arglist=(message) 47763c] (a food object)
;(ask Opal 'buy 'bagel) ;"opal took bagel" and added to possessions
;(define-class (milk) (parent (food 'milk)))
;(ask Opal 'buy 'milk) ;"Can't buy this food item"

;; PERSON B TRANSCRIPTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transcript for B.3+4

;# The following defines small-world:
;(define room (instantiate place 'room))
;(define thing-of-value (instantiate thing 'thing-of-value))
;(ask room 'appear thing-of-value)
;(define man (instantiate person 'man room))
;(define woman (instantiate person 'woman room))

;# Execute
;(ask man 'take-all) ;; man now has thing-of-value
;(ask woman 'take thing-of-value) ;; error, as man and woman both have same strength of 100
;(ask woman 'put 'strength 150)
;(ask woman 'take thing-of-value) ;; taken!
;(ask man 'take thing-of-value) ;; error, as man has less strength than woman

;(ask thing-of-value thing?) ;; these two are
;(thing? thing-of-value)     ;; equivalent statements

;; Transcript for B.5

;# Restart small-world, but with the following additions:
;(define lab (instantiate hotspot 'lab "password"))
;(can-go room 'up lab)
;(can-go lab 'down room)
;(define my-laptop (instantiate laptop))
;(ask room 'appear my-laptop)

;# Execute
;(ask man 'take my-laptop)
;(ask man 'go 'up)
;# now prompted to enter a password. Assume incorrect password is entered: "idunno"
;(ask my-laptop 'surf "http://www.google.ca") ;; error, as my-laptop is not connected
;(ask my-laptop 'connect "password")
;(ask my-laptop 'surf "http://www.google.ca")
;(ask man 'go 'down)
;# to verify that the laptop is disconnected:
;(ask my-laptop 'connect "password")

;; Transcript for B.6

;# The following defines a new small-world:
;(define room (instantiate place 'room))
;(define my-bagel (instantiate bagel))
;(ask room 'appear my-bagel)
;(define my-coffee (instantiate coffee))
;(ask room 'appear my-coffee)
;(define my-potstickers (instantiate potstickers))
;(ask room 'appear my-potstickers)
;(define man (instantiate person 'man room))

;# Execute:
;(ask man 'strength) ;; initially 100
;(ask man 'look-around)
;(ask man 'take-all)
;(inventory man)
;(ask man 'eat)
;(inventory man) ;; now empty
;(ask man 'look-around) ;; also food is gone from place
;(ask man 'strength) ;; now 250 (100 + 50 + 25 + 75)

;; Transcript for B.7

;# Restart small-world, with the following additions:
;(define basement (instantiate place 'basement))
;(can-go room 'down basement)
;(can-go basement 'up room)
;(define bad-man (instantiate thief 'bad-man basement))

;# Execute:
;(ask man 'take-all)
;(ask man 'go 'down)
;(inventory man)
;(inventory bad-man) ;; theft works as expected
;# Restart Small-world
;(ask man 'take my-bagel)
;(ask man 'take my-potstickers)
;(ask man 'eat) ;; strength is now 225, greater than the thief's (200)
;(ask man 'take my-coffee)
;(ask man 'go 'down) ;; thief cannot steal
;# Restart Small-world
;(ask man 'take-all)
;(ask man 'go 'down) ;; thief in room now
;(define pig (instantiate police 'bob basement))
;(ask man 'go 'up) ;; cause thief to go back to basement, where police is

;; Transcript for B.8 ;;

;# Restart Small-world, with the following addition:
;(define woman (instantiate person 'woman room))

;# Execute:
;(ask man 'take my-bagel)
;(ask woman 'take my-bagel) ;; cannot take as woman does not have enough strength
;(ask woman 'put 'strength 150)
;(ask woman 'take my-bagel)
