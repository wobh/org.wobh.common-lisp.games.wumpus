;;;; -*- Mode:lisp;coding:utf-8 -*-
;;;; FILE: wumpus.lisp

;;;; DESCRIPTION

;;;; A Common Lisp version of Gregory Yob's classic game

;;;; AUTHORS

;;;; William H. Clifford <wobh@wobh.org>

;;;; NOTES

;;;; see README.org

(defpackage #:org.wobh.wumpus
  (:nicknames #:wumpus #:hunt)
  (:use #:common-lisp)
  (:export #:*cave-networks* #:get-cave-network)
  (:export #:dodecahedron #:dodecahedron-circuit #:mobius-strip #:string-of-beads
	   #:hex-net-on-torus #:dendrite-with-degeneracies #:one-way-lattice)
  ;; Cave names
  (:export #:make-hunt
	   #:setup-hunt #:reset-hunt)
  (:export #:setup-game)
  (:export #:main)
  (:documentation "HUNT THE WUMPUS

* Symbols exported

Summary of symbols exported. See README.org for more information.

** Game setup functions

- main :: Launches game, defaults to classic Hunt the Wumpus settings.
- setup-game :: Creates Hunt the Wumpus game environment
- setup-hunt :: Creates Hunting environment, cave, wumpus, and hunter qualities
- reset-hunt :: Reset hunt to original settings

** Cave networks

- *cave-networks* :: named cave networks
- get-cave-network :: get cave networks by name

*** Names of cave networks

- dodecahedron
- dodecahedron-circuit
- mobius-strip
- string-of-beads
- hex-net-on-torus
- dendrite-with-degeneracies
- one-way-lattice"))


(in-package #:org.wobh.wumpus)


;;;; Randomness

(defparameter *hunt-random-state* (make-random-state T))

(defun hunt-random (limit &optional (random-state *hunt-random-state*))
  (random limit random-state))

(defun hunt-random-elt (seq &optional (random-state *hunt-random-state*))
  "Get a random element from sequence."
  (elt seq (random (length seq) random-state)))

(defun hunt-shuffle (seq &optional (random-state *hunt-random-state*))
  "Knuth shuffle"
  (let ((len (length seq)))
    (dotimes (i len seq)
      (rotatef (elt seq i)
	       (elt seq (+ i (hunt-random (- len i) random-state)))))))

;; Knuth shuffle
;; https://groups.google.com/d/topic/comp.lang.lisp/1ZtO84hrAuM/discussion


;;;;; The Hunt

(defparameter *hunt-events*
  '((hunter-enters-cave    (hunter-senses-chamber))

    (hunter-senses-chamber (get-hunter-action
                            hunter-hears-bats
			    hunter-feels-draft
			    hunter-smells-wumpus))

    (get-hunter-action     (hunter-moves
			    hunter-shoots
			    player-quits-game))
    
    (hunter-smells-wumpus  (get-hunter-action))
    (hunter-feels-draft    (get-hunter-action))
    (hunter-hears-bats     (get-hunter-action))
    
    (player-quits-game     (hunt-ends))

    (hunter-moves          (hunter-enters-chamber))
    (hunter-enters-chamber (hunter-senses-chamber
                            bat-snatches-hunter
			    hunter-falls-in-pit
			    wumpus-bothered))
    (wumpus-bothered       (wumpus-rests
			    wumpus-wanders
			    wumpus-mauls-hunter))
    (wumpus-mauls-hunter   (hunter-slain
			    wumpus-rests
			    wumpus-wanders))
    (wumpus-wanders        (wumpus-moves))
    (wumpus-moves          (wumpus-rests
			    wumpus-mauls-hunter))
    (wumpus-rests          (get-hunter-action))
    (hunter-slain          (hunt-ends))
    (hunter-falls-in-pit   (hunt-ends))
    (bat-snatches-hunter   (hunter-enters-chamber))
    
    (hunter-shoots         (arrow-strikes-ground
                            arrow-strikes-wumpus
			    arrow-strikes-hunter
                            arrow-scatters-bats
			    arrow-falls-in-pit))
    (arrow-falls-in-pit    (get-hunter-action))
    (arrow-scatters-bats   (get-hunter-action
			    bat-snatches-hunter))
    (bat-snatches-hunter   (hunter-enters-chamber))
    (arrow-strikes-hunter  (hunter-slain
			    get-hunter-action))
    (arrow-strikes-wumpus  (wumpus-slain
			    wumpus-enraged))
    (wumpus-enraged        (wumpus-seeks-shooter
			    wumpus-wanders))
    (wumpus-seeks-shooter  (wumpus-wanders
			    wumpus-rests
			    wumpus-mauls-hunter))
    (arrow-strikes-ground  (wumpus-bothered))
    (wumpus-slain          (hunt-ends))
    
    (hunt-ends             Nil)
    
    )
  "Possible hunting events and consequences")

(defparameter *hunt-endings*
  '(player-quits-hunt
    hunter-fell-in-pit
    hunter-slain
    wumpus-slain)
  "Possible ways for game to end")

(defparameter *wumpus-health-setup* 1
  "The number of arrow-strikes required to slay the Wumpus")

(defparameter *wumpus-hurt-setup* 'wumpus-bothered
  "What the Wumpus does when injured")

(defparameter *hunter-health-setup* 1
  "The number of Wumpus-strikes required to slay the Hunter")

(defparameter *quiver-hold-setup* 5
  "The number of arrows the hunter starts off with")

(defparameter *quiver-room-setup* 5
  "how many arrows can the quiver hold")

(defparameter *bow-range-maximum* 5
  "The maximum number of chambers a bow can shoot an arrow")

(defparameter *arrow-path-eval-setup* 'eval-arrow-path-classic
  "What happens when an arrow is shot through the caves")

(defparameter *cave-name-default* 'dodecahedron
  "The default cave system.")

(defparameter *bats-total* 2
  "The number of bat chambers in cave")

(defparameter *pits-total* 2
  "The number of pit chambers in cave")

(defstruct (hunt (:conc-name Nil))
  "The hunting environment: cave, wumpus, hunter, etc."
  (cave-passages      ())
  (cave-bats          ())
  (cave-pits          ())
  (wumpus-hurt        *wumpus-hurt-setup*)
  (wumpus-path        ())
  (wumpus-life        (list *wumpus-health-setup*))
  (hunter-path        ())
  (hunter-life        (list *hunter-health-setup*))
  (quiver-room        *quiver-room-setup*)
  (quiver-hold        (list *quiver-hold-setup*))
  (bow-range-maximum  *bow-range-maximum*)
  (arrow-paths        ())
  (arrow-path-eval    *arrow-path-eval-setup*)
  (hunt-endings       *hunt-endings*)
  (hunt-events        (mapcar #'first *hunt-events*))
  (hunt-history       ())
  )


;;;; The Cave

;;; Cave passages

(defparameter *cave-networks*
  '((dodecahedron
     (( 1  4  7) ( 0  2  9) ( 1  3 11) ( 2  4 13) ( 0  3  5)
      ( 4  6 14) ( 5  7 16) ( 0  6  8) ( 7  9 17) ( 1  8 10)
      ( 9 11 18) ( 2 10 12) (11 13 19) ( 3 12 14) ( 5 13 15)
      (14 16 19) ( 6 15 17) ( 8 16 18) (10 17 19) (12 15 18)))
    (dodecahedron-circuit
     (( 1 19  7) ( 2  0  5) ( 3  1 18) ( 4  2 16) ( 5  3 14)
      ( 6  4  1) ( 7  5 13) ( 8  6  0) ( 9  7 12) (10  8 19)
      (11  9 17) (12 10 15) (13 11  8) (14 12  6) (15 13  4)
      (16 14 11) (17 15  3) (18 16 10) (19 17  2) ( 0 18  9)))
    (mobius-strip
     ((19  1  2) (18  0  3) ( 0  3  4) ( 1  2  5) ( 2  5  6)
      ( 3  4  7) ( 4  7  8) ( 5  6  9) ( 6  9 10) ( 7  8 11)
      ( 8 11 12) ( 9 10 13) (10 13 14) (11 12 15) (12 15 16)
      (13 14 17) (14 17 18) (15 16 19) ( 1 16 19) ( 0 17 18)))
    (string-of-beads
     (( 1  2 19) ( 0  2  3) ( 0  1  3) ( 1  2  4) ( 3  5  6)
      ( 4  6  7) ( 4  5  7) ( 5  6  8) ( 7  9 10) ( 8 10 11)
      ( 8  9 11) ( 9 10 12) (11 13 14) (12 14 15) (12 13 15)
      (13 14 16) (15 17 18) (16 18 19) (16 17 19) ( 0 17 18)))
    (hex-net-on-torus
     (( 5  9 15) ( 5  6 16) ( 6  7 17) ( 7  8 18) ( 8  9 19)
      ( 0  1 14) ( 1  2 10) ( 2  3 11) ( 3  4 12) ( 4  0 13)
      ( 6 15 19) ( 7 15 16) ( 8 16 17) ( 9 17 18) ( 5 18 19)
      ( 0 10 11) ( 1 11 12) ( 2 12 13) ( 3 13 14) ( 4 10 14)))
    (dendrite-with-degeneracies
     (( 0  0  4) ( 1  1  4) ( 2  2  5) ( 3  3  5) ( 0  1  6)
      ( 2  3  6) ( 4  5  9) ( 7  8  8) ( 7  7  9) ( 6  8 10)
      ( 9 12 13) (11 12 12) (10 17 11) (10 14 15) (13 16 17)
      (13 18 19) (14 16 15) (14 17 17) (13 18 18) (15 19 19)))
    (one-way-lattice
     (( 4  3  7) ( 0  4  5) ( 1  5  6) ( 2  6  7) ( 7  8 11)
      ( 4  8  9) ( 5  9 10) ( 6 10 11) (11 12 15) ( 8 12 13)
      ( 9 13 14) (10 14 15) (15 16 19) (12 16 17) (13 17 18)
      (14 18 19) ( 0  3 19) ( 0  1 16) ( 1  2 17) ( 2  3 18))))
  "A cave is a network of chambers connected by passages.")

(defun get-cave-network (cave-name)
  "Find passage network associated with cave-name"
  (assert (find cave-name *cave-networks* :key 'first))
  (second (find cave-name *cave-networks* :key 'first)))

;;; FIXME: I think I have to either use keywords for cave names, or
;;; export all these cave names to make them useful externally.

;;; TODO: make-cave-network, make-random-cave-network


;;; Chambers

(defun count-chambers-in-cave (cave)
  "How many chambers are in cave"
  (length cave))

(defun list-chambers-in-cave (cave)
  "A list of chambers in cave"
  (loop
     for chamber from 0 below (count-chambers-in-cave cave)
     collect chamber))

(defun random-chamber (cave)
  "Return a random chamber from cave"
  (hunt-random (count-chambers-in-cave cave)))

(defun chamber-in-cave-p (chamber cave)
  "Is this chamber in this cave?"
  (find chamber (list-chambers-in-cave cave)))

;; I don't know why, but at first I did chamber-in-cave-p this way:
;; (when (chambers-nearby chamber cave)
;;    chamber))
;; but I realized it had this interesting flaw:
;; chambers with no connecting tunnels are not in the cave
;; ((1 2 3) () () (0 1 2))

(defun chambers-nearby (chamber cave)
  "What chambers have tunnels connecting to here?"
  (assert (chamber-in-cave-p chamber cave))
  (elt cave chamber))

(defun random-chamber-nearby (chamber cave)
  "Return a random chamber near a given chamber"
  (assert (chamber-in-cave-p chamber cave))
  (hunt-random-elt (chambers-nearby chamber cave)))


;;; Passages and Paths
;;; A path is a list of locales connected by passages

(defun chamber-in-path-p (chamber path)
  "What position, if any, is this chamber in the path?"
  (position chamber path :from-end T))

;;; NOTE: All paths are expected to be in reverse order, with the
;;; latest locale in path as first item in path.

(defun truncate-path (chamber path)
  "Return a path, truncated at the first instances of a given
chamber."
  (assert (chamber-in-path-p chamber path))
  (let ((rpath (reverse path)))
    (reverse
     (subseq rpath 0
	     (1+ (position chamber rpath))))))

(defun passage-to-p (here there cave)
  "Does a tunnel connect this chamber with another?"
  (find there (chambers-nearby here cave)))

(defun latest-locale (path)
  "Where does this path lead to?"
  (first path))

(defun path-in-cave-p (path cave)
  "Is this a valid path through this cave?"
  (when (notany #'null (list*
			(chamber-in-cave-p (latest-locale path) cave)
			(loop
			   for (here there) on path
			   while there
			   collect
			     (passage-to-p here there cave))))
    path))

;; NOTE: a hunter may be carried to a new location by a bat which
;; currently has the effect of teleporting the hunter. For now, only
;; the Wumpus path and arrow flights will be considered valid cave
;; paths.

(defun correct-path-stop (path cave)
  "Create a correct path, stopping as soon as an incorrect locale is
reached."
  (let ((rpath (reverse path)))
    (loop
       for (here there) on rpath
       while (passage-to-p here there cave)
       collect there into seq
       finally (return
		 (reverse (cons (first rpath) seq))))))

(defun correct-path-steer (path cave)
  "Create a correct path, replacing incorrect locales in path with
randomly chosen correct ones."
  (let ((rpath (reverse path)))
    (loop
       for (here there) on rpath
       while there
       collect
	 (if (passage-to-p here there cave)
	     there
	     (random-chamber-nearby here cave))
       into seq
       finally (return
		 (reverse (cons (first rpath) seq))))))

;;; FIXME: Should it exclude the passage it came from? (yes)

;;; TODO: We need a correct path wander. This is what bats will do
;;; when carrying a hunter off, pick a random location and carry him
;;; there by random walk.

;;; What should happen when a bat carrying a hunter passes through a
;;; wumpus or pit chamber?

;; (defun make-random-path (&optional path-length)
;; What are the odds of a random path having any particular chamber at the end?

;; (defun wander-to-there (here there cave)
;;   "Create a valid path between two locales by wandering."
;;   (error "FIXME: this function should exist, but is not defined"))

;;; FIXME: how should this work? Does not have to be the shortest
;;; path, but just a "reasonably" short path. Maybe find up to 6 of
;;; the shortest paths to destination, pick one at random.

;; (defun correct-path-seek (path cave)
;;   "Create a correct path by seeking random route through cave between
;;   locales."
;;   (list *
;; 	(first path)
;; 	(loop
;; 	   for (here there) on path
;; 	   while there
;; 	   collect
;; 	     (if (passage-to-p here there cave)
;; 		 there
;; 		 (wander-to-there here there cave)))))




;;;; Setup hunt

(defun setup-hunt (&key
		   (cave-name           *cave-name-default*)
		   (wumpus-health       *wumpus-health-setup*)
		   (wumpus-hurt         *wumpus-hurt-setup*)
		   (hunter-health       *hunter-health-setup*)
		   (bow-range-maximum   *bow-range-maximum*)
		   (quiver-room         *quiver-room-setup*)
		   (quiver-hold         *quiver-hold-setup*)
		   (arrow-path-eval     *arrow-path-eval-setup*))
  "Create a hunt environment"
  (let* ((hunt
	  (make-hunt :cave-passages     (get-cave-network cave-name)
		     :wumpus-life       (list wumpus-health)
		     :wumpus-hurt       wumpus-hurt
		     :hunter-life       (list hunter-health)
		     :bow-range-maximum bow-range-maximum
		     :quiver-room       quiver-room
		     :quiver-hold       (list quiver-hold)
		     :arrow-path-eval   arrow-path-eval
		     ))
	 (random-chambers
	  (hunt-shuffle (list-chambers-in-cave (cave-passages hunt)))))
    (push (pop random-chambers) (wumpus-path hunt))
    (push (pop random-chambers) (hunter-path hunt))
    (loop
       repeat *bats-total*
       do
	 (push (pop random-chambers) (cave-bats hunt)))
    (loop
       repeat *pits-total*
       do
	 (push (pop random-chambers) (cave-pits hunt)))
    hunt))

(defun reset-hunt (hunt)
  "Reset a hunt environment to the start conditions"
  (setf (wumpus-life hunt) (last (wumpus-life hunt))
	(wumpus-path hunt) (last (wumpus-path hunt))
	(hunter-life hunt) (last (hunter-life hunt))
	(hunter-path hunt) (last (hunter-path hunt))
	(quiver-hold hunt) (last (quiver-hold hunt))
	(arrow-paths hunt) ()
	(hunt-events hunt) (last (hunt-events hunt)))
  hunt)


;;;; The Wumpus

(defun wumpus-health (hunt)
  "What is the current health of the wumpus?"
  (first (wumpus-life hunt)))

(defun wumpus-alive-p (hunt)
  "Is the wumpus alive?"
  (< 0 (wumpus-health hunt)))

(defun hurt-wumpus (hunt &optional (hurt 1))
  "Injure the wumpus"
  (assert (< 0 hurt))
  (with-accessors ((life wumpus-life)
		   (health wumpus-health)) hunt
    (push (- health hurt) life)))

(defun heal-wumpus (hunt &optional (heal 1))
  "Heal/grow the wumpus"
  (assert (< 0 heal))
  (with-accessors ((life wumpus-life)
		   (health wumpus-health)) hunt
    (push (+ health heal) life)))

(defun wumpus-locale (hunt)
  "Where is the wumpus?"
  (latest-locale (wumpus-path hunt)))

(defun send-wumpus (locale hunt)
  "Move the wumpus to a new locale"
  (assert (chamber-in-cave-p (wumpus-locale hunt) (cave-passages hunt)))
  (push locale (wumpus-path hunt)))


;;;; The Hunter

(defun hunter-health (hunt)
  "What is the current health of the hunter?"
  (first (hunter-life hunt)))

(defun hunter-alive-p (hunt)
  "Is the hunter alive?"
  (< 0 (hunter-health hunt)))

(defun hurt-hunter (hunt &optional (hurt 1))
  "Injure the hunter"
  (assert (< 0 hurt))
  (with-accessors ((life hunter-life)
		   (health hunter-health)) hunt
    (push (- health hurt) life)))

(defun heal-hunter (hunt &optional (heal 1))
  "Heal/grow the hunter"
  (assert (< 0 heal))
  (with-accessors ((life hunter-life)
		   (health hunter-health)) hunt
    (push (+ health heal) life)))

(defun hunter-locale (hunt)
  "Where is the hunter?"
  (latest-locale (hunter-path hunt)))

(defun send-hunter (locale hunt)
  "Send the hunter to a new locale"
  (assert (chamber-in-cave-p (hunter-locale hunt) (cave-passages hunt)))
  (push locale (hunter-path hunt)))


;;;; The Hunter's Quiver

(defun quiver-holding (quiver)
  "How many arrows does the hunter's quiver hold?"
  (first (quiver-hold quiver)))

(defun quiver-full-p (quiver)
  "Is the hunter's quiver full?"
  (= (quiver-holding quiver) (quiver-room quiver)))

(defun quiver-void-p (quiver)
  "Is the hunter's quiver empty?"
  (< (quiver-holding quiver) 1))

(defun gain-arrow (quiver &optional (arrows 1))
  "Add an arrow to the hunter's quiver"
  (assert (not (quiver-full-p quiver)))
  (with-accessors ((holding quiver-holding)
		   (room quiver-room)
		   (hold quiver-hold)) quiver
    (let ((sum (+ holding arrows)))
      (push (if (< sum room) sum room) hold))))

(defun lose-arrow (quiver &optional (arrows 1))
  "Take an arrow from the hunter's quiver"
  (assert (not (quiver-void-p quiver)))
  (with-accessors ((holding quiver-holding)
		   (room quiver-room)
		   (hold quiver-hold)) quiver
    (let ((dif (- holding arrows)))
      (push (if (< dif 0) 0 dif) hold))))


;;;; The Hunter's Bow

(defun in-range-p (distance bow)
  "Is the given distance in the range of the hunter's bow"
  (<= 1 distance (bow-range-maximum bow)))


;;;; The Hunter's Arrows

(defun arrow-locale (arrow-path)
  "Where is the arrow now?"
  (first arrow-path))

(defun arrow-source (arrow-path)
  "Where did the arrow seem to come from?"
  (second arrow-path))

(defun make-new-shot (arrow-path hunt)
  (push arrow-path (arrow-paths hunt)))

(defun latest-shot (hunt)
  "What was the path of the last arrow shot?"
  (first (arrow-paths hunt)))

(defun set-latest-shot (hunt arrow-path)
  (setf (first (arrow-paths hunt)) arrow-path))

(defsetf latest-shot set-latest-shot)

(defun track-latest-shot (hunt)
  "From what room did the arrow come from?"
  (arrow-source (latest-shot hunt)))

(defun stop-latest-shot (chamber hunt)
  "Arrow stops in midflight."
  (assert (chamber-in-path-p chamber (latest-shot hunt)))
  (setf (latest-shot hunt) (truncate-path chamber (latest-shot hunt))))


;;;; The Wumpus and Hunter in the Cave

(defun chambers-near-wumpus (hunt)
  "What chambers are near the wumpus"
  (chambers-nearby (wumpus-locale hunt) (cave-passages hunt)))

(defun chambers-near-hunter (hunt)
  "What chambers are near the hunter"
  (chambers-nearby (hunter-locale hunt) (cave-passages hunt)))

(defun wumpus-in-arrow-path-p (hunt)
  "Position of wumpus in the latest arrow path. An arrow path always has
a hunter location as its last value."
  (chamber-in-path-p (wumpus-locale hunt) (butlast (latest-shot hunt))))

(defun hunter-in-arrow-path-p (hunt)
  "Position of hunter in the latest arrow path. An arrow path always has
a hunter location as its last value."
  (chamber-in-path-p (hunter-locale hunt) (butlast (latest-shot hunt))))

(defun hunter-near-bat-p (hunt)
  "Is the hunter near a bat chamber?"
  (intersection (chambers-near-hunter hunt) (cave-bats hunt)))
  
(defun hunter-with-bat-p (hunt)
  "Is the hunter in a bat chamber?"
  (find (hunter-locale hunt) (cave-bats hunt)))

(defun hunter-near-pit-p (hunt)
  "Is the hunter near a pit chamber?"
  (intersection (chambers-near-hunter hunt) (cave-pits hunt)))

(defun hunter-in-pit-p (hunt)
  "Is the hunter in a pit chamber?"
  (find (hunter-locale hunt) (cave-pits hunt)))

(defun hunter-near-wumpus-p (hunt)
  "Is the hunter near the wumpus' chamber?"
  (find (hunter-locale hunt) (chambers-near-wumpus hunt)))

(defun hunter-with-wumpus-p (hunt)
  "Is the hunter in the wumpus' chamber?"
  (= (hunter-locale hunt) (wumpus-locale hunt)))


;;;; Hunt events

(defun record-event (event hunt)
  "Record an event to the hunt history"
  (push event (hunt-history hunt)))

(defun latest-event (hunt)
  "What is the latest event in the hunt history?"
  (first (hunt-history hunt)))

(defun events-since (last-event hunt)
  "What events have happened since a given event last occurred?"
  (loop
     for event in (hunt-history hunt)
     collect event
     until (eq event last-event)))

(defun hunt-end-p (hunt)
  "Has one of hunt-ending events occurred?"
  (find (latest-event hunt) (hunt-endings hunt)))

(defun player-quits-hunt (hunt)
  "Player quits game"
  (record-event 'player-quits-hunt hunt)
  (events-since 'player-quits-hunt hunt))

(defun wumpus-rests (hunt)
  "The wumpus rests"
  (record-event 'wumpus-rests hunt))

(defun wumpus-moves (chamber hunt)
  "The wumpus moves to a nearby chamber"
  (assert (passage-to-p (wumpus-locale hunt) chamber (cave-passages hunt)))
  (send-wumpus chamber hunt)
  (record-event 'wumpus-moves hunt))

(defun wumpus-wanders (hunt)
  "The wumpus moves randomly"
  (wumpus-moves
   (random-chamber-nearby
    (wumpus-locale hunt) (cave-passages hunt)) hunt))

(defun wumpus-bothered (hunt)
  "The wumpus is bothered"
  (let ((chance (hunt-random 4)))
    (unless (= chance 3)
      (wumpus-wanders hunt))))

(defun wumpus-enraged (hunt)
  "What does the wumpus do when enraged?"
  (assert (equal (events-since 'arrow-strikes-wumpus hunt)
		 '(wumpus-wakes arrow-strikes-wumpus)))
  (wumpus-moves (track-latest-shot hunt) hunt)
  (unless (hunter-with-wumpus-p hunt)
    (wumpus-bothered hunt)))

(defun wake-wumpus (cause hunt)
  "What does the wumpus do when woken?"
  (record-event 'wumpus-wakes hunt)
  (cond
    ((eq cause 'arrow-strikes-wumpus)
     (funcall (symbol-function (wumpus-hurt hunt)) hunt))
    (T
     (wumpus-bothered hunt)))
  (when (hunter-with-wumpus-p hunt)
    (hurt-hunter hunt)
    (record-event 'wumpus-mauls-hunter hunt)
    (unless (hunter-alive-p hunt)
      (record-event 'hunter-slain hunt)))
  (when (and (wumpus-alive-p hunt) (hunter-alive-p hunt))
    (wumpus-rests hunt)))

(defun arrow-strikes-wumpus (hunt)
  "An arrow strikes the wumpus"
  (record-event 'arrow-strikes-wumpus hunt)
  (stop-latest-shot (wumpus-locale hunt) hunt)
  (hurt-wumpus hunt)
  (cond ((wumpus-alive-p hunt)
	 (wake-wumpus 'arrow-strikes-wumpus hunt))
	(T
	 (record-event 'wumpus-slain hunt))))

(defun hunter-enters-chamber (hunt)
  "The hunter enters a chamber"
  (loop
     with hunter-moved = Nil
     do
       (record-event 'hunter-enters-chamber hunt) ; necessary?
       (cond
	 ((hunter-with-wumpus-p hunt)
	  (record-event 'hunter-bumps-wumpus hunt)
	  (wake-wumpus 'hunter-bumps-wumpus hunt)
	  (setf hunter-moved Nil))
	 ((hunter-in-pit-p hunt)
	  (record-event 'hunter-fell-in-pit hunt)	  
	  (setf hunter-moved Nil))
	 ((hunter-with-bat-p hunt)
	  (record-event 'bat-snatches-hunter hunt)
	  (send-hunter (random-chamber (cave-passages hunt)) hunt)
	  (setf hunter-moved T))
	 (T
	  (setf hunter-moved Nil)))
     until (null hunter-moved)))

(defun hunter-senses-chamber (hunt)
  "The hunter uses his/her senses to sense for danger in nearby
chambers"
  (unless (hunt-end-p hunt)
    (record-event 'hunter-senses-chamber hunt)
    (cond
      ((hunter-near-wumpus-p hunt)
       (record-event 'hunter-smells-wumpus hunt))
      ((hunter-near-pit-p hunt)
       (record-event 'hunter-feels-draft hunt))
      ((hunter-near-bat-p hunt)
       (record-event 'hunter-hears-bats hunt))
      (T Nil))
    (events-since 'hunter-senses-chamber hunt)))

(defun hunter-moves (chamber hunt)
  "The hunter moves to a nearby chamber"
  (assert (passage-to-p chamber (hunter-locale hunt) (cave-passages hunt)))
  (record-event 'hunter-moves hunt)
  (send-hunter chamber hunt)
  (hunter-enters-chamber hunt)
  (hunter-senses-chamber hunt)
  (events-since 'hunter-moves hunt))

(defun hunter-enters-cave (hunt)
  "The hunter enters the cave."
  (record-event 'hunter-enters-cave hunt)
  (hunter-enters-chamber hunt)
  (hunter-senses-chamber hunt)
  (events-since 'hunter-enters-cave hunt))

(defun arrow-strikes-hunter (hunt)
  "An arrow strikes the hunter"
  (record-event 'arrow-strikes-hunter hunt)
  (stop-latest-shot (hunter-locale hunt) hunt)
  (hurt-hunter hunt)
  (unless (hunter-alive-p hunt)
    (record-event 'hunter-slain hunt)))

(defun eval-arrow-path-classic (hunt)
  "What happens when an arrow is shot through the cave - classic
wumpus"
  (cond
    ((wumpus-in-arrow-path-p hunt)
     (arrow-strikes-wumpus hunt))
    ((hunter-in-arrow-path-p hunt)
     (arrow-strikes-hunter hunt))
    (T
     (record-event 'arrow-strikes-ground hunt)
     (wake-wumpus 'arrow-strikes-ground hunt))))

;;; TODO: Bats may move if an arrow passes through their chamber. What
;;; happens when bats move into a chamber with: a pit? A wumpus? A
;;; hunter? Other bats?

;; (defun arrow-scatters-bats (hunt)
;;   "Bats may move if an arrow passes through their chamber."
;;   (loop
;;      for chamber in (cave-bats hunt)
;;      do
;;        (when (chamber-in-path-p chamber (latest-shot))
;; 	 (loop

(defun eval-arrow-path-fancy (hunt)
  "What happens when an arrow is shot through the cave? Arrows can fall in
pit, scare bats."
  (cond
    ((wumpus-in-arrow-path-p hunt)
     (arrow-strikes-wumpus hunt))
    ((hunter-in-arrow-path-p hunt)
     (arrow-strikes-hunter hunt))
    ((find (arrow-locale (latest-shot hunt)) (cave-pits hunt))
     (record-event 'arrow-fell-in-pit hunt))
    (T
     (record-event 'arrow-strikes-ground hunt)
     (wake-wumpus 'arrow-strikes-ground hunt))))

;; TODO: provide a more interesting arrow-path evaluation--for
;; example, an arrow that falls in pit might not wake the Wumpus

(defun hunter-shoots (arrow-path hunt)
  "The hunter shoots an arrow"
  (assert (not (quiver-void-p hunt)))
  (assert (path-in-cave-p arrow-path (cave-passages hunt)))
;;  (push arrow-path (arrow-paths hunt))
  (record-event 'hunter-shoots-arrow hunt)
  (lose-arrow hunt)
  (push (correct-path-steer arrow-path (cave-passages hunt)) (arrow-paths hunt))
  (funcall (symbol-function (arrow-path-eval hunt))
	   hunt)
  (events-since 'hunter-shoots-arrow hunt))


;;;; Output

(defparameter *wumpus-out* *standard-output*
  "What stream does wumpus print messages to?")

(defparameter *wumpus-output-format* "~:@(~@?~)"
  "Set to Nil to print in mixed case.")

(defun make-wumpus-message (string)
  "Formats a string into the Wumpus format."
  (check-type string string)
  (with-standard-io-syntax
    (apply #'format Nil
	   (if *wumpus-output-format*
	       (list *wumpus-output-format* string)
	       (list string)))))

(defun write-wumpus-line (string &key (stream *wumpus-out*))
  "Write messages to Wumpus player."
  (fresh-line stream)
  (format stream (make-wumpus-message string))
  (finish-output stream))

(defun send-wumpus-message (&optional string (stream *wumpus-out*))
  "Output a wumpus message"
  (when string
    (write-wumpus-line string :stream stream)))

;;; FIXME: reinventing the wheel

(defparameter *chamber-forms*
  '((corner . (quote . quote))
    (wumpus . (1-    . 1+)))
  "(read . write) forms for chambers")

(defun get-read-chamber-function (form-name)
  "Get the read-chamber function"
  (symbol-function
   (car (cdr (find form-name *chamber-forms* :key 'first)))))

(defun get-write-chamber-function (form-name)
  "Get the write-chamber function"
  (symbol-function
   (cdr (cdr (find form-name *chamber-forms* :key 'first)))))

(defparameter *chamber-form-name-default* 'wumpus
  "The default chamber form name")

;;; TODO: expose this option in game environment.

(defun write-wumpus-chamber-to-string (chamber)
  "Write a wumpus chamber to a string"
  (format Nil "~D"
	  (funcall
	   (get-write-chamber-function *chamber-form-name-default*)
		   chamber)))

(defun write-wumpus-chambers (chambers)
  "Write a list of wumpus chambers to a string"
  (loop
     for chamber in chambers
     collect (write-wumpus-chamber-to-string chamber)))

;;; FIXME The output form shouldn't have to be looked up every time
;;; it's called but defined once-per-game and used throughout.

(defparameter *wumpus-io* *query-io*
  "What stream does wumpus write prompts to")

(defun write-wumpus-prompt (prompt &key (stream *wumpus-io*) (fresh-line Nil))
  "Write a wumpus prompt."
  (when fresh-line
    (fresh-line stream))
  (princ (make-wumpus-message prompt) stream)
  (finish-output stream))

(defun read-wumpus-char (&optional (stream *wumpus-io*))
  "Read a single character from wumpus player input."
  (clear-input stream)
  (with-standard-io-syntax
    (read-char stream)))

(defun read-wumpus-line (&optional (stream *wumpus-io*))
  "Read input from Wumpus player."
  (clear-input stream)
  (with-standard-io-syntax
    (read-line stream)))

(defun read-wumpus-int (&optional (stream *wumpus-io*))
  "Read a number from wumpus player input."
  (parse-integer (read-wumpus-line stream) :junk-allowed T))

(defun read-wumpus-chamber (&optional (stream *wumpus-io*))
  "Read a tunnel number."
  (let ((chamber (read-wumpus-int stream)))
    (when chamber
      (funcall (get-read-chamber-function *chamber-form-name-default*)
	       chamber))))

(defmacro with-wumpus-input ((var prompt
				  &key
				  (readf #'read-wumpus-line)
				  (writef #'write-wumpus-prompt))
			     &body body)
  "Read input and do something with it or set it to Nil a different input is needed."
  (let ((out (gensym "OUTCOME")))
    `(loop
	with ,var = Nil
	with ,out = Nil
	do
	  (funcall ,writef ,prompt)
	  (setf ,var (funcall ,readf))
	  (setf ,out ,@body)
	until (not (null ,var))
	finally (return ,out))))


;;;; Prompts and Messages

(defun write-title ()
  "Print the wumpus title"
  (write-wumpus-line "Hunt The Wumpus"))

(defun instructions-p ()
  "Ask if the user wishes to see instructions"
  (write-wumpus-prompt "Instructions (Y-N) " :fresh-line T)
  (not (eq #\n (char-downcase (read-wumpus-char)))))

(defun make-message-instructions ()
  "Make the instructions message"
  (with-output-to-string (message)
    (format message
	    "~&Welcome to 'Hunt The Wumpus'~%~
~&  The Wumpus lives in a cave of 20 rooms. Each room~%~
~&has 3 tunnels leading to other rooms. (Look at a~%~
~&dodecahedron to see how this works-if you don't know~%~
~&what a dodecahedron is, ask someone)~2%~

~&     Hazards:~%~
~& Bottomless pits - Two rooms have bottomless pits in them~%~
~&     If you go there, you fall into the pit (& lose!)~%~
~& Super bats - Two other rooms have super bats. If you~%~
~&     go there, a bat grabs you and takes you to some other~%~
~&     room at random. (Which may be troublesome)~%")
    ;; Hit RETURN to continue;a$
    (format message
	    "~&Wumpus:~%~
~& The Wumpus is not bothered by hazards (he has sucker~%~
~& feet and is too big for a bat to lift).  Usually~%~
~& he is asleep.  Two things wake him up: you shooting an~%~
~& arrow or you entering his room.~%~
~&     If the Wumpus wakes he moves (p=.75) one room~%~
~& or stays still (p=.25).  After that, if he is where you~%~
~& are, he eats you up and you lose!~2%~

~&    You:~%~
~& Each turn you may move or shoot a crooked arrow~%~
~&   Moving:  You can move one room (thru one tunnel)~%~
~&   Arrows:  You have 5 arrows.  You lose when you run out~%~
~&   each arrow can go from 1 to 5 rooms. You aim by telling~%~
~&   the computer the rooms you want the arrow to go to.~%~
~&   If the arrow can't go that way (if no tunnel) it moves~%~
~&   at random to the next room.~%~
~&     If the arrow hits the wumpus, you win.~%~
~&     If the arrow hits you, you lose.~%")
    ;; Hit RETURN to continue;a$
    (format message
	    "~&Warnings:~%~
~&     When you are one room away from a wumpus or hazard,~%~
~&     the computer says:~%~
~& Wumpus:  'I smell a wumpus'~%~
~& Bat   :  'Bats nearby'~%~
~& Pit   :  'I feel a draft'~%")))

;; TODO: figure out how to make an output-pager that does this.
;; TODO: get wumpus 2's introduction


;;;; The Game Environment

(defparameter *game-event-messages-classic*
  '((arrow-strikes-wumpus  "Aha! You got the Wumpus!")
    (arrow-strikes-hunter  "Ouch! Arrow got you!")
    (arrow-strikes-ground  "Missed")
    (hunter-enters-chamber make-message-hunter-enters-chamber)
    (hunter-fell-in-pit    "YYYYIIIIEEEE . . . Fell in a pit")
    (hunter-bumps-wumpus   "... Oops! Bumped a Wumpus!")
    (wumpus-mauls-hunter   "Tsk tsk tsk - Wumpus got you!")
    (bat-snatches-hunter   "ZAP--Super bat snatch! Elsewhereville for you!")
    (hunter-smells-wumpus  "I smell a Wumpus!")
    (hunter-hears-bats     "Bats nearby!")
    (hunter-feels-draft    "I feel a draft"))
  "Event messages in game. Should be a string, function, or a symbol
  of a function which takes the game for an argument and returns a string.")

(defparameter *game-event-messages-wobh*
  '((arrow-strikes-wumpus  "A roar of pain and rage fills the caverns.")
    (arrow-strikes-hunter  "You cry out in agony as the arrow reenters the chamber and pierces you.")
    (arrow-strikes-ground  "You hear a clattering sound in the distance")
    (hunter-enters-chamber make-message-hunter-enters-chamber)
    (hunter-fell-in-pit    "YYYYIIIIEEEE . . . Fell in a pit")
    (hunter-bumps-wumpus   "You stumble into something large and warm.")
    (wumpus-mauls-hunter   "The darkness fills with claws, teeth, and pain.")
    (bat-snatches-hunter   "With a loud squeaking, you are lifted and carried off.")
    (hunter-smells-wumpus  "A terrible stench causes you to gasp.")
    (hunter-hears-bats     "You hear squeaking sounds nearby.")
    (hunter-feels-draft    "You feel a draft."))
  "Event messages in game. Should be a string, function, or a symbol
  of a function which takes the game for an argument and returns a string.")



(defparameter *game-input-prompts-classic*
  '((make-arrow-path "Room # ")
    (same-arrow-path "Arrows aren't that crooked - try another room")
    (get-arrow-range "No. of rooms (~D-~D) ")
    (get-hunter-move "Where to ")
    (err-hunter-move "Not possible - ")
    (get-hunter-action "Shoot or move (S-M) "))
  "User input prompts in game")

(defparameter *eval-hunter-action-default* 'eval-hunter-action-classic
  "The original game did not allow the player to quit the game.")

(defparameter *eval-hunter-shot-default* 'eval-hunter-shot-classic
  "The original game required that you know what rooms were connected
  when creating the shot.")

(defparameter *make-arrow-path-default* 'make-arrow-path-classic
  "The original game sent an arrow to a random nearby chamber if the
  path was incorrect.")

(defparameter *play-again-default* 'same-setup-p
  "Original Wumpus did not ask if you wished to quit game, only if you
  wanted the same cave setup as before.")


(defstruct (game (:conc-name game-))
  (event-messages     *game-event-messages-classic*)
  (input-prompts      *game-input-prompts-classic*)
  (eval-hunter-action *eval-hunter-action-default*)
  (eval-hunter-shot   *eval-hunter-shot-default*)
  (make-arrow-path    *make-arrow-path-default*)
  (use-play-again     *play-again-default*)
  (hunt Nil))

(defun get-event-message (event game)
  "Return the message string for a hunt event"
  (let ((message
	 (second (find event (game-event-messages game) :key 'first))))
    (etypecase message
      (null     Nil)
      (string   message)
      (symbol   (funcall (symbol-function message) game))
      (function (funcall message game)))))

(defun set-event-message (event game message)
  "Set an event message in game-event-messages"
  (if (find event (game-event-messages game) :key 'first)
      (setf (second (find event (game-event-messages game) :key 'first))
	    message)
      (push (list event message) (game-event-messages game))))

(defsetf get-event-message set-event-message)

(defun make-message-from-events (events game)
  "Return a message string from a sequence of events"
  (with-output-to-string (message)
    (loop
       for event in (nreverse events)
       do
	 (let ((event-msg (get-event-message event game)))
	   (unless (null event-msg)
	     (write-wumpus-line event-msg :stream message))))))

(defun get-input-prompt (prompter game)
  "Get a prompt from *wumpus-input-prompts*"
  (let ((prompt
	 (second (find prompter (game-input-prompts game) :key 'first))))
    (etypecase prompt
      (string   prompt)
      (symbol   (funcall (symbol-function prompt) game))
      (function (funcall prompt game)))))

(defun set-input-prompt (prompter game prompt)
  "Set a prompt in game-input-prompts"
  (if (find prompter (game-input-prompts game) :key 'first)
      (setf (second (find prompter (game-input-prompts game) :key 'first)) prompt)
      (push (list prompter prompt) (game-input-prompts game))))

(defsetf get-input-prompt set-input-prompt)



(defparameter *allow-quit* Nil
  "Classic Wumpus did not allow the player to quit.")

(defparameter *play-again* Nil
  "The original game jumped straight to same-setup.")

(defparameter *cancel-shot-with-zero* Nil
  "Allows the player to cancel the shot by entering 0")

(defparameter *show-near-chambers-in-make-arrow-path* Nil
  "Show nearby chambers in make-arrow-path prompt.")

(defun setup-game (&key (hunt Nil)
		   (allow-quit            *allow-quit*)
		   (play-again            *play-again*)
		   (cancel-shot-with-zero *cancel-shot-with-zero*)
		   (show-near-chambers
		    *show-near-chambers-in-make-arrow-path*)
		   (event-messages *game-event-messages-classic*))
  "Setup game game environment"
  (let* ((game (make-game :event-messages event-messages))
	 (bow-range-minimum 1))
    (setf (game-hunt game) (or hunt (setup-hunt)))
    (when allow-quit
      (setf (game-eval-hunter-action game) 'eval-hunter-action-with-quit)
      (setf (get-input-prompt 'get-hunter-action game)
	    "Shoot, move, or quit (S-M-Q) "))
    (when play-again
      (setf (game-use-play-again game) 'play-again-p))
    (when show-near-chambers
      (setf (game-make-arrow-path game) 'make-arrow-path-show-near-chambers))
    (when cancel-shot-with-zero
      (setf bow-range-minimum 0)
	(setf (game-eval-hunter-shot game) 'eval-hunter-shot-cancel))
    (setf (get-input-prompt 'get-arrow-range game)
	  (format Nil (get-input-prompt 'get-arrow-range game)
		  bow-range-minimum
		  (bow-range-maximum (game-hunt game))))
    game))

(defun make-arrow-path-wumpus-dead-shot (hunt)
  "Make a an arrow path that leads to the wumpus"
  (list (wumpus-locale hunt)
	(random-chamber-nearby (wumpus-locale hunt) (cave-passages hunt))))

(defun make-arrow-path-hunter-dead-shot (hunt)
  "Make a an arrow path that leads to the hunter"
  (list (hunter-locale hunt)
	(random-chamber-nearby (hunter-locale hunt) (cave-passages hunt))))

;;; TODO: write a path-finding function, for testing.

(defun make-arrow-path-show-near-chambers (range game)
  "Make an arrow path with the prompt showing nearby chambers"
  (assert (in-range-p range (game-hunt game)))
  (with-accessors ((hunt game-hunt)) game
    (loop
       with arrow-path = (list (hunter-locale hunt))
       for i from 0 below range
       do
	 (with-wumpus-input (passage (get-input-prompt 'make-arrow-path game)
				     :readf #'read-wumpus-chamber)
	   (cond
	     ((and (< 2 (length (latest-shot hunt)))
		   (= passage (second (latest-shot hunt))))
	      (write-wumpus-line
	       (get-input-prompt 'same-arrow-path game))
	      (setf passage Nil))
	     (T
	      (push passage arrow-path))))
       finally (return (latest-shot hunt)))))

(defun make-arrow-path-classic (range game)
  "Make an arrow path"
  (assert (in-range-p range (game-hunt game)))
  (with-accessors ((hunt game-hunt)) game
    (loop
       with arrow-path = (list (hunter-locale hunt))
       with prompt = (get-input-prompt 'make-arrow-path game)
       for i from 0 below range
       do
	 (with-wumpus-input (passage prompt
				     :readf #'read-wumpus-chamber)
	   (cond
	     ((and (< 2 (length arrow-path))
		   (= passage (second arrow-path)))
	      (write-wumpus-line
	       (get-input-prompt 'same-arrow-path game))
	      (setf passage Nil))
	     (T
	      (push passage arrow-path))))
       finally (return arrow-path))))

(defun eval-hunter-shot-cancel (game)
  "What happens when the hunter shoots an arrow."
  (with-wumpus-input (range (get-input-prompt 'get-arrow-range game)
			    :readf #'read-wumpus-int)
    (cond
      ((in-range-p range (game-hunt game))
       (hunter-shoots
	(funcall (symbol-function (game-make-arrow-path game))
		 range game)
	(game-hunt game)))
      ((zerop range) Nil)
      (T
       (setf range Nil)))))

(defun eval-hunter-shot-classic (game)
  "What happens when the hunter shoots an arrow."
  (with-wumpus-input (range (get-input-prompt 'get-arrow-range game)
			    :readf #'read-wumpus-int)
    (cond
      ((in-range-p range (game-hunt game))
       (hunter-shoots
	(funcall (symbol-function (game-make-arrow-path game))
		 range game)
	(game-hunt game)))
      (T
       (setf range Nil)))))

(defun eval-hunter-move (game)
  "What happens when a hunters moves to a new chamber"
  (with-wumpus-input (chamber (get-input-prompt 'get-hunter-move game)
			      :readf #'read-wumpus-chamber)
    (with-accessors ((hunt game-hunt)) game
      (cond
	((passage-to-p (hunter-locale hunt) chamber (cave-passages hunt))
	 (hunter-moves chamber hunt))
	(T
	 (write-wumpus-prompt (get-input-prompt 'err-hunter-move game))
	 (setf chamber Nil))))))

;; FIXME move-hunter creates an error if the move is invalid,
;; possibly, I can use condition system.

(defun eval-player-quit (game)
  "Player quits game"
  (with-wumpus-input (input "Do you wish to continue playing? "
			    :readf #'read-wumpus-char)
    (case (char-downcase input)
      (#\y Nil)
      (#\n (player-quits-hunt (game-hunt game)))
      (T
       (setf input Nil)))))

(defun eval-hunter-action-classic (game)
  "Shoot or move"
  (fresh-line *wumpus-io*)
  (make-message-from-events 
   (with-wumpus-input (input (get-input-prompt 'get-hunter-action game)
			     :readf #'read-wumpus-char)
     (case (char-downcase input)
       (#\s (funcall (symbol-function (game-eval-hunter-shot game))
		     game))
       (#\m (eval-hunter-move game))
       (T
	(setf input Nil))))
   game))

(defun eval-hunter-action-with-quit (game)
  "Shoot, move, or quit."
  (fresh-line *wumpus-io*)
  (make-message-from-events 
   (with-wumpus-input (input (get-input-prompt 'get-hunter-action game)
			     :readf #'read-wumpus-char)
     (case (char-downcase input)
       (#\s (funcall (symbol-function (game-eval-hunter-shot game))
		     game))
       (#\m (eval-hunter-move game))
       (#\q (eval-player-quit (game-hunt game)))
       (T
	(setf input Nil))))
     game))

(defun make-prompt-arrow-path-show-near-chambers (game)
  "Make prompt for make-arrow-path which shows nearby chambers"
  (with-accessors ((hunt game-hunt)) game
    (format Nil "Room # (~{~A~^, ~}) "
	    (write-wumpus-chambers
	     (chambers-nearby
	      (arrow-locale (latest-shot hunt)) (game-hunt game))))))

(defun make-message-ending (game)
  "What does the game say when it ends?"
  (with-accessors ((hunt game-hunt)) game
    (assert (hunt-end-p hunt))
    (cond ((eq (latest-event hunt) 'wumpus-slain)
	   "Hee hee hee - The Wumpus'll get you next time!!")
	  (T
	   "Ha ha ha - You lose!"))))

(defun make-message-hunter-enters-chamber (game)
  "What does the game say at the beginning of a game turn?"
  (with-accessors ((hunt game-hunt)) game
    (unless (hunt-end-p hunt)
      (with-output-to-string (message)
	(format message "You are in room ~A~%~
                       ~&Tunnels lead to ~{~A ~}"
		(write-wumpus-chamber-to-string (hunter-locale hunt))
		(write-wumpus-chambers (chambers-near-hunter hunt)))))))

(defun same-setup-p (game)
  "Play again with the same cave as last time?"
  (write-wumpus-prompt "Same setup (Y-N) " :fresh-line T)
  (cond ((eq #\y (char-downcase (read-wumpus-char)))
	 (setf (game-hunt game) (reset-hunt (game-hunt game)))
	 game)
	(T
	 (setup-game))))

(defun play-again-p (game)
  "Play again?"
  (write-wumpus-prompt "Play again (Y-N) " :fresh-line T)
  (when (eq #\y (char-downcase (read-wumpus-char)))
    (same-setup-p game)))

(defun begin-hunt (game)
  "The hunt begins!"
  (make-message-from-events
   (hunter-enters-cave (game-hunt game)) game))

(defun main (&key (game Nil))
  "Hunt the Wumpus"
  (write-title)
  (when (instructions-p)
    (write-wumpus-line (make-message-instructions)))
  (let ((env (or game (setup-game))))
    (loop
       with play = T
       do
	 (write-wumpus-line (begin-hunt env))
	 (loop
	    do
	      (write-wumpus-line
	       (funcall (game-eval-hunter-action env) env))
	    until (hunt-end-p (game-hunt env))
	    finally (write-wumpus-line
		     (make-message-ending env)))
	 (setf play
	       (funcall
		(symbol-function (game-use-play-again env))
		env))
	 (when play
	   (setf env play))
       until (null play)
       finally (return env))))

;;;; FIXME: all these game options need better organization. I may
;;;; need to include messages, prompts, and possibly random states.


;;;; Wumpus Test functions

;;; TODO move these to defpackage when testing setup is complete

;; (:export "*TEST-RANDOM-STATE*")
;; (:export "*TEST-HUNT*" "SETUP-TEST-HUNT")
;; (:export "*TEST-GAME*" "SETUP-TEST-GAME")
;; (:export "*TEST-MAIN")


(defparameter *test-random-state* (make-random-state T)
  "A random state that can be reused for test games")

(defparameter *test-hunt* Nil)

(defun setup-test-hunt (&key
			(cave-name         *cave-name-default*)
			(wumpus-health     *wumpus-health-setup*)
			(wumpus-hurt       *wumpus-hurt-setup*)
			(hunter-health     *hunter-health-setup*)
			(bow-range-maximum *bow-range-maximum*)
			(quiver-room       *quiver-room-setup*)
			(quiver-hold       *quiver-hold-setup*)
			(arrow-path-eval   *arrow-path-eval-setup*))
  "Set *test-hunt* with *test-random-state*"
  (setf *hunt-random-state* (make-random-state *test-random-state*))
  (setf *test-hunt*
	(setup-hunt
	 :cave-name         cave-name
	 :wumpus-health     wumpus-health
	 :wumpus-hurt       wumpus-hurt
	 :hunter-health     hunter-health
	 :bow-range-maximum bow-range-maximum
	 :quiver-room       quiver-room
	 :quiver-hold       quiver-hold
	 :arrow-path-eval   arrow-path-eval)))



(defparameter *test-game* Nil)

(defun setup-test-game (&key
                          (hunt                  (or *test-hunt* (setup-test-hunt)))
                          (allow-quit            *allow-quit*)
                          (play-again            *play-again*)
                          (cancel-shot-with-zero *cancel-shot-with-zero*)
                          (show-near-chambers    *show-near-chambers-in-make-arrow-path*))
  "Set *test-game* with hunt (or test-hunt)"
  (setf *test-game*
	(setup-game :hunt                  hunt
		    :allow-quit            allow-quit
		    :play-again            play-again
		    :cancel-shot-with-zero cancel-shot-with-zero
		    :show-near-chambers    show-near-chambers)))

(defun setup-game-wobh ()
  "Setup game with my favorite new settings."
  (setup-game
   :hunt (setup-hunt
	  :cave-name 'dodecahedron-circuit
	  :wumpus-health 3
	  :wumpus-hurt 'wumpus-enraged
	  :hunter-health 2
	  :arrow-path-eval 'eval-arrow-path-fancy)
   :event-messages *game-event-messages-wobh*
   :allow-quit T
   :play-again T
   :cancel-shot-with-zero T))


;; (defun test (&optional game)
;;   (let* ((*print-pretty* T)
;; 	 (*allow-quit* T)
;; 	 (env (or game (setup-game-test))))
;;     (setf (game-eval-hunter-action env) 'eval-hunter-action-test)
;;     (loop
;;        do
;; 	 (print env)
;; 	 (terpri)
;; 	 (write-wumpus-line
;; 	  (make-message-turn env))
;; 	 (funcall (game-eval-hunter-action env) env)
;;        until (hunt-end-p env))
;;     env))

;; FIXME: I'm still confused about what this test game main function
;; should do. I want the test player to be able to inspect the game
;; structure during play. I'm not sure how I want it to work.
