;;;; -*- Mode:lisp;coding:utf-8 -*-
;;;; FILE: wumpus.lisp

;;;; DESCRIPTION

;;;; Common Lisp version of Gregory Yob's classic game

;;;; AUTHORS

;;;; William H. Clifford <wobh@wobh.org>

;;;; NOTES

;;;; http://www.atariarchives.org/bcc1/showpage.php?page=247
;;;; http://www.wurb.com/if/game/442

;;;; The original Wumpus got expanded in Wumpus II and Wumpus III. In
;;;; Gregory Yob's Creative Computing articles about the Wumpus games,
;;;; he makes a number of suggestions about adapting and modifying the
;;;; game.

;;;; "If you are a Wumpus fiend, make a version in which he avoids
;;;; pits and superbats can carry him only one room (with the
;;;; possibility of being dumped into your cave)."

;;;; So the idea here is to adapt Wumpus for Common Lisp, allow the
;;;; classic I and Wumpus II games (I couldn't find source for Wumpus
;;;; III), but set it up for other possibilities.

;;;; New Features

;;;; It might take more than one arrow to slay the Wumpus (or the
;;;; Hunter). The Hunter might be able to survive a Wumpus mauling.

;;;; If the arrow strikes the Wumpus and the Wumpus survives, the
;;;; Wumpus might be able to track the arrow to the room it came
;;;; from. If the hunter or other scapegoat is not found, the still
;;;; enraged Wumpus might make one further move at random. This would
;;;; force the hunter to shoot from a couple of rooms away to be safe.

;;;; TODO

;;;; How do arrows interact with pits or bats?

;;;; Wounded Wumpuses and Hunters might heal. (eat-mushrooms?)

;;;; The Wumpus might wake and move around before settling
;;;; down again. (hunter-makes-noise?)

;;;; The Wumpus might track the Hunter through the caves.


(defpackage "ORG.WOBH.WUMPUS"
  (:nicknames "WUMPUS" "HUNT")
  (:use "COMMON-LISP")
  (:export "*CAVE-NETWORKS*" "GET-CAVE-NETWORK")
  (:export "MAKE-HUNT"
	   "SETUP-HUNT" "RESET-HUNT")
  (:export "SETUP-GAME")
  (:export "MAIN")
  (:documentation "HUNT THE WUMPUS

* MAIN

Launches game, defaults to classic Hunt the Wumpus settings.

** :GAME

Option to provide custom Hunt the Wumpus game environment


* SETUP-GAME

Creates Hunt the Wumpus game environment

** :HUNT

Option for providing a custom hunting setting.

** :ALLOW-QUIT

When true, allows player to quit game at action prompt.

** :CANCEL-SHOT-WITH-ZERO

When true, allows hunter to cancel shot by entering '0' at shot range
prompt.

** :SHOW-NEAR-CHAMBERS

When true, shows the player the nearby chambers to the last room in
arrow path. This makes accurately steering an arrow shot path a lot
easier.


* SETUP-HUNT

** :CAVE-NAME

Option to provide a cave name for custom cave. The cave names

- DODECAHEDRON :: Classic Wumpus cave. Default cave.
- DODECAHEDRON-CIRCUIT :: dodecahedron with Hamiltonian circuit
                          passages
- MOBIUS-STRIP :: From Wumpus II
- STRING-OF-BEADS :: From Wumpus II
- HEX-NET-ON-TORUS :: From Wumpus II
- DENDRITE-WITH-DEGENERACIES :: From Wumpus II
- ONE-WAY-LATTICE :: From Wumpus II

** :WUMPUS-HEALTH

Option for allowing the Wumpus to take more than one arrow
hit. Default, 1.

** :WUMPUS-HURT

Symbol of function which governs what the wumpus does when struck by an arrow and not slain.

- WUMPUS-BOTHERED :: will randomly move or stay
- WUMPUS-ENRAGED :: will try to follow the arrow into the room it came from, and beyond if no one found

** :HUNTER-HEALTH

Option for allowing the Hunter to survive more than one wumpus
mauling. Default 1.

** :BOW-RANGE-MAXIMUM

Option for setting the maximum range of the Hunter's bow. Default
5.

** :QUIVER-ROOM

Option for setting the maximum number of arrows the Hunter's quiver
can hold. Default 5.

** :QUIVER-HOLD

Option for setting the number of arrows the Hunter's quiver starts off
with. Default 5."))


(in-package "ORG.WOBH.WUMPUS")


;;;; Randomness

(defparameter *hunt-random-state* (make-random-state T))

(defun hunt-random (limit &optional (random-state *hunt-random-state*))
  (random limit random-state))

(defun hunt-random-elt (seq &optional (random-state *hunt-random-state*))
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

(defparameter *hunt-endings*
  '(player-quits-hunt
    hunter-fell-in-pit
    arrow-slays-hunter
    arrow-slays-wumpus
    wumpus-slays-hunter)
  "Possible ways for game to end")

(defparameter *wumpus-health-setup* 1
  "The number of arrow-strikes required to slay the Wumpus")

(defparameter *wumpus-hurt-setup* 'wumpus-bothered
  "What the Wumpus does when injured")

(defparameter *hunter-health-setup* 1
  "The number of arrow-strikes required to slay the Hunter")

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
  (hunt-events        ())
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
  (assert (find cave-name *cave-networks* :key 'first))
  (second (find cave-name *cave-networks* :key 'first)))

;;; FIXME: I think I have to either use keywords for cave names, or
;;; export all these cave names to make them useful externally.

;;; TODO: make-cave-network, make-random-cave-network



;;; Cave information

(defun count-chambers-in-cave (cave)
  "How many chambers are in cave"
  (length (cave-passages cave)))

(defun list-chambers-in-cave (cave)
  "A list of chambers in cave"
  (loop
     for chamber from 0 below (count-chambers-in-cave cave)
     collect chamber))

(defun valid-chamber-p (chamber cave)
  (find chamber (list-chambers-in-cave cave)))

(defun chambers-nearby (chamber cave)
  "What chambers have tunnels connecting to here?"
  (elt (cave-passages cave) chamber))

(defun chamber-in-cave-p (chamber cave)
  "Is this chamber in this cave?"
  (find chamber (list-chambers-in-cave cave)))

;; I don't know why, but at first I did chamber-in-cave-p this way:
;; (when (chambers-nearby chamber cave)
;;    chamber))
;; but I realized it had this interesting flaw:
;; chambers with no connecting tunnels are not in the cave
;; ((1 2 3) () () (0 1 2))

(defun random-chamber (cave)
  (hunt-random (count-chambers-in-cave cave)))

(defun random-chamber-nearby (here cave)
  (assert (chamber-in-cave-p here cave))
  (hunt-random-elt (chambers-nearby here cave)))

(defun passage-to-p (here there cave)
  "Does a tunnel connect this chamber with another?"
  (assert (chamber-in-cave-p here cave))
;;  (assert (chamber-in-cave-p there cave))
  (find there (chambers-nearby here cave)))

(defun valid-path-p (path cave)
  "Is this a valid path through this cave?"
  (when (notany #'null
		(list* (chamber-in-cave-p (first path) cave)
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

(defun chamber-in-path-p (chamber path cave)
  (assert (chamber-in-cave-p chamber cave))
  (assert (valid-path-p path cave))
  (position chamber (nreverse path)))


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
  (let* ((hunt
	  (make-hunt :cave-passages     (get-cave-network cave-name)
		     :wumpus-life       (list wumpus-health)
		     :wumpus-hurt       wumpus-hurt
		     :hunter-life       (list hunter-health)
		     :bow-range-maximum bow-range-maximum
		     :quiver-room       quiver-room
		     :quiver-hold       (list quiver-hold)
		     :arrow-path-eval   arrow-path-eval
		     :hunt-events       (list 'hunter-enters-cave)))
	 (random-chambers
	  (hunt-shuffle (list-chambers-in-cave hunt))))
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
  (setf (wumpus-life hunt) (last (wumpus-life hunt))
	(wumpus-path hunt) (last (wumpus-path hunt))
	(hunter-life hunt) (last (hunter-life hunt))
	(hunter-path hunt) (last (hunter-path hunt))
	(quiver-hold hunt) (last (quiver-hold hunt))
	(arrow-paths hunt) ()
	(hunt-events hunt) (last (hunt-events hunt)))
  hunt)


;;;; The Wumpus

(defun wumpus-health (wumpus)
  "What is the current health of the wumpus?"
  (first (wumpus-life wumpus)))

(defun wumpus-alive-p (wumpus)
  "Is the wumpus alive?"
  (< 0 (wumpus-health wumpus)))

(defun wumpus-locale (wumpus)
  "Where is the wumpus?"
  (first (wumpus-path wumpus)))

(defun hurt-wumpus (wumpus &optional (hurt 1))
  (assert (< 0 hurt))
  (with-accessors ((life wumpus-life)
		   (health wumpus-health)) wumpus
    (push (- health hurt) life)))

(defun heal-wumpus (wumpus &optional (heal 1))
  (assert (< 0 heal))
  (with-accessors ((life wumpus-life)
		   (health wumpus-health)) wumpus
    (push (+ health heal) life)))

(defun send-wumpus (locale wumpus)
  (assert (valid-chamber-p (wumpus-locale wumpus) wumpus))
  (push locale (wumpus-path wumpus)))


;;;; The Hunter

(defun hunter-health (hunter)
  "What is the current health of the hunter?"
  (first (hunter-life hunter)))

(defun hunter-alive-p (hunter)
  "Is the hunter alive?"
  (< 0 (hunter-health hunter)))

(defun hunter-locale (hunter)
  "Where is the hunter?"
  (first (hunter-path hunter)))

(defun hurt-hunter (hunter &optional (hurt 1))
  (assert (< 0 hurt))
  (with-accessors ((life hunter-life)
		   (health hunter-health)) hunter
    (push (- health hurt) life)))

(defun heal-hunter (hunter &optional (heal 1))
  (assert (< 0 heal))
  (with-accessors ((life hunter-life)
		   (health hunter-health)) hunter
    (push (+ health heal) life)))

(defun send-hunter (locale hunter)
  (assert (valid-chamber-p (hunter-locale hunter) hunter))
  (push locale (hunter-path hunter)))


;;;; The Hunter's Quiver

(defun quiver-holding (quiver)
  (first (quiver-hold quiver)))

(defun quiver-full-p (quiver)
  (= (quiver-holding quiver) (quiver-room quiver)))

(defun quiver-void-p (quiver)
  (< (quiver-holding quiver) 1))

(defun gain-arrow (quiver &optional (arrows 1))
  (assert (not (quiver-full-p quiver)))
  (with-accessors ((holding quiver-holding)
		   (room quiver-room)
		   (hold quiver-hold)) quiver
    (let ((sum (+ holding arrows)))
      (push (if (< sum room) sum room) hold))))

(defun lose-arrow (quiver &optional (arrows 1))
  (assert (not (quiver-void-p quiver)))
  (with-accessors ((holding quiver-holding)
		   (room quiver-room)
		   (hold quiver-hold)) quiver
    (let ((dif (- holding arrows)))
      (push (if (< dif 0) 0 dif) hold))))

(defun reset-quiver (quiver)
  (with-accessors ((hold quiver-hold)) quiver
    (setf hold (last hold))))


;;;; The Hunter's Bow

(defun in-range-p (distance bow)
  (<= 1 distance (bow-range-maximum bow)))


;;;; The Hunter's Arrows

(defun arrow-locale (arrow-path)
  (first arrow-path))

(defun arrow-source (arrow-path)
  (second arrow-path))

;; NOTE: all paths are expected to be in reverse order


(defun latest-shot (hunt)
  "What was the path of the last arrow shot?"
  (first (arrow-paths hunt)))

(defun track-latest-shot (hunt)
  "From what room did the arrow come from?"
  (arrow-source (latest-shot hunt)))

(defun chambers-near-wumpus (hunt)
  (chambers-nearby (wumpus-locale hunt) hunt))

(defun chambers-near-hunter (hunt)
  (chambers-nearby (hunter-locale hunt) hunt))

(defun wumpus-in-arrow-path-p (cave)
  "Position of wumpus in the latest arrow path. An arrow path always has
a hunter location as its last value."
  (chamber-in-path-p (wumpus-locale cave) (butlast (latest-shot cave)) cave))

(defun hunter-in-arrow-path-p (cave)
  "Position of hunter in the latest arrow path. An arrow path always has
a hunter location as its last value."
  (chamber-in-path-p (hunter-locale cave) (butlast (latest-shot cave)) cave))

(defun hunter-near-bat-p (cave)
  (intersection (chambers-near-hunter cave) (cave-bats cave)))
  
(defun hunter-with-bat-p (cave)
  (find (hunter-locale cave) (cave-bats cave)))

(defun hunter-near-pit-p (cave)
  (intersection (chambers-near-hunter cave) (cave-pits cave)))

(defun hunter-in-pit-p (cave)
  (find (hunter-locale cave) (cave-pits cave)))

(defun hunter-near-wumpus-p (cave)
  (find (hunter-locale cave) (chambers-near-wumpus cave)))

(defun hunter-with-wumpus-p (cave)
  (= (hunter-locale cave) (wumpus-locale cave)))


;;;; Hunt events

(defun record-event (event hunt)
  (push event (hunt-events hunt)))

(defun latest-event (hunt)
  (first (hunt-events hunt)))

(defun events-since (last-event hunt)
  (loop
     for event in (hunt-events hunt)
     collect event
     until (eq event last-event)))

(defun hunt-end-p (hunt)
  (find (latest-event hunt) (hunt-endings hunt)))

(defun wumpus-sleeps (hunt)
  (record-event 'wumpus-sleeps hunt))

(defun wumpus-moves (chamber hunt)
  (assert (passage-to-p (wumpus-locale hunt) chamber hunt))
  (send-wumpus chamber hunt)
  (record-event 'wumpus-moves hunt))

(defun wumpus-wanders (hunt)
  (wumpus-moves
   (random-chamber-nearby
    (wumpus-locale hunt) hunt) hunt))

(defun wumpus-enraged (hunt)
  (assert (equal (events-since 'arrow-strikes-wumpus hunt)
		 '(wumpus-wakes arrow-strikes-wumpus)))
  (wumpus-moves (track-latest-shot hunt) hunt)
  (unless (hunter-with-wumpus-p hunt)
    (wumpus-bothered hunt)))

(defun wumpus-bothered (hunt)
  (let ((chance (hunt-random 4)))
    (unless (= chance 3)
      (wumpus-wanders hunt))))

(defun wake-wumpus (cause hunt)
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
      (record-event 'wumpus-slays-hunter hunt)))
  (when (and (wumpus-alive-p hunt) (hunter-alive-p hunt))
    (wumpus-sleeps hunt)))

(defun arrow-strikes-wumpus (wumpus)
  (record-event 'arrow-strikes-wumpus wumpus)
  (hurt-wumpus wumpus)
  (cond ((wumpus-alive-p wumpus)
	 (wake-wumpus 'arrow-strikes-wumpus wumpus))
	(T (record-event 'arrow-slays-wumpus wumpus))))

(defun hunter-enters-chamber (hunt)
  (loop
     with hunter-moved = Nil
     do
       (record-event 'hunter-enters-chamber hunt)
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
	  (send-hunter (random-chamber hunt) hunt)
	  (setf hunter-moved T))
	 (T
	  (setf hunter-moved Nil)))
     until (null hunter-moved)))

(defun hunter-moves (chamber hunt)
  (assert (passage-to-p chamber (hunter-locale hunt) hunt))
  (record-event 'hunter-moves hunt)
  (send-hunter chamber hunt)
  (hunter-enters-chamber hunt)
  (events-since 'hunter-moves hunt))

(defun hunter-senses-chamber (hunt)
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

(defun arrow-strikes-hunter (hunt)
  (record-event 'arrow-strikes-hunter hunt)
  (hurt-hunter hunt)
  (unless (hunter-alive-p hunt)
    (record-event 'arrow-slays-hunter hunt)))

(defun eval-arrow-path-classic (arrow-path hunt)
  (cond
    ((wumpus-in-arrow-path-p hunt)
     (setf arrow-path
	   (subseq arrow-path 0
		   (1+ (wumpus-in-arrow-path-p hunt))))
     (arrow-strikes-wumpus hunt))
    ((hunter-in-arrow-path-p hunt)
     (setf arrow-path
	   (subseq arrow-path 0
		   (1+ (hunter-in-arrow-path-p hunt))))
     (arrow-strikes-hunter hunt))
    (T
     (record-event 'arrow-strikes-ground hunt)
     (wake-wumpus 'arrow-strikes-ground hunt))))

(defun eval-arrow-path-fancy (arrow-path hunt)
  (cond
    ((wumpus-in-arrow-path-p hunt)
     (setf arrow-path
	   (subseq arrow-path 0
		   (1+ (wumpus-in-arrow-path-p hunt))))
     (arrow-strikes-wumpus hunt))
    ((hunter-in-arrow-path-p hunt)
     (setf arrow-path
	   (subseq arrow-path 0
		   (1+ (hunter-in-arrow-path-p hunt))))
     (arrow-strikes-hunter hunt))
    ((find (arrow-locale arrow-path) (cave-pits hunt))
     (record-event 'arrow-fell-in-pit hunt))
    (T
     (record-event 'arrow-strikes-ground hunt)
     (wake-wumpus 'arrow-strikes-ground hunt))))

;; TODO: provide a more interesting arrow-path evaluation--for
;; example, an arrow that falls in pit might not wake the Wumpus

(defun hunter-shoots (arrow-path hunt)
  (assert (not (quiver-void-p hunt)))
  (assert (valid-path-p arrow-path hunt))
  (push arrow-path (arrow-paths hunt))
  (record-event 'hunter-shoots-arrow hunt)
  (lose-arrow hunt)
  (funcall (symbol-function (arrow-path-eval hunt))
	   arrow-path hunt)
  (events-since 'hunter-shoots-arrow hunt))


;;;; Output

(defparameter *wumpus-out* *standard-output*)

(defparameter *wumpus-output-format* "~:@(~@?~)"
  "Set to Nil to print in mixed case.")

(defun make-wumpus-message (string)
  (assert (stringp string))
  (with-standard-io-syntax
    (apply #'format Nil
	   (if *wumpus-output-format*
	       (list *wumpus-output-format* string)
	       (list string)))))

(defun send-wumpus-message (string &optional (stream *wumpus-out*))
  (format stream (make-wumpus-message string))
  (force-output stream))

(defun write-wumpus-line (string &optional (stream *wumpus-out*))
  "Write messages to Wumpus player."
  (write-line (make-wumpus-message string) stream)
  (force-output stream))

;;; FIXME: reinventing the wheel

(defparameter *chamber-forms*
  '((corner . (quote . quote))
    (wumpus . (1-    . 1+)))
  "(read . write) forms for tunnels")

(defun get-read-chamber-function (form-name)
  (symbol-function
   (car (cdr (find form-name *chamber-forms* :key 'first)))))

(defun get-write-chamber-function (form-name)
  (symbol-function
   (cdr (cdr (find form-name *chamber-forms* :key 'first)))))

(defparameter *wumpus-chamber-form-name* 'wumpus)

(defun write-wumpus-chamber-to-string (chamber)
  (format Nil "~D"
	  (funcall
	   (get-write-chamber-function *wumpus-chamber-form-name*)
		   chamber)))

(defun write-wumpus-chambers (chambers)
  (loop
     for chamber in chambers
     collect (write-wumpus-chamber-to-string chamber)))

;;; FIXME The output form shouldn't have to be looked up every time
;;; it's called but defined once-per-game and used throughout.

(defparameter *wumpus-io* *query-io*)

(defun write-wumpus-prompt (prompt &optional (stream *wumpus-io*))
  (princ (make-wumpus-message prompt) stream)
  (force-output stream))

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
      (funcall (get-read-chamber-function *wumpus-chamber-form-name*)
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
  (write-wumpus-line "Hunt The Wumpus"))

(defun instructions-p ()
  "Ask if the user wishes to see instructions"
  (write-wumpus-prompt "Instructions (Y-N) ")
  (not (eq #\n (char-downcase (read-wumpus-char)))))

(defun make-message-instructions ()
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

(defparameter *wumpus-event-messages*
  '(
;;    (hunter-enters-chamber "")
    (hunter-fell-in-pit    "YYYYIIIIEEEE . . . Fell in a pit")
    (hunter-bumps-wumpus   "... Oops! Bumped a Wumpus!")
    (bat-snatches-hunter   "ZAP--Super bat snatch! Elsewhereville for you!")
    (arrow-strikes-wumpus  "Aha! You got the Wumpus!")
    (arrow-strikes-hunter  "Ouch! Arrow got you!")
    (arrow-strikes-ground  "Missed")
;;    (arrow-slays-wumpus    "")
;;    (arrow-slays-hunter    "")
;;    (hunter-moves          "")
;;    (wumpus-moves          "")
;;    (wumpus-wakes          "")
;;    (wumpus-slays-hunter   "Tsk tsk tsk - Wumpus got you!")
    (hunter-smells-wumpus  "I smell a Wumpus!")
    (hunter-hears-bats     "Bats nearby!")
    (hunter-feels-draft    "I feel a draft"))
  "Event messages in game")

(defun get-event-message (event)
  (second (find event *wumpus-event-messages* :key 'first)))

(defun make-message-from-events (events)
  (with-output-to-string (message)
    (loop
       for event in (nreverse events)
       do
	 (let ((string (get-event-message event)))
	   (when string
	     (format message "~&~A~%" string))))))

(defun make-message-ending (hunt)
  (assert (hunt-end-p hunt))
  (cond ((eq (latest-event hunt) 'arrow-slays-wumpus)
	 "Hee hee hee - The Wumpus'll get you next time!!")
	(T
	 "Ha ha ha - You lose!")))

(defun make-message-turn (hunt)
  (unless (hunt-end-p hunt)
    (with-output-to-string (message)
      (format message "~2&You are in room ~A~%~
                        ~&Tunnels lead to ~{~A ~}~%"
	      (write-wumpus-chamber-to-string (hunter-locale hunt))
	      (write-wumpus-chambers (chambers-near-hunter hunt)))
      (format message "~&~A"
	      (make-message-from-events (hunter-senses-chamber hunt))))))

(defparameter *wumpus-input-prompts*
  '((make-arrow-path "Room # ")
    (same-arrow-path "Arrows aren't that crooked - try another room")
    (get-arrow-range "No. of rooms (~D-~D) ")
    (get-hunter-move "Where to ")
    (err-hunter-move "Not possible - ")
    (get-hunter-action "Shoot or move (S-M) "))
  "User input prompts in game")

(defun get-prompt (prompt)
  (second (find prompt *wumpus-input-prompts* :key 'first)))

(defun set-prompt (prompt text)
  (setf (second (find prompt *wumpus-input-prompts* :key 'first)) text))

(defsetf get-prompt set-prompt)


;;;; The Game Environment

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
  (eval-hunter-action *eval-hunter-action-default*)
  (eval-hunter-shot   *eval-hunter-shot-default*)
  (make-arrow-path    *make-arrow-path-default*)
  (use-play-again     *play-again-default*)
  (hunt Nil))


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
		   (show-near-chambers    *show-near-chambers-in-make-arrow-path*))
  (let* ((env (make-game))
	   (bow-range-minimum 1))
      (setf (game-hunt env) (or hunt (setup-hunt)))
      (when allow-quit
	(setf (game-eval-hunter-action env) 'eval-hunter-action-with-quit)
	(setf (get-prompt 'get-hunter-action) "Shoot, move, or quit (S-M-Q) "))
      (when play-again
	  (setf (game-use-play-again env) 'play-again-p))
      (when show-near-chambers
	(setf (game-make-arrow-path env) 'make-arrow-path-show-near-chambers))
      (when cancel-shot-with-zero
	(setf bow-range-minimum 0)
	(setf (game-eval-hunter-shot env) 'eval-hunter-shot-cancel))
      (setf (get-prompt 'get-arrow-range)
	    (format Nil (get-prompt 'get-arrow-range)
		    bow-range-minimum
		    (bow-range-maximum (game-hunt env))))
      env))

;; (defun make-arrow-path (prompt range hunt)
;; 	   (cond
;; 	     ((and (< 2 (length arrow-flight))
;; 		   (= passage (second arrow-flight)))
;; 	      (write-wumpus-line
;; 	       (get-prompt 'same-arrow-path))
;; 	      (setf passage Nil))
;; 	     ((passage-to-p (first arrow-flight) passage hunt)
;; 	      (send-arrow passage arrow-flight))
;; 	     (T
;; 	      (send-arrow (random-chamber-nearby (first arrow-flight) hunt)
;; 		    arrow-flight)))))
;;     (push arrow-flight (arrow-paths hunt))))

(defun make-arrow-path-wumpus-dead-shot (hunt)
  (list (random-chamber-nearby (wumpus-locale hunt) hunt)
	(wumpus-locale hunt)))

(defun make-arrow-path-hunter-dead-shot (hunt)
  (list (random-chamber-nearby (hunter-locale hunt) hunt)
	(hunter-locale hunt)))

;;; TODO: write a path-finding function, for testing.

(defun make-arrow-path-show-near-chambers (range hunt)
  (assert (in-range-p range hunt))
  (loop
     with arrow-flight = (list (hunter-locale hunt))
     with prompt = Nil
     for i from 0 below range
     do
       (setf prompt
	     (format Nil "Room # (~{~A~^, ~}) "
		     (write-wumpus-chambers
		      (chambers-nearby
		       (first arrow-flight) hunt))))
       (push
	(with-wumpus-input (passage prompt
				    :readf #'read-wumpus-chamber)
	  (cond
	    ((and (< 2 (length arrow-flight))
		  (= passage (second arrow-flight)))
	     (write-wumpus-line
	      (get-prompt 'same-arrow-path))
	     (setf passage Nil))
	    ((passage-to-p (first arrow-flight) passage hunt)
	     passage)
	    (T
	     (random-chamber-nearby (first arrow-flight) hunt))))
	arrow-flight)
     finally (return arrow-flight)))

(defun make-arrow-path-classic (range hunt)
  (assert (in-range-p range hunt))
  (loop
     with arrow-flight = (list (hunter-locale hunt))
     with prompt = (get-prompt 'make-arrow-path)
     for i from 0 below range
     do
       (push
	(with-wumpus-input (passage prompt
				    :readf #'read-wumpus-chamber)
	  (cond
	    ((and (< 2 (length arrow-flight))
		  (= passage (second arrow-flight)))
	     (write-wumpus-line
	      (get-prompt 'same-arrow-path))
	     (setf passage Nil))
	    ((passage-to-p (first arrow-flight) passage hunt)
	     passage)
	    (T
	     (random-chamber-nearby (first arrow-flight) hunt))))
	arrow-flight)
	finally (return arrow-flight)))

(defun eval-hunter-shot-cancel (env)
  (with-wumpus-input (range (get-prompt 'get-arrow-range)
			    :readf #'read-wumpus-int)
    (cond
      ((in-range-p range (game-hunt env))
       (make-message-from-events
	(hunter-shoots (funcall (symbol-function (game-make-arrow-path env))
				range (game-hunt env))
		       (game-hunt env))))
      ((zerop range)
       "")
      (T
       (setf range Nil)))))

(defun eval-hunter-shot-classic (env)
  (with-wumpus-input (range (get-prompt 'get-arrow-range)
			    :readf #'read-wumpus-int)
    (cond
      ((in-range-p range (game-hunt env))
       (make-message-from-events
	(hunter-shoots (funcall (symbol-function (game-make-arrow-path env))
				range (game-hunt env))
		       (game-hunt env))))
      (T
       (setf range Nil)))))

(defun eval-hunter-move (hunt)
  (with-wumpus-input (chamber (get-prompt 'get-hunter-move)
			      :readf #'read-wumpus-chamber)
    (cond
      ((passage-to-p (hunter-locale hunt) chamber hunt)
       (make-message-from-events (hunter-moves chamber hunt)))
      (T
       (write-wumpus-prompt (get-prompt 'err-hunter-move))
       (setf chamber Nil)))))

;; FIXME move-hunter creates an error if the move is invalid,
;; possibly, I can use condition system.

(defun eval-player-quit (hunt)
  (record-event 'player-quits-hunt hunt)
  (make-message-from-events
   (events-since 'player-quits-hunt hunt)))

(defun eval-hunter-action-classic (env)
  "Shoot or move"
  (with-wumpus-input (input (get-prompt 'get-hunter-action)
			    :readf #'read-wumpus-char)
    (case (char-downcase input)
      (#\s (funcall (symbol-function (game-eval-hunter-shot env))
		    env))
      (#\m (eval-hunter-move (game-hunt env)))
      (T
       (setf input Nil)))))

(defun eval-hunter-action-with-quit (env)
  "Shoot, move, or quit."
  (with-wumpus-input (input (get-prompt 'get-hunter-action)
			    :readf #'read-wumpus-char)
    (case (char-downcase input)
      (#\s (funcall (symbol-function (game-eval-hunter-shot env))
		    env))
      (#\m (eval-hunter-move (game-hunt env)))
      (#\q (eval-player-quit (game-hunt env)))
      (T
       (setf input Nil)))))

(defun same-setup-p (hunt)
  (write-wumpus-prompt "Same setup (Y-N) ")
  (if (eq #\y (char-downcase (read-wumpus-char)))
      (reset-hunt hunt)
      (setup-hunt)))

(defun play-again-p (hunt)
  (write-wumpus-prompt "Play again (Y-N) ")
  (when (eq #\y (char-downcase (read-wumpus-char)))
    (same-setup-p hunt)))

(defun main (&key (game Nil))
  (write-title)
  (when (instructions-p)
    (write-wumpus-line (make-message-instructions)))
  (let ((env (or game (setup-game))))
    (loop
       with play = T
       do
	 (loop
	    do
	      (write-wumpus-line
	       (make-message-turn (game-hunt env)))
	      (write-wumpus-line
	       (funcall (game-eval-hunter-action env) env))
	    until (hunt-end-p (game-hunt env))
	    finally (write-wumpus-line
		     (make-message-ending (game-hunt env))))
	 (setf play (funcall (symbol-function (game-use-play-again env)) (game-hunt env)))
	 (when play
	   (setf (game-hunt env) play))
       until (null play)
       finally (return env))))

;;;; FIXME: all these game options need better organization. I may
;;;; need to include messages, prompts, and possibly random states.


;;;; Wumpus Test functions

;;; TODO move these to defpackage when testing setup is complete

;; (:export "*TEST-RANDOM-STATE*")
;; (:export "*TEST-HUNT*" "SETUP-TEST-HUNT")
;; (:export "*TEST-GAME*" "SETUP-TEST-GAME")
;; (:export "*TEST-MAIND)


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
	 :cave-name         (get-cave-network cave-name)
	 :wumpus-health     wumpus-health
	 :wumpus-hurt       wumpus-hurt
	 :hunter-health     hunter-health
	 :bow-range-maximum bow-range-maximum
	 :quiver-room       quiver-room
	 :quiver-hold       quiver-hold
	 :arrow-path-eval   arrow-path-eval)))



(defparameter *test-game* Nil)

(defun setup-test-game  (&key
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

(defun setup-wumpus-wobh ()
  "Setup game with my favorite new settings."
  (setup-game
   :hunt (setup-hunt
	  :cave-name 'dodecahedron-circuit
	  :wumpus-health 3
	  :wumpus-hurt 'wumpus-enraged
	  :hunter-health 2
	  :arrow-path-eval 'eval-arrow-path-fancy)
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