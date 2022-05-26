;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.games.wumpus-user
  (:use #:common-lisp)
  (:nicknames #:wumpus-user)
  (:local-nicknames (#:wumpus #:org.wobh.common-lisp.games.wumpus))
  (:import-from #:wumpus
                #:play
                #:make-hunt
                #:setup-hunt
                #:reset-hunt
                #:list-cave-networks
                #:get-cave-network
		#:*allow-quit*
		#:*arrow-path-eval-setup*
		#:*bow-range-maximum*
		#:*quiver-hold-setup*
		#:*quiver-room-setup*
		#:*hunter-health-setup*
		#:*wumpus-hurt-setup*
		#:*wumpus-health-setup*
		#:*cave-name-default*
		#:*show-near-chambers-in-make-arrow-path*
		#:*cancel-shot-with-zero*
		#:*game-event-messages-wobh*
		#:*play-again*)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.WUMPUS-USER
"))

(in-package #:org.wobh.common-lisp.games.wumpus-user)

;;;; Wumpus Test functions

;;; TODO move these to defpackage when testing setup is complete

;; (:export "*TEST-RANDOM-STATE*")
;; (:export "*TEST-HUNT*" "SETUP-TEST-HUNT")
;; (:export "*TEST-GAME*" "SETUP-TEST-GAME")
;; (:export "*TEST-MAIN")


(defparameter *test-random-state* (make-random-state t)
  "A random state that can be reused for test games")

(defparameter *test-hunt* nil)

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



(defparameter *test-game* nil)

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
   :allow-quit t
   :play-again t
   :cancel-shot-with-zero t))


;; (defun test (&optional game)
;;   (let* ((*print-pretty* t)
;;       (*allow-quit* t)
;;       (env (or game (setup-game-test))))
;;     (setf (game-eval-hunter-action env) 'eval-hunter-action-test)
;;     (loop
;;        do
;;       (print env)
;;       (terpri)
;;       (write-wumpus-line
;;        (make-message-turn env))
;;       (funcall (game-eval-hunter-action env) env)
;;        until (hunt-end-p env))
;;     env))

;; FIXME: I'm still confused about what this test game main function
;; should do. I want the test player to be able to inspect the game
;; structure during play. I'm not sure how I want it to work.
