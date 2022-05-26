;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.games.wumpus-test
  (:use #:common-lisp)
  (:local-nicknames (#:wumpus #:org.wobh.common-lisp.games.wumpus))
  (:import-from #:wumpus
                #:get-cave-network
                #:count-chambers-in-cave
                #:chamber-in-cave-p)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.WUMPUS-TEST
"))

(in-package #:org.wobh.common-lisp.games.wumpus-test)

(let* ((variant :dodecahedron)
       (subject (get-cave-network variant)))
  (let ((expect 20)
        (actual (count-chambers-in-cave subject)))
    (assert (= expect actual)
            (variant expect actual)
            "The ~A cave is expected to have ~D chambers, not ~D."
            variant
            expect
            actual))
  (let* ((indyvar 20)
         (expect nil)
         (actual (chamber-in-cave-p indyvar subject)))
    (assert (eql expect actual)
            (variant indyvar)
            "The ~A cave is expected to not have chamber ~D"
            variant
            indyvar)))
