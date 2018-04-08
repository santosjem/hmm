;; Lab: Probability
;; CSC 261 
;;
;; File
;;   analysis.scm
;;
;; Summary
;;   An implementation of Jayne's evidence and a collection of tests.
;;
;; Provides
;;   (evidence correct corrupt prior1 prior2 typist1 typist2)
;;   (test1 file-t1-correct file-t1-corrupt
;;          file-t2-correct file-t2-corrupt
;;          test-correct test-corrupt)
;;   (test2 file-t1-correct1 file-t1-corrupt1
;;          file-t1-correct2 file-t1-corrupt2
;;          file-t2-correct1 file-t2-corrupt1
;;          file-t2-correct2 file-t2-corrupt2
;;          test-correct test-corrupt)
;;   (test3 correct corrupt)

(load "charmodel.scm")

(define paths #f)
; #f for working within the github directory, rather than on mathlan

; Typist1
(define correct1
  (if paths 
  "/home/weinman/courses/CSC261/data/mills/original/1.txt"
  "original/1.txt"))
  
(define corrupt1
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/typist1/1.txt"
  "corrupted/typist1/1.txt"))

(define correct8
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/8.txt"
  "original/8.txt"))
  
(define corrupt8
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/typist1/8.txt"
  "corrupted/typist1/8.txt"))



(define correct16
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/16.txt"
  "original/16.txt"))
  
(define corrupt16
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/typist1/16.txt"
  "corrupted/typist1/16.txt"))

; Typist 2
(define correct4
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/4.txt"
  "original/4.txt"))
(define corrupt4
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/typist2/4.txt"
  "corrupted/typist2/4.txt"))

(define correct9
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/9.txt"
  "original/9.txt"))
(define corrupt9
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/typist2/9.txt"
  "corrupted/typist2/9.txt"))

(define correct18
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/18.txt"
  "original/18.txt"))
(define corrupt18
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/typist2/18.txt"
  "corrupted/typist2/18.txt"))


(define log10
  (lambda (num)
    (/ (log num) (log 10))))

;;; Procedure:
;;;   evidence
;;; Parameters:
;;;   correct, a filename
;;;   corrupt, a filename
;;;   prior1, an int corresponding to number of letters seen for typist 1
;;;   prior2, an int corresponding to number of letters seen for typist 2
;;;   typist1, a value
;;;   typist2, a value
;;; Purpose:
;;;   Calculates the jaynes evidence using the given arguments.
;;; Produces:
;;;;  e, a real number
;;; Preconditions:
;;;   correct and corrupt are valid filenames
;;;   typist1 and typist2 are the appropriate data structure
;;;   (created using create-counts
;;; Postconditions:
;;;   -inf < e < +inf
(define evidence
  (lambda (correct corrupt prior1 prior2 typist1 typist2)
    (letrec ([ln-to-log ;;defining ln-to-log base 10 helper function
              (lambda (num)
                (/ num
                   (log 10)))]
             [p1 (/ prior1 (+ prior1 prior2))]
             [p2 (/ prior2 (+ prior1 prior2))])
      (* 10 ;;multiply log by 10 for jaynes evidence
         (+ (log10 (/ p1 p2))
            (ln-to-log (- (log-likelihood correct corrupt typist1)
                          (log-likelihood correct corrupt typist2))))))))
;;; Procedure:
;;;   test1
;;; Parameters:
;;;   file-t1-correct, a filename
;;;   file-t2-correct, a filename
;;;   file-t1-corrupt, a filename
;;;   file-t2-corrupt, a filename
;;;   test-correct, a filename
;;;   test-corrupt, a filename
;;; Purpose:
;;;   Helper function to reduce repeated code for single letter training
;;; Produces:
;;;;  e, a real number
;;; Preconditions:
;;;   filenames are valid file names for correct and corrupt files
;;; Postconditions:
;;;   -inf < e < +inf
(define test1
  (lambda
      (file-t1-correct file-t1-corrupt
       file-t2-correct file-t2-corrupt
       test-correct test-corrupt)
    (let ([typist1 (create-counts 28 1)] ;def t1
          [typist2 (create-counts 28 1)]) ;def t2
      (count-conditionals! file-t1-correct file-t1-corrupt
                           typist1) ;instantiate t1
      (count-conditionals! file-t2-correct file-t2-corrupt
                           typist2) ;instantiate t2
      (normalize-counts! typist1)
      (normalize-counts! typist2)
      (evidence test-correct test-corrupt 1 1 typist1 typist2))))


;; Files corresponding to typists are as follows:
;; Typist 1 : 1, 8, 16
;; Typist 2: 4, 9, 18 
;; Calculating all possible permutations for testing with a single letter
;;;;;;;;;;;;;;;;;;;;;;;;TEST 1 START;;;;;;;;;;;;;;;;;;;;;;;;;
(display "TESTS WITH 1 LETTER EACH \n")
;; Test with 1 and 4
(display "Typist1: 1, Typist2: 4 \n")
(display "Test on 8:Typist1 \n")
(test1 correct1 corrupt1 correct4 corrupt4 correct8 corrupt8)
(display "Test on 16:Typist1 \n")
(test1 correct1 corrupt1 correct4 corrupt4 correct16 corrupt16)
(display "Test on 9:Typist2 \n")
(test1 correct1 corrupt1 correct4 corrupt4 correct9 corrupt9)
(display "Test on 18:Typist2 \n")
(test1 correct1 corrupt1 correct4 corrupt4 correct18 corrupt18)
(newline)

(display "Typist1: 8, Typist2: 4 \n")
(display "Test on 1:Typist1 \n")
(test1 correct8 corrupt8 correct4 corrupt4 correct1 corrupt1)
(display "Test on 16:Typist1 \n")
(test1 correct8 corrupt8 correct4 corrupt4 correct16 corrupt16)
(display "Test on 9:Typist2 \n")
(test1 correct8 corrupt8 correct4 corrupt4 correct9 corrupt9)
(display "Test on 18:Typist2 \n")
(test1 correct8 corrupt8 correct4 corrupt4 correct18 corrupt18)
(newline)

(display "Typist1: 16, Typist2: 4 \n")
(display "Test on 1:Typist1 \n")
(test1 correct16 corrupt16 correct4 corrupt4 correct1 corrupt1)
(display "Test on 8:Typist1 \n")
(test1 correct16 corrupt16 correct4 corrupt4 correct8 corrupt8)
(display "Test on 9:Typist2 \n")
(test1 correct16 corrupt16 correct4 corrupt4 correct9 corrupt9)
(display "Test on 18:Typist2 \n")
(test1 correct16 corrupt16 correct4 corrupt4 correct18 corrupt18)
(newline)

(display "Typist1: 1, Typist2: 9 \n")
(display "Test on 8:Typist1 \n")
(test1 correct1 corrupt1 correct9 corrupt9 correct8 corrupt8)
(display "Test on 16:Typist1 \n")
(test1 correct1 corrupt1 correct9 corrupt9 correct16 corrupt16)
(display "Test on 4:Typist2 \n")
(test1 correct1 corrupt1 correct9 corrupt9 correct4 corrupt4)
(display "Test on 18:Typist2 \n")
(test1 correct1 corrupt1 correct9 corrupt9 correct18 corrupt18)
(newline)

(display "Typist1: 8, Typist2: 9 \n")
(display "Test on 1:Typist1 \n")
(test1 correct8 corrupt8 correct9 corrupt9 correct1 corrupt1)
(display "Test on 16:Typist1 \n")
(test1 correct8 corrupt8 correct9 corrupt9 correct16 corrupt16)
(display "Test on 4:Typist2 \n")
(test1 correct8 corrupt8 correct9 corrupt9 correct4 corrupt4)
(display "Test on 18:Typist2 \n")
(test1 correct8 corrupt8 correct9 corrupt9 correct18 corrupt18)
(newline)

(display "Typist1: 16, Typist2: 9 \n")
(display "Test on 1:Typist1 \n")
(test1 correct16 corrupt16 correct9 corrupt9 correct1 corrupt1)
(display "Test on 8:Typist1 \n")
(test1 correct16 corrupt16 correct9 corrupt9 correct8 corrupt8)
(display "Test on 4:Typist2 \n")
(test1 correct16 corrupt16 correct9 corrupt9 correct4 corrupt4)
(display "Test on 18:Typist2 \n")
(test1 correct16 corrupt16 correct9 corrupt9 correct18 corrupt18)
(newline)

(display "Typist1: 1, Typist2: 18 \n")
(display "Test on 8:Typist1 \n")
(test1 correct1 corrupt1 correct18 corrupt18 correct8 corrupt8)
(display "Test on 16:Typist1 \n")
(test1 correct1 corrupt1 correct18 corrupt18 correct16 corrupt16)
(display "Test on 9:Typist2 \n")
(test1 correct1 corrupt1 correct18 corrupt18 correct9 corrupt9)
(display "Test on 4:Typist2 \n")
(test1 correct1 corrupt1 correct18 corrupt18 correct4 corrupt4)
(newline)

(display "Typist1: 8, Typist2: 18 \n")
(display "Test on 1:Typist1 \n")
(test1 correct8 corrupt8 correct18 corrupt18 correct1 corrupt1)
(display "Test on 16:Typist1 \n")
(test1 correct8 corrupt8 correct18 corrupt18 correct16 corrupt16)
(display "Test on 9:Typist2 \n")
(test1 correct8 corrupt8 correct18 corrupt18 correct9 corrupt9)
(display "Test on 4:Typist2 \n")
(test1 correct8 corrupt8 correct18 corrupt18 correct4 corrupt4)
(newline)

(display "Typist1: 16, Typist2: 18 \n")
(display "Test on 1:Typist1 \n")
(test1 correct16 corrupt16 correct18 corrupt18 correct1 corrupt1)
(display "Test on 8:Typist1 \n")
(test1 correct16 corrupt16 correct18 corrupt18 correct8 corrupt8)
(display "Test on 9:Typist2 \n")
(test1 correct16 corrupt16 correct18 corrupt18 correct9 corrupt9)
(display "Test on 4:Typist2 \n")
(test1 correct16 corrupt16 correct18 corrupt18 correct4 corrupt4)
(newline)
;;;;;;;;;;;;;;;;;;;;;;;;TEST 1 END;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Procedure:
;;;   test2
;;; Parameters:
;;;   file-t1-correct, a filename
;;;   file-t2-correct, a filename
;;;   file-t1-corrupt, a filename
;;;   file-t2-corrupt, a filename
;;;   test-correct, a filename
;;;   test-corrupt, a filename
;;; Purpose:
;;;   Helper function to reduce repeated code for single letter training
;;; Produces:
;;;;  e, a real number
;;; Preconditions:
;;;   filenames are valid file names for correct and corrupt files
;;; Postconditions:
;;;   -inf < e < +inf

(define test2
  (lambda
      (file-t1-correct1 file-t1-corrupt1
       file-t1-correct2 file-t1-corrupt2
       file-t2-correct1 file-t2-corrupt1
       file-t2-correct2 file-t2-corrupt2
       test-correct test-corrupt)
    (let ([typist1 (create-counts 28 1)] ;def t1
          [typist2 (create-counts 28 1)]) ;def t2
      (count-conditionals! file-t1-correct1
                           file-t1-corrupt1
                           typist1) ;instantiate t1
      (count-conditionals! file-t1-correct2
                           file-t1-corrupt2
                           typist1)
      (count-conditionals! file-t2-correct1
                           file-t2-corrupt1
                           typist2) ;instantiate t2
      (count-conditionals! file-t2-correct2
                           file-t2-corrupt2
                           typist2)
      (normalize-counts! typist1)
      (normalize-counts! typist2)
      (evidence test-correct test-corrupt 2 2 typist1 typist2))))


;;;;;;;;;;;;;;;;;;;;;;;;TEST 2 START;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test with 2 letters each
;; Calculating all possible permutations for testing with two letters


;Letters corresponding to each typist
;; Typist 1 : 1, 8, 16
;; Typist 2: 4, 9, 18

(display "TESTS WITH 2 LETTERS EACH \n")
(display "Typist1: 1, 8, Typist2: 4, 9 \n")
(display "Test on 16: Typist1 \n")
(test2 correct1 corrupt1
       correct8 corrupt8
       correct4 corrupt4
       correct9 corrupt9
       correct16 corrupt16)
(display "Test on 18: Typist2 \n")
(test2 correct1 corrupt1
       correct8 corrupt8
       correct4 corrupt4
       correct9 corrupt9
       correct18 corrupt18)
(newline)

(display "Typist1: 1, 16, Typist2: 4, 9 \n")
(display "Test on 8: Typist1 \n")
(test2 correct1 corrupt1
       correct16 corrupt16
       correct4 corrupt4
       correct9 corrupt9
       correct8 corrupt8)
(display "Test on 18: Typist2 \n")
(test2 correct1 corrupt1
       correct16 corrupt16
       correct4 corrupt4
       correct9 corrupt9
       correct18 corrupt18)
(newline)

(display "Typist1: 8, 16, Typist2: 4, 9 \n")
(display "Test on 1: Typist1 \n")
(test2 correct8 corrupt8
       correct16 corrupt16
       correct4 corrupt4
       correct9 corrupt9
       correct1 corrupt1)
(display "Test on 18: Typist2 \n")
(test2 correct8 corrupt8
       correct16 corrupt16
       correct4 corrupt4
       correct9 corrupt9
       correct18 corrupt18)

(newline)
(newline)

;; Letters corresponding to each typist
;; Typist 1 : 1, 8, 16
;; Typist 2: 4, 9, 18
(display "Typist1: 1, 8, Typist2: 9, 18 \n")
(display "Test on 16: Typist1 \n")
(test2 correct1 corrupt1
       correct8 corrupt8
       correct9 corrupt9
       correct18 corrupt18
       correct16 corrupt16)
(display "Test on 4: Typist2 \n")
(test2 correct1 corrupt1
       correct8 corrupt8
       correct9 corrupt9
       correct18 corrupt18
       correct4 corrupt4)
(newline)

(display "Typist1: 1, 16, Typist2: 9, 18 \n")
(display "Test on 8: Typist1 \n")
(test2 correct1 corrupt1
       correct16 corrupt16
       correct9 corrupt9
       correct18 corrupt18
       correct8 corrupt8)
(display "Test on 4: Typist2 \n")
(test2 correct1 corrupt1
       correct16 corrupt16
       correct9 corrupt9
       correct18 corrupt18
       correct4 corrupt4)
(newline)

(display "Typist1: 8, 16, Typist2: 9, 18 \n")
(display "Test on 1: Typist1 \n")
(test2 correct8 corrupt8
       correct16 corrupt16
       correct9 corrupt9
       correct18 corrupt18
       correct1 corrupt1)
(display "Test on 4: Typist2 \n")
(test2 correct8 corrupt8
       correct16 corrupt16
       correct9 corrupt9
       correct18 corrupt18
       correct4 corrupt4)
(newline)

;; Letters corresponding to each typist
;; Typist 1 : 1, 8, 16
;; Typist 2: 4, 9, 18
(display "Typist1: 1, 8, Typist2: 4, 18 \n")
(display "Test on 16: Typist1 \n")
(test2 correct1 corrupt1
       correct8 corrupt8
       correct4 corrupt4
       correct18 corrupt18
       correct16 corrupt16)
(display "Test on 9: Typist2 \n")
(test2 correct1 corrupt1
       correct8 corrupt8
       correct4 corrupt4
       correct18 corrupt18
       correct9 corrupt9)
(newline)

(display "Typist1: 1, 16, Typist2: 4, 18 \n")
(display "Test on 8: Typist1 \n")
(test2 correct1 corrupt1
       correct16 corrupt16
       correct4 corrupt4
       correct18 corrupt18
       correct8 corrupt8)
(display "Test on 9: Typist2 \n")
(test2 correct1 corrupt1
       correct16 corrupt16
       correct4 corrupt4
       correct18 corrupt18
       correct9 corrupt9)
(newline)

(display "Typist1: 8, 16, Typist2: 4, 18 \n")
(display "Test on 1: Typist1 \n")
(test2 correct8 corrupt8
       correct16 corrupt16
       correct4 corrupt4
       correct18 corrupt18
       correct1 corrupt1)
(display "Test on 9: Typist2 \n")
(test2 correct8 corrupt8
       correct16 corrupt16
       correct4 corrupt4
       correct18 corrupt18
       correct9 corrupt9)
(newline)

;; Letters corresponding to each typist
; Unlabeled: 10, 15, 22, 11, 3, 7
;; Typist 1 : 1, 8, 16
;; Typist 2: 4, 9, 18
(define correct10
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/10.txt"
  "original/10.txt"))
(define correct15
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/15.txt"
  "original/15.txt"))
(define correct22
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/22.txt"
  "original/22.txt"))
(define correct11
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/11.txt"
  "original/11.txt"))
(define correct3
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/3.txt"
  "original/3.txt"))
(define correct7
  (if paths
  "/home/weinman/courses/CSC261/data/mills/original/7.txt"
  "original/7.txt"))

(define corrupt10
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/unknown/10.txt"
  "corrupted/unknown/10.txt"))
(define corrupt15
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/unknown/15.txt"
  "corrupted/unknown/15.txt"))
(define corrupt22
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/unknown/22.txt"
  "corrupted/unknown/22.txt"))
(define corrupt11
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/unknown/11.txt"
  "corrupted/unknown/11.txt"))
(define corrupt3
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/unknown/3.txt"
  "corrupted/unknown/3.txt"))
(define corrupt7
  (if paths
  "/home/weinman/courses/CSC261/data/mills/corrupted/unknown/7.txt"
  "corrupted/unknown/7.txt"))

;;; Procedure:
;;;   test3
;;; Parameters:
;;;   file-t1-correct, a filename
;;;   file-t2-correct, a filename
;;;   file-t1-corrupt, a filename
;;;   file-t2-corrupt, a filename
;;;   test-correct, a filename
;;;   test-corrupt, a filename
;;; Purpose:
;;;   Helper function to reduce repeated code for single letter training
;;; Produces:
;;;;  e, a real number
;;; Preconditions:
;;;   filenames are valid file names for correct and corrupt files
;;; Postconditions:
;;;   -inf < e < +inf
(define test3
  (lambda (correct corrupt)
    (let ([typist1 (create-counts 28 1)] ;def t1
          [typist2 (create-counts 28 1)]) ;def t2
      (count-conditionals! correct1 corrupt1 typist1) ;instantiate t1
      (count-conditionals! correct8 corrupt8 typist1)
      (count-conditionals! correct16 corrupt16 typist1)
      (count-conditionals! correct4 corrupt4 typist2) ;instantiate t2
      (count-conditionals! correct9 corrupt9 typist2)
      (count-conditionals! correct18 corrupt18 typist2)
      (normalize-counts! typist1)
      (normalize-counts! typist2)
      (evidence correct corrupt 3 3 typist1 typist2))))


;; TESTS WITH ALL TRAINED ON UNKNOWNS
(display "TEST WITH ALL THREE LETTERS TRAINED \n")
(display "Test with letter10 \n")
(test3 correct10 corrupt10)
(display "Test with letter15 \n")
(test3 correct15 corrupt15)
(display "Test with letter22 \n")
(test3 correct22 corrupt22)
(display "Test with letter11 \n")
(test3 correct11 corrupt11)
(display "Test with letter3 \n")
(test3 correct3 corrupt3)
(display "Test with letter7 \n")
(test3 correct7 corrupt7)
