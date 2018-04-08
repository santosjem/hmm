(include "charmodel.scm")
(include "hmm.scm")

(define o1 "original/1.txt")
(define o3 "original/3.txt")
(define o4 "original/4.txt")
(define o7 "original/7.txt")
(define o8 "original/8.txt")
(define o9 "original/9.txt")
(define o10 "original/10.txt")
(define o11 "original/11.txt")
(define o15 "original/15.txt")
(define o16 "original/16.txt")
(define o18 "original/18.txt")
(define o22 "original/22.txt")

(define t1-1 "corrupted/typist1/1.txt")
(define t1-2 "corrupted/typist1/2.txt")
(define t1-8 "corrupted/typist1/8.txt")
(define t1-14 "corrupted/typist1/14.txt")
(define t1-16 "corrupted/typist1/16.txt")
(define t1-17 "corrupted/typist1/17.txt")

(define t2-4 "corrupted/typist2/4.txt")
(define t2-9 "corrupted/typist2/9.txt")
(define t2-12 "corrupted/typist2/12.txt")
(define t2-13 "corrupted/typist2/13.txt")
(define t2-18 "corrupted/typist2/18.txt")
(define t2-19 "corrupted/typist2/19.txt")

(define u3 "corrupted/unknown/3.txt")
(define u5 "corrupted/unknown/5.txt")
(define u6 "corrupted/unknown/6.txt")
(define u7 "corrupted/unknown/7.txt")
(define u10 "corrupted/unknown/10.txt")
(define u11 "corrupted/unknown/11.txt")
(define u15 "corrupted/unknown/15.txt")
(define u20 "corrupted/unknown/20.txt")
(define u21 "corrupted/unknown/21.txt")
(define u22 "corrupted/unknown/22.txt")
(define u23 "corrupted/unknown/23.txt")
(define u24 "corrupted/unknown/24.txt")

;(define counts (create-counts 28 1))
;(count-transitions! o4 counts)
;(count-transitions! o16 counts)

;(define typist (create-counts 28 1))
;(count-transitions! t2-19 typist)

;(define marg (marginal-counts counts))

;(normalize-marginal-counts! marg)

;(log-evidence t2-4 typist2 counts marg)

(define test
  (lambda
      (corrupt
       typist1-original-file
       typist2-original-file
       typist-corrupt-file)
    (let ([counts (create-counts 28 1)]
          [typist-counts (create-counts 28 1)]) ;def t2
      (count-transitions! typist1-original-file counts)
      (count-transitions! typist2-original-file counts)
      (count-transitions! typist-corrupt-file typist-counts)
      (let ([marg (marginal-counts counts)])
        (normalize-marginal-counts! marg)
        (normalize-counts! typist-counts)
        (log-evidence corrupt typist-counts counts marg)))))

;(test1 t2-4 o4 o16 t1-1)

(define counts (create-counts 28 1))
(count-transitions! o4 counts)
(count-transitions! o16 counts)

(define typist (create-counts 28 1))
(count-transitions! t2-19 typist)

(define marg (marginal-counts counts))
(normalize-counts! counts)
(normalize-marginal-counts! marg)

(log-evidence t2-4 typist counts marg)

