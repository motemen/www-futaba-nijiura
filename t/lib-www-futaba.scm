#!/usr/local/bin/gosh
(use file.util)
(use util.list)
(use gauche.charconv)
(use gauche.test)

(use www.futaba)

(define (load-assets-html filename)
  (call-with-input-file
    #`"t/html/,|filename|.html"
    port->string
    :encoding "cp932"))
  
(test-section "module")
(test-module 'www.futaba)

(test-section "index")
(define index (futaba-parse-index (load-assets-html "img-index")))
(test* "index length"
       10
       (length index))

(test* "index head"
       "腐女子の思考が理解できない"
       (assoc-ref (car index) 'body))

(test-section "thread")
(define thread (futaba-parse-thread (load-assets-html "img-thread")))

(test* "thread length"
       50
       (length thread))

(test* "thread head body"
       "統一感が全くないデザインもいいな！"
       (assoc-ref (car thread) 'body))

(test* "thread #10 body"
       ">統一感が全くないデザインもいいな！\nしかも姚天君以外は\n初登場時のシルエットと姿が全然違う！"
       (assoc-ref (list-ref thread 10) 'body))
