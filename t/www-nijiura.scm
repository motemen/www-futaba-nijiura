#!/usr/local/bin/gosh
(use util.list)
(use gauche.charconv)
(use gauche.test)

(define (load-assets-html filename)
  (call-with-input-file
    #`"t/html/,|filename|.html"
    port->string
    :encoding "cp932"))
  
(test-start "www.nijiura")
(use www.nijiura)

(test-section "module")
(test-module 'www.nijiura)

(test-section "index")
(define index (nijiura-parse-index (load-assets-html "img-index")))

(test* "index length"
       10
       (length index))

(test* "index head body"
       "腐女子の思考が理解できない"
       (assoc-ref (car index) 'body))

(test* "index #2 path"
       "res/36234303.htm"
       (assoc-ref (list-ref index 2) 'path))

(test-section "thread")
(define thread (nijiura-parse-thread (load-assets-html "img-thread")))

(test* "thread length"
       50
       (length thread))

(test* "thread head body"
       "統一感が全くないデザインもいいな！"
       (assoc-ref (car thread) 'body))

(test* "thread #10 body"
       ">統一感が全くないデザインもいいな！\nしかも姚天君以外は\n初登場時のシルエットと姿が全然違う！"
       (assoc-ref (list-ref thread 10) 'body))

(test-end)
