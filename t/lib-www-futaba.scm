#!/usr/local/bin/gosh
(use file.util)
(use util.list)
(use gauche.charconv)
(use gauche.test)

(use www.futaba)

(test-module 'www.futaba)

(test-section "thread")
(define thread (parse-thread (call-with-input-file
                               "t/html/img-thread.html"
                               port->string
                               :encoding "cp932")))

(test* "parse-thread length"
       50
       (length thread))

(test* "parse-thread head body"
       "統一感が全くないデザインもいいな！"
       (assoc-ref (car thread) 'body))

(test* "parse-thread #10 body"
       ">統一感が全くないデザインもいいな！\nしかも姚天君以外は\n初登場時のシルエットと姿が全然違う！"
       (assoc-ref (list-ref thread 10) 'body))
