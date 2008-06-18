(define-module www.futaba
  (use srfi-1)
  (use rfc.uri)
  (use rfc.http)
  (use sxml.sxpath)
  (use gauche.charconv)
  (export
    futaba-thread->list))
(select-module www.futaba)

(load "htmlprag")

(define (futaba-thread->list url)
  (receive (#f #f host #f path #f #f) (uri-parse url)
    (receive (#f #f content) (http-get host path)
      (let1 shtml (html->shtml (ces-convert content "*JP"))
        (map
          (lambda (e)
            (string-join
              (map (lambda (x) (if (equal? x '(br)) "\n" (if (list? x) (last x) x))) (cdr e))))
          ((sxpath '(// blockquote)) shtml))
        ))))

(provide "www/futaba")
