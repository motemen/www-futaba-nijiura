(define-module www.futaba
  (use srfi-1)
  (use srfi-19)
  (use rfc.uri)
  (use rfc.http)
  (use sxml.sxpath)
  (use gauche.charconv)
  (export
    futaba-thread-url->list))
(select-module www.futaba)
(export-all)

(load "htmlprag")

; (sxpath "//small/following-sibling::blockquote")
;(define *thread-body-sxpath-query* (compose list-unique (sxpath "//a[.=\"返信\"]/following-sibling::blockquote")))
;(define *thread-meta-sxpath-query* (compose list-unique (sxpath "//a[.=\"返信\"]/preceding-sibling::text()[contains(\"/\",.)]")))
(define *thread-body-sxpath-query* (compose list-unique (sxpath "//small/following-sibling::blockquote")))
(define *thread-meta-sxpath-query* (compose list-unique (sxpath "//small/preceding-sibling::text()[contains('/', .)]")))
(define *index-link-query*         (sxpath "//a[.='返信']/@href"))
(define *response-sxpath-query*    (sxpath '(// (td (@ (equal? (bgcolor "#F0E0D6")))))))
(export-all)

(define (blockquote->response bq)
  (let ((meta-info-string (string-join ((sxpath '(*text*)) bq)))
        (body             (shtml-tree->string ((sxpath '(// blockquote)) bq))))
    (let ((date   (string->date meta-info-string "~y~m~d~H~M~S"))
          (number ((#/No\.(\d+)/ meta-info-string) 1)))
      `((body   . ,body)
        (date   . ,date)
        (number . ,number)))))

(define (futaba-thread-url->list url)
  (let1 shtml (url->shtml url)
    (let ((start-body (shtml-tree->string (car (*thread-body-sxpath-query* shtml))))
          (start-meta (car (*thread-meta-sxpath-query* shtml)))
          (responses (*response-sxpath-query* shtml)))
      (let ((date   (string->date start-meta "~y~m~d~H~M~S"))
            (number ((#/No\.(\d+)/ start-meta) 1)))
        (cons
          `((body   . ,start-body)
            (date   . ,date)
            (number . ,number))
          (map blockquote->response responses)))
      )))

;;; Utilities
(define (url->shtml url)
  (receive (#f #f host #f path #f #f) (uri-parse url)
    (receive (#f #f content) (http-get host path)
      (html->shtml (ces-convert content "*JP")))))

(define (list-unique lis . maybe-memq)
  (let1 memq (get-optional maybe-memq memq)
    (reverse (fold (lambda (x xs) (if (memq x xs) xs (cons x xs))) '() lis))))

(define (shtml-tree->string lis)
  (if (and (list? lis) (not (equal? (car lis) '@)))
    (string-join
      (map
        (lambda (x)
          (cond ((string? x) x)
                ((list? x)   (shtml-tree->string x))
                (else "")))
        lis))
    (if (string? lis) lis "")))

(provide "www/futaba")
