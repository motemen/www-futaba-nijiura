(define-module www.futaba
  (use srfi-1)
  (use srfi-2)
  (use srfi-19)
  (use rfc.uri)
  (use rfc.http)
  (use sxml.sxpath)
  (use gauche.charconv)
  (export
    parse-thread))
(select-module www.futaba)

;(define (list-unique lis . maybe-memq)
;  (let1 memq (get-optional maybe-memq memq)
;    (reverse (fold (lambda (x xs) (if (memq x xs) xs (cons x xs))) '() lis))))

; (sxpath "//small/following-sibling::blockquote")
;(define *thread-body-sxpath-query* (compose list-unique (sxpath "//a[.=\"返信\"]/following-sibling::blockquote")))
;(define *thread-meta-sxpath-query* (compose list-unique (sxpath "//a[.=\"返信\"]/preceding-sibling::text()[contains(\"/\",.)]")))
;(define *thread-body-sxpath-query* (compose list-unique (sxpath "//small/following-sibling::blockquote")))
;(define *thread-meta-sxpath-query* (compose list-unique (sxpath "//small/preceding-sibling::text()[contains('/', .)]")))
;(define *index-link-query*         (sxpath "//a[.='返信']/@href"))
;(define *response-sxpath-query*    (sxpath '(// (td (@ (equal? (bgcolor "#F0E0D6")))))))

(define (parse-thread html)
  (cons (parse-thread-head html) (parse-thread-response html)))

(define (parse-thread-head html)
  (and-let* ((m (#/<\/a><input type=checkbox name=\d+ value=delete>([^ ]+) No\.(\d+) <small>[^<]+<\/small>\n<blockquote>(.*?) <\/blockquote>/ html))
             (date (string->date (m 1) "~y~m~d~H~M~S"))
             (no   (m 2))
             (body (html-string->plain (m 3))))
    `((body   . ,body)
      (date   . ,date)
      (number . ,no))
  ))
  
(define (parse-thread-response html)
  (filter-map
    (lambda (line)
      (and-let* ((m (#/^<input type=checkbox.*?>(.*?) No.(\d+) <blockquote>(.*?) <\/blockquote>/ line))
                 (date (string->date (m 1) "~y~m~d~H~M~S"))
                 (no   (m 2))
                 (body (html-string->plain (m 3))))
        `((body   . ,body)
          (date   . ,date)
          (number . ,no))
        ))
    (string-split html #/[\r\n]/)))

;(define (blockquote->response bq)
;  (let ((meta-info-string (string-join ((sxpath '(// *text*)) bq)))
;        (body             (shtml-tree->string ((sxpath '(// blockquote)) bq))))
;    (let ((date   (string->date meta-info-string "~y~m~d~H~M~S"))
;          (number ((#/No\.(\d+)/ meta-info-string) 1)))
;      `((body   . ,body)
;        (date   . ,date)
;        (number . ,number)))))

;(define (futaba-thread-url->list url)
;  (let1 shtml (url->shtml url)
;    (let ((start-body (shtml-tree->string (car (*thread-body-sxpath-query* shtml))))
;          (start-meta (car (*thread-meta-sxpath-query* shtml)))
;          (responses (*response-sxpath-query* shtml)))
;      (let ((date   (string->date start-meta "~y~m~d~H~M~S"))
;            (number ((#/No\.(\d+)/ start-meta) 1)))
;        (cons
;          `((body   . ,start-body)
;            (date   . ,date)
;            (number . ,number))
;          (map blockquote->response responses)))
;      )))

;;; Utilities
(define (url->string url)
  (receive (#f #f host #f path #f #f) (uri-parse url)
    (receive (#f #f html) (http-get host path)
      (ces-convert html "*JP"))))

(define (html-string->plain html-string)
  (html-unescape-string
    (regexp-replace-all* html-string
                         #/<br>/ "\n"
                         #/<.*?>/ "")))

(define (html-unescape-string string)
  (regexp-replace-all* string
                       #/&lt;/   "<"
                       #/&gt;/   ">"
                       #/&quot;/ "\""
                       #/&amp;/  "&"))

;(define (shtml-tree->string lis)
;  (if (and (list? lis) (not (equal? (car lis) '@)))
;    (string-join
;      (map
;        (lambda (x)
;          (cond ((string? x) x)
;                ((list? x)   (shtml-tree->string x))
;                (else "")))
;        lis))
;    (if (string? lis) lis "")))

(provide "www/futaba")
