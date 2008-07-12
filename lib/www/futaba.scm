(define-module www.futaba
  (use srfi-1)
  (use srfi-2)
  (use srfi-19)
  (use rfc.uri)
  (use rfc.http)
  (use sxml.sxpath)
  (use gauche.charconv)
  (export
    futaba-parse-index
    futaba-parse-thread))
(select-module www.futaba)

(define (futaba-parse-index html)
  (let1 thread-heads '()
    (regexp-replace-all
      #/<\/a><input type=checkbox name=\d+ value=delete>(?:<[^>]+>)?([^ ]+)(?:<[^>]+>)? No\.(\d+) <small>[^<]+<\/small>\[<a href=([^>]+)>返信<\/a>\]\n<blockquote>(.*?) <\/blockquote>/
      html
      (lambda (m)
        (push!
          thread-heads
          (let ((date (string->date (m 1) "~y~m~d~H~M~S"))
                (no   (m 2))
                (path (m 3))
                (body (html-string->plain (m 4))))
            `((body   . ,body)
              (date   . ,date)
              (number . ,no)
              (path   . ,path))))))
    (reverse! thread-heads)))

(define (futaba-parse-thread html)
  (cons (futaba-parse-thread-head html) (futaba-parse-thread-response html)))

(define (futaba-parse-thread-head html)
  (and-let* ((m (#/<\/a><input type=checkbox name=\d+ value=delete>([^ ]+) No\.(\d+) <small>[^<]+<\/small>\n<blockquote>(.*?) <\/blockquote>/ html))
             (date (string->date (m 1) "~y~m~d~H~M~S"))
             (no   (m 2))
             (body (html-string->plain (m 3))))
    `((body   . ,body)
      (date   . ,date)
      (number . ,no))
  ))
  
(define (futaba-parse-thread-response html)
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
                       #/&lt\;/   "<"
                       #/&gt\;/   ">"
                       #/&quot\;/ "\""
                       #/&amp\;/  "&"))

(provide "www/futaba")
