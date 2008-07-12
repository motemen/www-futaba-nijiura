(define-module www.futaba
  (use srfi-1)
  (use srfi-2)
  (use srfi-19)
  (use rfc.uri)
  (use rfc.http)
  (use gauche.charconv)
  (export
    futaba-parse-index
    futaba-parse-thread))
(select-module www.futaba)

(define *regexp-index-threads*
  #/<\/a><input type=checkbox name=\d+ value=delete>(?:<[^>]+>)?(?<date>[^ ]+)(?:<[^>]+>)? No\.(?<no>\d+) <small>[^<]+<\/small>\[<a href=(?<path>[^>]+)>返信<\/a>\]\n<blockquote>(?<body>.*?) <\/blockquote>/)
(define *regexp-thread-head*
  #/<\/a><input type=checkbox name=\d+ value=delete>(?<date>[^ ]+) No\.(?<no>\d+) <small>[^<]+<\/small>\n<blockquote>(?<body>.*?) <\/blockquote>/)
(define *regexp-thread-response*
  #/^<input type=checkbox.*?>(?<date>.*?) No.(?<no>\d+) <blockquote>(?<body>.*?) <\/blockquote>/)

(define (futaba-parse-index html)
  (let loop ((html html) (thread-heads '()))
    (or (and-let* ((m (*regexp-index-threads* html)))
          (loop (m 'after) (cons (match->response-info m) thread-heads)))
        (reverse! thread-heads))))

(define (futaba-parse-thread html)
  (cons (futaba-parse-thread-head html) (futaba-parse-thread-response html)))

(define (futaba-parse-thread-head html)
  (cond ((*regexp-thread-head* html) => match->response-info)
        (else #f)))
  
(define (futaba-parse-thread-response html)
  (filter-map
    (lambda (line)
      (cond ((*regexp-thread-response* line) => match->response-info)
            (else #f)))
    (string-split html #/[\r\n]/)))

(define (match->response-info m)
  `((date . ,(string->date (m 'date) "~y~m~d~H~M~S"))
    (no   . ,(m 'no))
    (body . ,(html-string->plain (m 'body)))
    ,@(guard (e (else '())) `((path . ,(m 'path))))))

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
