(define-module www.futaba
  (use srfi-1)
  (use srfi-2)
  (use srfi-19)
  (use rfc.uri)
  (use rfc.http)
  (use gauche.charconv)
  (export
    futaba-url-type
    futaba-url->list
    futaba-parse-index
    futaba-parse-thread))
(select-module www.futaba)

(define *regexp-index-threads*
  #/<\/a><input type=checkbox name=\d+ value=delete>(?:<[^>]+>)?(?<date>[^ ]+)(?:<[^>]+>)? No\.(?<no>\d+) <small>[^<]+<\/small>\[<a href=(?<path>[^>]+)>返信<\/a>\]\n<blockquote>(?<body>.*?) <\/blockquote>/)
(define *regexp-thread-head*
  #/<\/a><input type=checkbox name=\d+ value=delete>(?<date>[^ ]+) No\.(?<no>\d+) <small>[^<]+<\/small>\n<blockquote>(?<body>.*?) <\/blockquote>/)
(define *regexp-thread-response*
  #/^<input type=checkbox.*?>(?<date>.*?) No.(?<no>\d+) <blockquote>(?<body>.*?) <\/blockquote>/)

(define (futaba-url-type url)
  (rxmatch-cond
    ((#/^http:\/\/(\w+)\.2chan.net\/b\/$/ url)
       (#f server)
     `(index ,server))
    ((#/^http:\/\/(\w+)\.2chan.net\/b\/res\/(\d+).htm$/ url)
       (#f server no)
     `(thread ,server ,no))
    (else
      #f)))

(define (futaba-url->list url)
  (or
    (and-let* ((url-type (futaba-url-type url))
               (url-type (car url-type)))
      (receive (content status) (url->string url)
        (values
          (cond
            ((eq? url-type 'index)
             (futaba-parse-index content))
            ((eq? url-type 'thread)
             (futaba-parse-thread content))
            (else #f))
          url-type
          status)))
    (values #f #f #f)))

(define (futaba-parse-index html)
  (let loop ((html html) (thread-heads '()))
    (or (and-let* ((m (*regexp-index-threads* html)))
          (loop (m 'after) (cons (match->response-info m) thread-heads)))
        (reverse! thread-heads))))

(define (futaba-parse-thread html)
  (and-let* ((head (futaba-parse-thread-head html))
             (tail (futaba-parse-thread-response html)))
    (cons head tail)))

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
    (receive (status #f content) (http-get host path)
      (values
        (ces-convert content "*JP")
        status))))

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
