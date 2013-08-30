;; vCard processing library
;; Corresponds to vCard specification 4.0, RFC 6350
;; (https://tools.ietf.org/rfc/rfc6350.txt)
(define-module (vcard)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (web uri)
  #:use-module (system base lalr)
  #:use-module (ice-9 rdelim) ;; provides "read-line"

  #:export (make-vcard
            vcard-parameters
            parse-vcard
            vcard->string))

;; vCard record. Constructing it requires only one argument:
;; parameters. Parameters like:
;;
;; TITLE;ALTID=1;LANGUAGE=en:Boss
;; PID:12345
;;
;; are presented as follows:
;;
;; (
;;   (title "Boss" ((altid . 1) (language . "en")))
;;   (pid . "12345")
;; )
(define-record-type vcard
  (make-vcard parameters)
  vcard?
  (parameters vcard-parameters))

(set-record-type-printer! vcard
                          (lambda (record port)
                            (display "#<vcard ...>\n" port)))

(define (vcard->string vc)
  (string-append
   "BEGIN:VCARD\n\r"
   "VERSION:4.0\n\r"
   (string-join
    (map
     (lambda (param)
       (let ((param-name (car param)) (param-values (cdr param)))
         (string-append
          (string-upcase (symbol->string param-name))
          (if (pair? param-values)
              (let ((type (assoc-ref (cadr param-values) 'value)))
                (string-append
                 ";" (vcard-properties->string (cadr param-values))
                 ":" (serialize-vcard-value (or type "text") (car param-values))))
              (string-append ":" (serialize-vcard-value "text" param-values)))
          "\n\r")))
     (vcard-parameters vc))
    "")
   "END:VCARD\n\r"))

(define (vcard-properties->string properties)
  (string-join
   (map
    (lambda (attr)
      (let ((name (car attr)) (value (cdr attr)))
        (string-append
         (string-upcase (symbol->string name)) "="
         (format #f "~a" value))))
    properties)
   ";"))

(define (serialize-vcard-value type val)
  (cond
    ;; TODO: value lists (e.g. (10, 20, 30) => "10,20,30")
    ((equal? type "uri")
     (uri->string val))
    ((equal? type "date")
     (if (date? val)
         (date->string val "~Y~m~d")
         ;; TODO: date as an incomplete triple?
         (write val)))
    ((equal? type "time")
     (cond ((date? val)
            (date->string val "~H~M~S~z"))
           ((time? val)
            (error "Handling of time objects is not implemented yet. Try date."))
           ;; TODO: incomplete time/date (hours or minutes only, etc.)
           (else
            (write val))))
    ((equal? type "date-time")
     (if (date? val)
         (date->string val "~Y~m~dT~H~M~S~z")
         ;; TODO: incomplete combinations of date and time
         (write val)))
    ((equal? type "date-and-or-time")
     (if (date? val)
         (date->string val "~Y~m~dT~H~M~S")
         ;; TODO: incomplete combinations of date and time
         (write val)))
    ((equal? type "timestamp")
     (date->string val "~Y~m~dT~H~M~S~z"))
    ((equal? type "boolean")
     (or (and val "true") "false"))
    ((equal? type "integer")
     (number->string val))
    ((equal? type "float")
     (number->string val))
    ((equal? type "utc-offset")
     ;; TODO
     (error "Not implemented."))
    ((equal? type "language-tag")
     val)
    (else
     (escape-vcard-value
      (format #f "~a" val)))))

(define (escape-vcard-value v)
  ;; TODO
  v)

(define (typecast-vcard-value v type)
  ;; TODO
  v)

(define (parse-vcard port)
  (vcard-parser
   (vcard-lexer port)
   vcard-parse-error))

(define vcard-parser
  (lalr-parser
   (TEXT PDELIM ADELIM EQL)
   (VCARD (VCARD PARAM) : (cons $2 $1)
          (PARAM) : (list $1)
          () : 0)
   (PARAM (TEXT PDELIM TEXT) : (cons (symbolize-vname $1) (typecast-vcard-value $3 #f))
          (TEXT ATTRS PDELIM TEXT) : (list (symbolize-vname $1) $4 (typecast-vcard-value $2 #f)))
   (ATTRS (ATTRS ATTR) : (cons $2 $1)
          (ATTR) : (list $1)
          () : 0)
   (ATTR (ADELIM TEXT EQL TEXT) : (cons (symbolize-vname $2) (string-downcase $4)))))

(define (vcard-lexer port)
  (lambda ()
    ;; TODO: do a proper (char-based) lexer
    (let ((token (read-line port)))
      (if (eof-object? token)
          (make-lexical-token '*eoi* #f #f)
          (cond
           ((equal? token ":")
            (make-lexical-token 'PDELIM #f token))
           ((equal? token ";")
            (make-lexical-token 'ADELIM #f token))
           ((equal? token "=")
            (make-lexical-token 'EQL #f token))
           (else
            (make-lexical-token 'TEXT #f token)))))))

(define* (vcard-parse-error msg #:optional tok)
  (display (string-append "[PARSER] " msg ": " (format #f "~s" tok) "\n")))

(define (symbolize-vname n)
  (string->symbol (string-downcase n)))
