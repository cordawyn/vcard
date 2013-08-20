;; vCard processing library
;; Corresponds to vCard specification 4.0, RFC 6350

(define-module (vcard)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)

  #:export (make-vcard))

;; Parameters like:
;;
;; TITLE;ALTID=1;LANGUAGE=en:Boss
;; PID:12345
;;
;; are presented as follows:
;;
;; (
;;   ('title . ("Boss" . (('altid . 1) ('language . "en"))))
;;   ('pid . "12345")
;; )

(define-record-type vcard
  (make-vcard parameters)
  vcard?
  (parameters vcard-parameters))

(set-record-type-printer! vcard
                          (lambda (record port)
                            (display "BEGIN:VCARD\n\r" port)
                            (display "VERSION:4.0\n\r" port)
                            (display-vcard-parameters (vcard-parameters record) port)
                            (display "END:VCARD\n\r" port)))

(define (display-vcard-parameters parameters port)
  (let display-param ((head (car parameters)) (tail (cdr parameters)))
    (let ((param-name (car head)) (param-value (cdr head)))
      (display (string-upcase (symbol->string param-name)) port)
      (if (string? param-value)
          (display (string-append ":" param-value "\n\r") port)
          (begin (let ((param (cdr param-value)))
                   (display (string-append ";" (string-upcase (symbol->string (car param))) "=" (typecast-vcard-param (cdr param))) port)
                   (display (string-append ":" (car param-value) "\n\r") port)))))
    (unless (null? tail)
            (display-param (car tail) (cdr tail)))))

(define (typecast-vcard-param value)
  value)
