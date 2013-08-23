;; vCard processing library
;; Corresponds to vCard specification 4.0, RFC 6350
;; (https://tools.ietf.org/rfc/rfc6350.txt)
(define-module (vcard)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)

  #:export (make-vcard
            vcard-parameters
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
                            (display (vcard->string record) port)))

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
                 ":" (serialize-vcard-value (or (and type (string->symbol (string-downcase type))) 'text) (car param-values))))
              (string-append ":" (serialize-vcard-value 'text param-values)))
          "\n\r")))
     (vcard-parameters vc))
    "")
   "END:VCARD\n\r"))

(define (vcard-properties->string properties)
  (string-join
   (map
    (lambda (val)
      (string-append
       (string-upcase (symbol->string (car val))) "="
       (cdr val)))
    properties)
   ";"))

(define (serialize-vcard-value type val)
  (case type
    ;; TODO
    (else
     (escape-vcard-value
      (or
       (and (string? val) val)
       (format #f "~s" val))))))

(define (escape-vcard-value v)
  v)
