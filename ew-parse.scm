(define message-grammar
'(
; Terminal symbols
*anchor*
*err*

tag-mailbox
tag-mailbox*
tag-mailbox+
tag-address*
tag-address+
tag-phrase*
tag-phrase-msg-id*

lt
gt
at
comma
semicolon
colon

dot         ; for STD11
atom        ; for STD11

slash       ; for MIME
question    ; for MIME
equal       ; for MIME
token       ; for MIME

us-texts    ; for UNSTRUCTURED
us-wsp      ; for UNSTRUCTURED
us-fold     ; for UNSTRUCTURED

wsp
fold

qs-begin
qs-end
qs-texts
qs-wsp
qs-fold
qs-qfold
qs-qpair

dl-begin
dl-end
dl-texts
dl-wsp
dl-fold
dl-qfold
dl-qpair

cm-begin
cm-end
cm-nested-begin
cm-nested-end
cm-texts
cm-wsp
cm-fold
cm-qfold
cm-qpair

; Productions
(start (tag-mailbox gap mailbox)                  : ()
  (tag-mailbox* gap mailbox*)                     : ()
  (tag-mailbox+ gap mailbox+)                     : ()
  (tag-address* gap address*)                     : ()
  (tag-address+ gap address+)                     : ()
  (tag-phrase* gap phrase*)                       : ()
  (tag-phrase-msg-id* gap phrase-msg-id*)         : ())
(address* ()                                      : ()
  (address+)                                      : ())
(address+ (address)                               : ()
  (address+ comma-gap address)                    : ())
(address (mailbox)                                : ()
  (group)                                         : ())
(addr-spec (local-part at-gap domain)             : ())
(date (atom-gap atom-gap atom-gap)                : ())
(date-time (atom-gap comma-gap date time)         : ()
  (date time)                                     : ())
(domain (sub-domain)                              : ()
  (domain dot-gap sub-domain)                     : ())
(domain-ref (atom-gap)                            : ())
(group (phrase colon-gap mailbox* semicolon-gap)  : ())
(hour (atom-gap colon-gap atom-gap)               : ()
  (atom-gap colon-gap atom-gap colon-gap atom-gap): ())
(local-part (word)                                : ()
  (local-part dot-gap word)                       : ())
(mailbox (addr-spec)                              : ()
  (phrase route-addr)                             : ()
  (route-addr)                                    : ())
(mailbox* ()                                      : ()
  (mailbox+)                                      : ())
(mailbox+ (mailbox)                               : ()
  (mailbox+ comma-gap mailbox)                    : ())
(month (atom-gap)                                 : ())
(msg-id (lt-gap addr-spec gt-gap)                 : ())
(phrase (phrase-c)                                : (ew-mark-phrase $1 $look))
(phrase-c (word)                                  : $1
  (phrase-c word)                                 : $1)
(route (at-domain+ colon-gap)                     : ())
(at-domain+ (at-gap domain)                       : ()
  (at-domain+ comma-gap at-gap domain)            : ())
(route-addr (lt-gap route/ addr-spec gt-gap)      : ())
(route/ ()                                        : ()
  (route)                                         : ())
(sub-domain (domain-ref)                          : ()
  (domain-literal-gap)                            : ())
(time (hour zone)                                 : ())
(word (atom-gap)                                  : $1
  (quoted-string-gap)                             : $1)
(zone (atom-gap)                                  : ())
(phrase/ ()                                       : ()
  (phrase)                                        : ())
(phrase* ()                                       : ()
  (phrase+)                                       : ())
(phrase+ (phrase)                                 : ()
  (phrase+ comma-gap phrase)                      : ())
(phrase-msg-id* (phrase/)                         : ()
  (phrase-msg-id* msg-id phrase/)                 : ())
(word1or2 (word)                                  : ()
  (word comma-gap word)                           : ())
(gap ()                                           : ()
  (gap wsp)                                       : ()
  (gap fold)                                      : ()
  (gap comment)                                   : ())
(lt-gap (lt gap)                                  : ())
(gt-gap (gt gap)                                  : ())
(at-gap (at gap)                                  : ())
(comma-gap (comma gap)                            : ())
(semicolon-gap (semicolon gap)                    : ())
(colon-gap (colon gap)                            : ())
(dot-gap (dot gap)                                : ())
(quoted-string-gap (quoted-string gap)            : $1)
(domain-literal-gap (domain-literal gap)          : ())
(atom-gap (atom gap)                              : $1)
(quoted-string (qs-begin qs qs-end)               : $1)
(qs ()                                            : ()
  (qs qs-texts)                                   : ()
  (qs qs-wsp)                                     : ()
  (qs qs-fold)                                    : ()
  (qs qs-qfold)                                   : ()
  (qs qs-qpair)                                   : ())
(domain-literal (dl-begin dl dl-end)              : ())
(dl ()                                            : ()
  (dl dl-texts)                                   : ()
  (dl dl-wsp)                                     : ()
  (dl dl-fold)                                    : ()
  (dl dl-qfold)                                   : ()
  (dl dl-qpair)                                   : ())
(comment (cm-begin cm cm-end)                     : ())
(cm ()                                            : ()
  (cm cm-nested-begin)                            : ()
  (cm cm-nested-end)                              : ()
  (cm cm-texts)                                   : ()
  (cm cm-wsp)                                     : ()
  (cm cm-fold)                                    : ()
  (cm cm-qfold)                                   : ()
  (cm cm-qpair)                                   : ())

))

(gen-lalr1 message-grammar "ew-parse.el"
"(provide 'ew-parse)
(require 'ew-data)
"
"(put 'ew:cm-texts 'decode 'ew-decode-comment)
(put 'ew:cm-wsp 'decode 'ew-decode-comment)
(put 'ew:cm-fold 'decode 'ew-decode-comment)
(put 'ew:cm-qfold 'decode 'ew-decode-comment)
(put 'ew:cm-qpair 'decode 'ew-decode-comment)
(put 'ew:us-texts 'decode 'ew-decode-unstructured)
(put 'ew:us-wsp 'decode 'ew-decode-unstructured)
(put 'ew:us-fold 'decode 'ew-decode-unstructured)
"
'ew)

(print-states)
