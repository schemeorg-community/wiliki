;;;
;;; wiliki/edit - handles edit, preview, and conflict page
;;;
;;;  Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.
;;;
;;;  $Id: edit.scm,v 1.14 2004/03/22 11:44:35 shirok Exp $
;;;

(select-module wiliki)

(use text.diff)
(use wiliki.settings-user-name)
(use wiliki.banned-content)

(define (edit-form preview? pagename content mtime logmsg donttouch)
  (define (buttons)
    (if preview?
        `((input (@ (type submit) (name preview) (value ,($$ "Preview"))))
          (input (@ (type submit) (name commit) (value ,($$ "Commit without preview")))))
        `((input (@ (type submit) (name preview) (value ,($$ "Preview again"))))
          (input (@ (type submit) (name commit) (value ,($$ "Commit")))))))
  (define (donttouch-checkbox)
    `((input (@ (type checkbox) (name donttouch) (value on) (id donttouch)
                ,@(if donttouch '((checked checked)) '())))
      (label (@ (for donttouch)) ,($$ "Don't update 'Recent Changes'"))))

  `((form
     (@ (method POST) (action ,(cgi-name-of (wiliki))))
     (p (b "By posting, you will grant the rights to modify and "
           "distribute your submission under the terms of the Attribution "
           "ShareAlike 2.0 license. See "
           (a (@ (href ,(url "wiki-license")))
              "wiki-license")
           " for more information."))
     ;; ,@(buttons) ,@(donttouch-checkbox)
     (br)
     (input (@ (type hidden) (name c) (value c)))
     (input (@ (type hidden) (name p) (value ,pagename)))
     (input (@ (type hidden) (name l) (value ,(lang))))
     (input (@ (type hidden) (name mtime) (value ,mtime)))
     (textarea (@ (name content)
                  (class content)
                  (rows ,(textarea-rows-of (wiliki)))
                  (cols ,(textarea-cols-of (wiliki))))
               ,content)
     (br)
     (p ,($$ "ChangeLog (brief summary of your edit for later reference):"))
     (textarea (@ (name logmsg)
                  (class logmsg)
                  (rows 2)
                  (cols ,(textarea-cols-of (wiliki))))
               ,(cond
                 ((and (zero? (string-length logmsg))
                       (wiliki:settings-user-name #f))
                  => (lambda (username)
                       (string-append " (" username ")")))
                 (else
                  logmsg)))
     (br)
     ,@(buttons)
     (br)
     (stree
      ,($$ "<h2>Text Formatting Rules</h2>
      <p>No HTML.</p>
      <p>A line begins with \";;\" doesn't appear in the output (comment).</p>
      <p>A line begins with \"~\" is treated as if it is continued
         from the previous line, except comments.  (line continuation).</p>
      <p>Empty line to separating paragraphs (&lt;p&gt;)</p>
      <p>\"<tt>- </tt>\", \"<tt>-- </tt>\" and \"<tt>--- </tt>\" ... at the
         beginning of a line for an item of unordered list (&lt;ul&gt;).
         Put a space after dash(es).</p>
      <p>\"<tt># </tt>\", \"<tt>## </tt>\", \"<tt>### </tt>\" ... at the
         beginning of a line for an item of ordered list (&lt;ol&gt;).
         Put a space after <tt>#</tt>'s.</p>
      <p>A line with only \"<tt>----</tt>\" is &lt;hr&gt;.</p>
      <p>\"<tt>:item:description</tt>\" at the beginning of a line is &lt;dl&gt;.
         The item includes all colons but the last one.  If you want to include
         a colon in the description, put it in the next line.</p>
      <p><tt>[[Name]]</tt> to make \"Name\" a WikiName.  Note that
         a simple mixed-case word doesn't become a WikiName.
         \"Name\" beginning with \"$\" has special meanings (e.g. 
         \"[[$date]]\" is replaced for the time at the editing.)</p>
      <p>A URL-like string beginning with \"<tt>http:</tt>\" becomes
         a link.  \"<tt>[URL name]</tt>\" becomes a <tt>name</tt> that linked
         to <tt>URL</tt>.</p>
      <p>Surround words by two single quotes (<tt>''foo''</tt>)
         to emphasize.</p>
      <p>Surround words by three single quotes (<tt>'''foo'''</tt>)
         to emphasize more.</p>
      <p>\"<tt>*</tt>\", \"<tt>**</tt>\" and \"<tt>***</tt>\"' ... 
         at the beginning of a line is a header.  Put a space
         after the asterisk(s).</p>
      <p>Whitespace(s) at the beginning of line for preformatted text.</p>
      <p>A line of \"{{{\" starts verbatim text, which ends with
         a line of \"}}}\".
         No formatting is done in verbatim text.  Even comments and line
         continuation don't have effect.</p>
      <p>A line begins with \"||\" and also ends with \"||\" becomes a
         row of a table.  Consecutive rows forms a table.  Inside a row,
         \"||\" delimits columns.</p>
      <p>\"~%\" is replaced for \"&lt;br&gt;\".</p>
      <p>If you want to use special characters at the
         beginning of line, put six consecutive single quotes.
         It emphasizes a null string, so it's effectively nothing.</p>"))
     )))

(define (cmd-edit pagename)
  (unless (editable? (wiliki))
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wiliki-db-get pagename #t)))
    (html-page (make <wiliki-page>
                   :title pagename
                   :content
                   (edit-form #t pagename
                              (ref page 'content)
                              (ref page 'mtime) "" #f)))))

(define (cmd-preview pagename content mtime logmsg donttouch)
  (let ((page (wiliki-db-get pagename #t)))
    (html-page
     (make <wiliki-page>
       :title (format #f ($$ "Preview of ~a") pagename)
       :content
       `(,(preview-box (wiliki:format-content content))
         (hr)
         ,@(edit-form #f pagename content mtime logmsg donttouch))))))

(define (cmd-commit-edit pagename content mtime logmsg donttouch)
  (let ((p   (wiliki-db-get pagename #t))
        (now (sys-time)))

    (define (update-page content)
      (when (page-changed? content (ref p 'content))
        (let1 new-content (expand-writer-macros content)
          (unless (banned-content-page? (wiliki) pagename)
            (write-log (wiliki) pagename (ref p 'content) new-content now logmsg))
          (set! (ref p 'mtime) now)
          (set! (ref p 'content) new-content)
          (wiliki-db-put! pagename p
                          :donttouch donttouch
                          :logmsg logmsg)))
      (redirect-page pagename))

    ;; check if page has been changed.  we should ignore the difference
    ;; of line terminators.
    (define (page-changed? c1 c2)
      (not (equal? (call-with-input-string c1 port->string-list)
                   (call-with-input-string c2 port->string-list))))

    (define (handle-conflict)
      ;; let's see if we can merge changes
      (or (and-let* ((logfile (log-file-path (wiliki)))
                     (picked (wiliki-log-pick-from-file pagename logfile)))
            (let ((common (wiliki-log-revert*
                           (wiliki-log-entries-after picked mtime)
                           (ref p 'content))))
              (receive (merged success?)
                  (wiliki-log-merge common (ref p 'content) content)
                (if success?
                  (update-page (string-join merged "\n" 'suffix))
                  (conflict-page p (conflict->diff merged)
                                 content logmsg donttouch)))))
          (if (equal? (ref p 'content) content)
            (redirect-page pagename) ;; no need to update
            (let1 diff '()
              (diff-report (ref p 'content) content
                           :writer (lambda (line type)
                                     (push! diff
                                            (if type (cons type line) line))))
              (conflict-page p (reverse! diff) content logmsg donttouch)))))

    (define (conflict->diff merged)
      (let1 difflist '()
        (dolist (chunk merged)
          (if (pair? chunk)
            (let1 k (if (eq? (car chunk) 'b) '+ '-)
              (dolist (line (cdr chunk)) (push! difflist (cons k line))))
            (push! difflist chunk)))
        (reverse! difflist)))

    (define (empty-logmsg? logmsg)
      (or (zero? (string-length logmsg))
          (#/^ *\(/ logmsg)))

    (define (handle-empty-logmsg)
      (html-page (make <wiliki-page>
                   :title pagename
                   :content
                   `((p (strong (@ (class "missing-logentry"))
                                "Commit failed: Please supply a log entry and put your"
                                " signature at the end."))
                     ,@(edit-form #t pagename
                                  content mtime logmsg donttouch)))))

    (define (handle-banned-content-permission-denied)
      (html-page (make <wiliki-page>
                   :title pagename
                   :content
                   `((p (strong (@ (class "permission-denied"))
                                "Commit failed: You are not allowed to edit"
                                " this page. Sorry."))
                     ,@(edit-form #t pagename
                                  content mtime logmsg donttouch)))))

    (define (handle-banned-content bad-content)
      (html-page (make <wiliki-page>
                   :title pagename
                   :content
                   `((p (strong (@ (class "permission-denied"))
                                "Commit failed: Your page contained the following banned content: "
                                ,bad-content))
                     ,@(edit-form #t pagename
                                  content mtime logmsg donttouch)))))

    ;; The body of cmd-commit-edit
    ;; If content is empty and the page is not the top page, we erase
    ;; the page.
    (unless (editable? (wiliki))
      (errorf "Can't edit the page ~s: the database is read-only" pagename))
    (cond
     ((empty-logmsg? logmsg)
      (handle-empty-logmsg))
     ((and (banned-content-page? (wiliki) pagename)
           (not (banned-content-passphrase? (wiliki) logmsg)))
      (handle-banned-content-permission-denied))
     ((banned-content (wiliki) logmsg content)
      => handle-banned-content)
     ((and (ref p 'mtime)
           (not (eqv? (ref p 'mtime)
                      mtime)))
      (handle-conflict))
     (else
      (if (banned-content-page? (wiliki) pagename)
          (set! logmsg "Anti-spam maintenance"))
      (update-page content)))))

(define (conflict-page page diff content logmsg donttouch)
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "($$ "Update Conflict"))
     :content
     `((stree ,($$ "<p>It seems that somebody has updated this page
       while you're editing.  The difference is snown below.
       Please revise <a href=\"#edit\">your edit</a> and commit again.</p>"))
       (hr)
       (ul
        (li ,(wiliki:format-diff-line
              `(+ . ,($$ "lines you added (or somebody else deleted)"))))
        (li ,(wiliki:format-diff-line
              `(- . ,($$ "lines somebody else added (or you deleted)")))))
       ,(wiliki:format-diff-pre diff)
       (a (@ (name "edit")) (hr))
       ,($$ "<p>The following shows what you are about to submit.  Please re-edit the content and submit again.</p>")
       ,@(edit-form #t (ref page 'key) content (ref page 'mtime) logmsg donttouch)
       ))))

(define (preview-box content)
  `(table
    (@ (style "table-layout:fixed") (width "100%") (cellpadding 5))
    (tr (td (@ (class "preview"))
            ,@content))))

(provide "wiliki/edit")

