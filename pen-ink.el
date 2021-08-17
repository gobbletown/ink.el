;; https://github.com/semiosis/ink.el

;; *( is so it will work in YAML. However, it
;; still requires escaping, so now it's just to
;; differentiate from regular text properties,
;; but the differentiation serves no useful
;; purpose.

(define-derived-mode ink-mode text-mode "Ink"
  "Ink mode")

(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

;; See the ink source.
;; This mode is not meant as a regular editing/prompting environment.
;; Rather, it's like looking at HTML source.
(define-derived-mode ink-source-mode emacs-lisp-mode "Ink source"
  "Ink source mode")

;; (add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-edit-mode))

(defun ink-encode (text &optional data)
  (interactive (list (pen-selection) pen-last-prompt-data))

  (let* ((text (or
                text
                (pen-selection)))
         (data (or data
                   pen-last-prompt-data)))

    (if (interactive-p)
        (progn
          (pen-alist-setcdr
           'data "PEN_ENGINE"
           (read-string-hist "engine: "
                             (cdr (assoc "PEN_ENGINE" data))))
          (pen-alist-setcdr
           'data "PEN_LANGUAGE"
           (read-string-hist "language: "
                             (cdr (assoc "PEN_LANGUAGE" data))))
          (pen-alist-setcdr
           'data "PEN_TOPIC"
           (read-string-hist "topic: "
                             (or
                              (cdr (assoc "PEN_TOPIC" data))
                              (pen-topic t)))))))

    (if (not (cdr (assoc "PEN_ENGINE" data)))
        (pen-alist-setcdr 'data "PEN_ENGINE" "OpenAI GPT-3"))
    (if (not (cdr (assoc "PEN_LANGUAGE" data)))
        (pen-alist-setcdr 'data "PEN_LANGUAGE" "English"))

  (let* ((ink
          (let ((buf (new-buffer-from-string text))
                (ink)
                (start 1)
                (end))
            (with-current-buffer buf
              (setq end (length text))
              (loop for p in data do
                    (let ((key (car p))
                          (val (cdr p)))
                      (message key)
                      (put-text-property start end key val)))
              (setq ink (format "%S" (buffer-string))))
            (kill-buffer buf)
            ink))
         (ink (string-replace "#(" "*(" ink)))
    (if (interactive-p)
        (if (pen-selected-p)
            (pen-region-filter (eval `(lambda (s) ,ink)))
          (pen-etv ink))
      ink)))

(defun ink-decode (text)
  ;; Do not use (pen-selection t)
  ;; This assumes the text is visibly encoded
  (interactive (list (pen-selection)))

  (if (sor text)
      (let* ((text (if (string-match "\\*(" text)
                       (str (eval-string (string-replace "*(" "#(" text)))
                     text)))
        (if (interactive-p)
            (if (pen-selected-p)
                (pen-region-filter (eval `(lambda (s) ,text)))
              (pen-etv text))
          text))))

(provide 'pen-ink)