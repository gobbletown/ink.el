;; https://github.com/semiosis/ink.el

;; *( is so it will work in YAML. However, it
;; still requires escaping, so now it's just to
;; differentiate from regular text properties,
;; but the differentiation serves no useful
;; purpose.

(define-derived-mode ink-mode text-mode "Ink"
  "Ink mode"
  :after-hook (ink-decode-buffer))

(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

;; See the ink source.
;; This mode is not meant as a regular editing/prompting environment.
;; Rather, it's like looking at HTML source.
(define-derived-mode ink-source-mode emacs-lisp-mode "Ink source"
  "Ink source mode")

;; (add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-edit-mode))

(defun pen-open-ink (&optional fp)
  (if (not fp)
      (setq fp (get-path)))
  (let* ((dn (f-dirname fp))
         (bn (basename fp))
         (fn (file-name-sans-extension bn))
         (ext (file-name-extension bn)))
    (cond ((or (string-equal "ink" ext)
               (string-equal "INK" ext)))
          (ink-decode-buffer))))

;; (add-hook 'find-file-hooks 'pen-open-ink)
;; (remove-hook 'find-file-hooks 'pen-open-ink)

(defun ink-mode-before-save-hook ()
  (when (eq major-mode 'ink-mode)
    (let ((ink
           (ink-encode-from-textprops (pen-textprops-in-region-or-buffer))))
      (erase-buffer)
      (insert ink))))

(add-hook 'before-save-hook #'ink-mode-before-save-hook)

(defun ink-mode-after-save-hook ()
  (when (eq major-mode 'ink-mode)
    (ink-decode-buffer)))

(add-hook 'after-save-hook #'ink-mode-after-save-hook)

(defun pen-textprops-in-region-or-buffer ()
  (if (region-active-p)
      (format "%S" (buffer-substring (region-beginning) (region-end)))
    (format "%S" (buffer-string))))

;; TODO Ensure that only a particular list of text properties are preserved
(defun ink-encode-from-textprops (s)
  (interactive (list (pen-textprops-in-region-or-buffer)))
  (let ((ink (string-replace "#(" "*(" s)))
    (if (interactive-p)
        (if (pen-selected-p)
            (pen-region-filter (eval `(lambda (s) ,ink)))
          (pen-etv ink))
      ink)))

(defun ink-encode-from-data (text &optional data)
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

(defun ink-depropertize (ink)
  (string-replace "#(" "*(" ink)
  ;; (ink-decode-buffer)
  )

(defun ink-decode (text)
  ;; Do not use (pen-selection t)
  ;; This assumes the text is visibly encoded
  (interactive (list (pen-selection)))

  (if (sor text)
      (let* ((text (if (string-match "\\*(" text)
                       (eval-string (string-replace "*(" "#(" text))
                     text)))
        (if (interactive-p)
            (if (pen-selected-p)
                (pen-region-filter (eval `(lambda (s) ,text)))
              (pen-etv text))
          text))))
(defalias 'ink-propertize 'ink-decode)

(defun ink-decode-buffer ()
  (interactive)
  (let ((s (ink-decode (buffer-string))))
    (erase-buffer)
    (insert s)))

(provide 'pen-ink)