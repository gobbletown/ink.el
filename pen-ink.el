;; https://github.com/semiosis/ink.el

;; *( is so it will work in YAML. However, it
;; still requires escaping, so now it's just to
;; differentiate from regular text properties,
;; but the differentiation serves no useful
;; purpose.

;; When you save in one, the other is updated
(define-derived-mode ink-mode text-mode "Ink"
  "Ink mode"
  :after-hook
  (progn
    (add-hook 'write-contents-functions #'ink-mode-before-save-hook)))

;; See the ink source.
;; This mode is not meant as a regular editing/prompting environment.
;; Rather, it's like looking at HTML source.
(define-derived-mode ink-source-mode emacs-lisp-mode "Ink source"
  "Ink source mode"
  :after-hook
  (progn
    (add-hook 'write-contents-functions #'ink-source-mode-before-save-hook)
    (ink-decode-source-buffer)))

(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-source-mode))

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
          (ink-decode-source-buffer))))

;; (add-hook 'find-file-hooks 'pen-open-ink)
;; (remove-hook 'find-file-hooks 'pen-open-ink)

(defun ink-mode-before-save-hook (&optional args)
  ;; (throw 'ink-mode-save-source t)
  (when (eq major-mode 'ink-mode)
    (let* ((ink
            (ink-encode-from-textprops (pen-textprops-in-region-or-buffer)))
           (b (current-buffer))
           (bufn (s-replace-regexp "^\\*\\(.*\\)\\*$" "\\1" (buffer-name)))
           (bn (basename bufn)))
      (kill-buffer b)
      (with-current-buffer (switch-to-buffer bn)
        (erase-buffer)
        (insert ink)
        (beginning-of-buffer)
        (save-buffer))
      ;; (error "abort real save")
      ;; (throw 'ink-mode-save-source t)
      )
    t))

(defun ink-source-mode-before-save-hook (&optional args)
  (when (eq major-mode 'ink-source-mode)
    (ink-decode-source-buffer)
    t))

(defun ink-mode-after-save-hook ()
  (when (eq major-mode 'ink-mode)
    ;; Write back to the original .ink buffer
    ;; (ink-decode-source-buffer)
    ))

(add-hook 'after-save-hook #'ink-mode-after-save-hook)

;; TODO The combination of doing both of these here may result in less duplication and less random changing of the file
;; - filter only properties I want
;; - sort
;; TODO
;; - clear all
(defun pen-textprops-in-region-or-buffer ()
  (if (region-active-p)
      (format "%S" (buffer-substring (region-beginning) (region-end)))
    (format "%S" (buffer-string))))

;; TODO Ensure that only a particular list of text properties are preserved
(defun ink-encode-from-textprops (s)
  (interactive (list (pen-textprops-in-region-or-buffer)))
  (let ((ink s))
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
         (ink ink))
    (if (interactive-p)
        (if (pen-selected-p)
            (pen-region-filter (eval `(lambda (s) ,ink)))
          (pen-etv ink))
      ink)))

(defun ink-depropertize (ink)
  ink
  ;; (ink-decode-source-buffer)
  )

(defun ink-remove-bad-properties ()
  (interactive)
  (remove-text-properties
   (point-min)
   (point-max)
   (let ((lst (ink-list-all-bad-properties (buffer-string))))
     (-interleave lst (make-list (length lst) nil)))))

;; (etv (pps (ink-list-all-properties (buffer-string))))
(defun ink-list-all-bad-properties (s)
  (-filter
   (lambda (e)
     (not (and (stringp (car e))
               (string-match "^PEN_" (car e)))))
   (-uniq
    (flatten-once
     (loop for inl in (object-intervals s)
           collect
           (loop for (p v) on (nth 2 inl) while v
                 collect
                 (list p v)))))))

(defun ink-decode (text)
  ;; Do not use (pen-selection t)
  ;; This assumes the text is visibly encoded
  (interactive (list (pen-selection)))

  (if (sor text)
      (let* ((text (if (string-match "#(" text)
                       (eval-string text)
                     text)))
        (if (interactive-p)
            (if (pen-selected-p)
                (pen-region-filter (eval `(lambda (s) ,text)))
              (pen-etv text))
          text))))
(defalias 'ink-propertize 'ink-decode)

(defun ink-decode-source-buffer ()
  (interactive)
  (if (eq major-mode 'ink-source-mode)
      (let* ((s (ink-decode (buffer-string)))
             (fp (buffer-file-name))
             (bufname (concat "*" fp "*")))
        (let ((bexists (buffer-exists bufname)))
          (with-current-buffer (switch-to-buffer bufname)
            (if (or (not bexists)
                    (yn "Reload from .ink?"))
                (progn
                  (erase-buffer)
                  (insert s)
                  (beginning-of-buffer)))
            (ink-mode)
            (current-buffer))))))

(provide 'pen-ink)