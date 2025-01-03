;;; hitto-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jared Arcilla
;; TODO Description

(require 'image-mode)
(require 'plz)
(require 'url-util)
(require 'hydra)

(defgroup hitto-mode nil
  "Mangadex reader"
;; todo
;;  :link '(function-link doc-view)
  :version "29.4"
 ;; :group 'applications
 ;; :group 'data
 ;; :group 'multimedia
  :prefix "hitto-")

(defcustom hitto-view-cache-directory
  (expand-file-name (format "hitto%d" (user-uid))
                    temporary-file-directory)
  "The base directory, where the PNG images will be saved."
  :type 'directory)

(defun hitto-form-img-link (base-link quality chapter-hash data)
  (concat base-link "/" quality "/" chapter-hash "/" data)
  )

(defun hitto-get-chapter-links (chapter)
  ;; TODO refactor to use let*
  (let ((json-alist
        (plz 'get
          (format "https://api.mangadex.org/at-home/server/%s" chapter) :as #'json-read)))
    (let ((base-url (cdr (assoc 'baseUrl json-alist)))
          (chapter-hash (cdr (assoc 'hash (cdr (assoc 'chapter json-alist)))))
          (data-list (cdr (assoc 'data (cdr (assoc 'chapter json-alist))))))
      (mapcar
       (lambda (data) (hitto-form-img-link base-url "data" chapter-hash data))
       data-list))))

(defun hitto-cache-chapter (chapter-id)
  (if (file-directory-p (format "%s/%s" hitto-view-cache-directory chapter-id))
      (print "Chapter already cached")
    (let ((chapter-links (hitto-get-chapter-links chapter-id)))
      (hitto-cache-from-links chapter-links chapter-id 0))))

(defun hitto-cache-from-links (links chapter-id iteration)
  (if links (progn (hitto-cache-single-page (car links) chapter-id iteration)
                   (hitto-cache-from-links (cdr links) chapter-id (+ iteration 1)))))
(defun hitto-cache-single-page (link chapter-id iteration)
  (let ((file-name (format "%s/%s/%06d.png" hitto-view-cache-directory chapter-id iteration)))
    (unless (file-exists-p file-name)
        (plz 'get link :as `(file ,file-name)))))

;; Search functions
(defun hitto-get-title-from-data (manga-data)
  (assoc-recursive manga-data 'attributes 'title 'en))

(defun hitto-chapter-formatted-metadata-string (chapter-data)
  (let
      ((chapter (assoc-recursive chapter-data 'attributes 'chapter))
      (language (assoc-recursive chapter-data 'attributes 'translatedLanguage))
      (title (assoc-recursive chapter-data 'attributes 'title)))
    (format "Chapter %s %s" chapter title)
    ))


(defun hitto-search-manga (manga-string)
  "Search for manga. First point of contact"
  (interactive "s")
   (let*
       ((json-alist (plz 'get (format "https://api.mangadex.org/manga?title=%s"
                                      (url-hexify-string manga-string)) :as #'json-read))
        (data-list (cdr (assoc 'data json-alist)))
        (title-to-id-alist (mapcar
                           (lambda (manga-data) (cons (hitto-get-title-from-data manga-data) (cdr (assoc 'id manga-data))))
                           data-list))
        (selected-manga-name
         (completing-read "Manga Titles: " title-to-id-alist))
        (selected-manga-id (cdr (assoc selected-manga-name title-to-id-alist)))
        (chapter-json-data (plz 'get (format "https://api.mangadex.org/manga/%s/feed?limit=100&translatedLanguage[]=en&%s=asc"
                                             selected-manga-id
                                             (url-hexify-string "order[chapter]")) :as #'json-read))
        (chapter-data-list (cdr (assoc 'data chapter-json-data)))
        (chapter-choice-to-id-alist (mapcar
                                     (lambda (chapter) (cons (hitto-chapter-formatted-metadata-string chapter) (cdr (assoc 'id chapter))))
                                       chapter-data-list))
        (selected-chapter-name
         (completing-read "Chapter Titles: " chapter-choice-to-id-alist))
        (selected-chapter-id (cdr (assoc selected-chapter-name chapter-choice-to-id-alist)))
        )
     (progn
       (print (format "Caching %s %s" selected-chapter-name selected-chapter-id))
       (hitto-cache-chapter selected-chapter-id)
       (hitto-read-start selected-chapter-id selected-chapter-name))))

;; Helper functions taken from somewhere on the internet
(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)


;; Displaying images
(define-derived-mode hitto-mode image-mode "Manga Reader")

(define-minor-mode hitto-mode-keys-minor-mode
  "Overriding keys"
  :lighter "hitto"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-n") 'hitto-next-page)
            (define-key map (kbd "C-c C-p") 'hitto-previous-page)
            (define-key map (kbd "C-c C-j") 'image-scroll-down)
            (define-key map (kbd "C-c C-k") 'image-scroll-up)
            map))

(setq-default hitto-last-used-buffer nil)
(setq hitto-image-files nil)

(defun hitto-read-start (chapter-id name &optional page)
  (or page (setq page 0))
  (let ((image-buffer (get-buffer-create (format "*%s*" name))))
    (progn
      (setq-default hitto-last-used-buffer image-buffer)
      (with-current-buffer image-buffer
        (hitto-mode)
        (hitto-mode-keys-minor-mode 1)
        (make-local-variable 'hitto-page-number)
        (make-local-variable 'hitto-image-files)
        (setq hitto-image-files (vconcat (directory-files (format "%s/%s" hitto-view-cache-directory chapter-id) t "\\.png$")))
        (hitto-read-page image-buffer page)))))

(defun hitto-read-page (buffer page)
  (let ((page-scale (hitto-page-scale)))
    (if (and (< page (length hitto-image-files)) (>= page 0))
        (progn
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (erase-buffer)
            (insert-image (create-image
                           (aref hitto-image-files page) nil nil :scale page-scale))
            (setq hitto-page-number page))
          (hitto-mode-nav/body)) nil)))

(defun hitto-next-page ()
  (interactive)
  (with-current-buffer hitto-last-used-buffer
    (hitto-read-page hitto-last-used-buffer (+ hitto-page-number 1))))

(defun hitto-previous-page ()
  (interactive)
  (with-current-buffer hitto-last-used-buffer
    (hitto-read-page hitto-last-used-buffer (- hitto-page-number 1))))

(defun hitto-scroll-up ()
  (interactive)
  (set-window-vscroll (selected-window) (- (window-vscroll (selected-window)) 10)))

(defun hitto-scroll-down ()
  (interactive)
    (set-window-vscroll (selected-window) (+ (window-vscroll (selected-window)) 10)))

(setq hitto-image-size-factor 0)

(defun hitto-refresh-page ()
  (hitto-read-page (current-buffer) hitto-page-number))

(defun hitto-increase-size ()
  (interactive)
  (progn
    (setq hitto-image-size-factor (+ hitto-image-size-factor 1))
    (hitto-refresh-page)))

(defun hitto-decrease-size ()
  (interactive)
  (progn
    (setq hitto-image-size-factor (- hitto-image-size-factor 1))
    (hitto-refresh-page)))

(defun hitto-page-scale ()
  (+ (/ hitto-image-size-factor 10.0) 1))

;; Bindings
(defhydra hitto-mode-nav ()
  ("n" hitto-next-page "Next")
  ("p" hitto-previous-page "Prev")
  ("+" hitto-increase-size "Zoom In")
  ("-" hitto-decrease-size "Zoom Out")
  ("u" hitto-scroll-up "Up")
  ("d" hitto-scroll-down "Down"))

;; Random helper functions for myself
(defun hitto-set-this-as-last-used-buffer ()
  (interactive)
  (setq hitto-last-used-buffer (current-buffer)))

(provide 'hitto-mode)
;;; hitto-mode.el ends here
