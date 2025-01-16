;;; -*- lexical-binding: t -*-
;;; hitto-mode.el --- Description
;;
;; Copyright (C) 2024 Jared Arcilla
;;
;; Author: Jared Arcilla <arcillajared@gmail.com>
;; Maintainer: Jared Arcilla <arcillajared@gmail.com>
;; Created: January 15, 2025
;; Modified: January 15, 2025
;; Version: 0.0.1
;; Keywords: multimedia
;; Homepage: https://github.com/jparcill/hitto-mode.el
;; Package-Requires: ((emacs "29.0"))


(require 'image-mode)
(require 'plz)
(require 'url-util)

(defgroup hitto nil
  "Emacs Manga reader"
  :version "29.0"
  :group 'applications
  :group 'data
  :group 'multimedia
  :prefix "hitto-")


(defcustom hitto-view-cache-directory
  (expand-file-name (format "hitto%d" (user-uid))
                    temporary-file-directory)
  "The base directory, where the PNG images will be saved."
  :type 'directory
  :group 'hitto)

(defcustom hitto-use-evil-bindings t
  "Apply evil bindings. Similar to pdf-tools"
  :type 'boolean)

(defvar-local hitto--image-size-factor 0)
(defvar hitto--when-to-async 5)
(defvar-local hitto--page-number nil)
(defvar-local hitto--chapter-number nil)
(defvar-local hitto--manga-id nil)
(defvar-local hitto--chapter-id nil)
(defvar-local hitto--manga-name nil)

(define-derived-mode hitto-mode fundamental-mode "Hitto Mode"
  (if hitto-use-evil-bindings (hitto-apply-evil-bindings) nil))

(defun hitto-apply-evil-bindings ()
  (progn
    (evil-define-key 'normal hitto-mode-map "q" 'kill-this-buffer)
    (evil-define-key 'normal hitto-mode-map "j" 'hitto-scroll-down)
    (evil-define-key 'normal hitto-mode-map "k" 'hitto-scroll-up)
    (evil-define-key 'normal hitto-mode-map "l" 'hitto-next-page)
    (evil-define-key 'normal hitto-mode-map "n" 'hitto-next-page)
    (evil-define-key 'normal hitto-mode-map "h" 'hitto-previous-page)
    (evil-define-key 'normal hitto-mode-map "p" 'hitto-previous-page)
    (evil-define-key 'normal hitto-mode-map "+" 'hitto-increase-size)
    (evil-define-key 'normal hitto-mode-map "=" 'hitto-increase-size)
    (evil-define-key 'normal hitto-mode-map "-" 'hitto-decrease-size)))

(defvar-keymap hitto-mode-map
  :parent image-mode-map
  "Q"        #'kill-this-buffer
  ;; Navigation in the document
  "n"        #'hitto-next-page
  "p"        #'hitto-previous-page
  "<next>"   #'forward-page
  "<prior>"  #'backward-page
  "<remap> <forward-page>"   #'hitto-next-page
  "<remap> <backward-page>"  #'hitto-previous-page
  "DEL"      #'hitto-scroll-down
  "C-n"      #'hitto-scroll-down
  "<down>"   #'hitto-scroll-down
  "<remap> <next-line>"      #'hitto-scroll-down
  "C-p"          #'hitto-scroll-up
  "<up>"         #'hitto-scroll-up
  "<remap> <previous-line>"  #'hitto-scroll-up
  "RET"      #'image-next-line
  ;; Zoom in/out.
  "+"        #'hitto-increase-size
  "="        #'hitto-increase-size
  "-"        #'hitto-decrease-size)

;;;###autoload
(defun hitto-search-manga (manga-string)
  "Search for manga. First point of contact"
  (interactive "sManga Title: ")
   (let*
       ((json-alist (plz 'get (format "https://api.mangadex.org/manga?title=%s"
                                      (url-hexify-string manga-string)) :as #'json-read))
        (data-list (cdr (assoc 'data json-alist)))
        (title-to-id-alist (mapcar
                           (lambda (manga-data) (cons (hitto--get-title-from-data manga-data) (cdr (assoc 'id manga-data))))
                           data-list))
        (selected-manga-name
         (completing-read "Manga Titles: " title-to-id-alist))
        (selected-manga-id (cdr (assoc selected-manga-name title-to-id-alist)))
        (chapters-data-list (hitto--query-for-chapters-data selected-manga-id))
        (chapter-choice-to-data-alist (mapcar
                                     (lambda (chapter) (cons (hitto--chapter-formatted-metadata-string chapter) chapter))
                                       chapters-data-list))
        (selected-chapter-name
         (completing-read "Chapter Titles: " chapter-choice-to-data-alist))
        (selected-chapter-data (cdr (assoc selected-chapter-name chapter-choice-to-data-alist)))
        (selected-chapter-id (cdr (assoc 'id selected-chapter-data)))
        (selected-chapter-number (string-to-number (hitto--assoc-recursive selected-chapter-data 'attributes 'chapter)))
        )
     (progn
       (hitto--cache-chapter selected-chapter-id)
       (hitto--save-metadata-to-buffer-local-vars
        selected-manga-id
        selected-manga-name
        selected-chapter-id
        selected-chapter-number)
       (hitto--read-start selected-manga-name))))


(defun hitto-next-page ()
  (interactive)
    (hitto--read-page (current-buffer) (+ hitto--page-number 1)))

(defun hitto-previous-page ()
  (interactive)
    (hitto--read-page (current-buffer) (- hitto--page-number 1)))

(defun hitto-scroll-up ()
  (interactive)
  (set-window-vscroll (selected-window) (- (window-vscroll (selected-window)) 5)))

(defun hitto-scroll-down ()
  (interactive)
    (set-window-vscroll (selected-window) (+ (window-vscroll (selected-window)) 5)))

(defun hitto-increase-size ()
  (interactive)
  (progn
    (setq hitto--image-size-factor (+ hitto--image-size-factor 1))
    (hitto--refresh-page)))

(defun hitto-decrease-size ()
  (interactive)
  (progn
    (setq hitto--image-size-factor (- hitto--image-size-factor 1))
    (hitto--refresh-page)))

(defun hitto-next-chapter ()
  (interactive)
  (let*
    ((next-chapter-number (+ hitto--chapter-number 1))
     (chapter-data (hitto--chapter-data-from-chapter-number hitto--manga-id (number-to-string next-chapter-number)))
     (next-chapter-id (cdr (assoc 'id chapter-data))))
  (progn
    (hitto--cache-chapter next-chapter-id)
    (hitto--save-metadata-to-buffer-local-vars hitto--manga-id hitto--manga-name next-chapter-id next-chapter-number)
    (hitto--read-start hitto--manga-name))))

(defun hitto-go-to-page (page)
  (interactive "nGoto:")
  (hitto--read-page (current-buffer) page))

;; "Private" Functions
;; --------------------

(defun hitto--refresh-page ()
  (hitto--read-page (current-buffer) hitto--page-number))

(defun hitto--read-page (buffer page)
  (if (file-exists-p (hitto--page-file-name hitto--chapter-id page))
      (let ((page-scale (hitto--page-scale))
            (image-file (hitto--page-file-name hitto--chapter-id page))) ;; For keeping the page the same size
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert-image (create-image image-file nil nil :scale page-scale))
          (setq hitto--page-number page)))) nil)

(defun hitto--page-scale ()
  (+ (/ hitto--image-size-factor 10.0) 1))

(defun hitto--form-img-link (base-link quality chapter-hash data)
  (concat base-link "/" quality "/" chapter-hash "/" data))

(defun hitto--cache-chapter (chapter-id)
  (if (file-directory-p (format "%s/%s" hitto-view-cache-directory chapter-id))
      (print "Chapter already cached")
    (let ((chapter-links (hitto--get-chapter-links chapter-id)))
      (hitto--cache-from-links chapter-links chapter-id 0))))

(defun hitto--get-chapter-links (chapter)
  (let ((json-alist
        (plz 'get
          (format "https://api.mangadex.org/at-home/server/%s" chapter) :as #'json-read)))
      (let ((base-url (cdr (assoc 'baseUrl json-alist)))
            (chapter-hash (cdr (assoc 'hash (cdr (assoc 'chapter json-alist)))))
            (data-list (cdr (assoc 'data (cdr (assoc 'chapter json-alist))))))
        (if (seq-empty-p data-list)
            (progn
              (print "No chapters found")
              '())
          (mapcar
           (lambda (data) (hitto--form-img-link base-url "data" chapter-hash data))
           data-list)))))

(defun hitto--get-title-from-data (manga-data)
  (hitto--assoc-recursive manga-data 'attributes 'title 'en))

(defun hitto--chapter-formatted-metadata-string (chapter-data)
  (let
      ((chapter (hitto--assoc-recursive chapter-data 'attributes 'chapter))
      (title (hitto--assoc-recursive chapter-data 'attributes 'title)))
    (format "Chapter %s %s" chapter title)))

(defun hitto--save-metadata-to-current-buffer (manga-id manga-name chapter-id chapter-number)
  (progn
    (setq hitto--chapter-id chapter-id)
    (setq hitto--chapter-number chapter-number)
    (setq hitto--manga-name manga-name)
    (setq hitto--manga-id manga-id)))

;; Helper functions taken from somewhere on the internet
(defun hitto--assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)


(defun hitto--page-file-name (chapter-id iteration)
  (format "%s/%s/%06d.png" hitto-view-cache-directory chapter-id iteration))

(defun hitto--cache-from-links (links chapter-id iteration)
  (if links (progn (hitto--cache-single-page (car links) chapter-id iteration)
                   (hitto--cache-from-links (cdr links) chapter-id (+ iteration 1)))))

(defun hitto--cache-single-page (link chapter-id iteration)
  (let ((file-name (hitto--page-file-name chapter-id iteration)))
    (unless (file-exists-p file-name)
      (if (< iteration hitto--when-to-async)
        (plz 'get link :as `(file ,file-name))
        (plz 'get link :as `(file ,file-name) :then nil)))))

(defun hitto--query-for-chapters-data (manga-id)
  "Query mangadex. Data is formatted like Vector[Chapter Alist]"
  (let*
      ((chapters-json-data (plz 'get (format "https://api.mangadex.org/manga/%s/feed?limit=100&translatedLanguage[]=en&%s=asc&includeEmptyPages=0"
                                             manga-id
                                             (url-hexify-string "order[chapter]")) :as #'json-read))
       (chapters-data-list (cdr (assoc 'data chapters-json-data))))
    chapters-data-list))

(defun hitto--chapter-data-from-chapter-number (manga-id chapter-number)
  (progn
    (car (seq-filter (lambda (chapter-data) (equal (hitto--assoc-recursive chapter-data 'attributes 'chapter) chapter-number))
                (hitto--query-for-chapters-data manga-id)))))

(defun hitto--save-metadata-to-buffer-local-vars (manga-id manga-name chapter-id chapter-number)
  (progn
    (switch-to-buffer (get-buffer-create (format "*%s*" manga-name)))
    (hitto-mode)
    (setq-local hitto--chapter-id chapter-id
                hitto--chapter-number chapter-number
                hitto--manga-name manga-name
                hitto--manga-id manga-id)))

(defun hitto--read-start (name &optional page)
  (or page (setq page 0))
  (let ((image-buffer (get-buffer-create (format "*%s*" name))))
    (progn
      (switch-to-buffer image-buffer)
      (hitto--read-page image-buffer page))))

;; Debugging functions
;; --------------------

(defun hitto--go-to-images-directory ()
  (find-file (format "%s/%s" hitto-view-cache-directory hitto--chapter-id)))

(provide 'hitto-mode)
;;; hitto-mode.el ends here
