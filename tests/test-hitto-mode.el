;;; -*- lexical-binding: t; -*-

(require 'hitto-mode)

(describe "Hitto-mode"
  (before-each
    (spy-on 'plz))

  (describe "caching"
    (it "works" t))

  (describe "browsing"
    (describe "hitto--read-start"
      :var ((hitto--chapter-id "chapter-id")
            (hitto--chapter-number "5")
            (hitto--manga-name "My Manga")
            (hitto--manga-id "manga-id"))

      (it "creates a manga reading buffer"
        (spy-on 'get-buffer-create)
        (hitto--read-start "My Manga" 5)
        (expect 'get-buffer-create :to-have-been-called-with "*My Manga*")))))
