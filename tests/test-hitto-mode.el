;;; -*- lexical-binding: t; -*-

(require 'hitto-mode)

(describe "Hitto-mode"
  (before-each
    (spy-on 'plz))

  (describe "caching"
    (it "works" t)))
