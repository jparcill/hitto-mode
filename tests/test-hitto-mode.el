;;; -*- lexical-binding: t; -*-

(require 'hitto-mode)
(require 'image-mode)

(cl-defun fake-pls (method url &rest rest &key (as string))
  (cond (((cl-search "?title=" url) (load-title-response))
         ((cl-search "server" url) (load-chapter-link-response))
         ((cl-search "feed" url) (load-chapters-data-response)))))

(describe "Hitto-mode"
  :var ((title-response '((result . "ok")
                          (response . "collection")
                          (data .
                                [((id . "a77742b1-befd-49a4-bff5-1ad4e6b0ef7b")
                                  (type . "manga")
                                  (attributes
                                   (title
                                    (en . "Chainsaw Man"))
                                   (altTitles .
                                              [((uk . "Людина-бензопила"))
                                               ((kk . "Шынжырлы ара адам"))])
                                   (description
                                    (en . "Broke young man + chainsaw dog demon = Chainsaw Man!"))
                                   (isLocked . t)
                                   (links
                                    (raw . "https://shonenjumpplus.com/episode/10834108156650024834")
                                    (engtl . "https://mangaplus.shueisha.co.jp/titles/100037"))
                                   (originalLanguage . "ja")
                                   (lastVolume . "")
                                   (lastChapter . "")
                                   (publicationDemographic . "shounen")
                                   (status . "ongoing")
                                   (year . 2018)
                                   (contentRating . "suggestive")
                                   (tags .
                                         [((id . "0a39b5a1-b235-4886-a747-1d05d216532d")
                                           (type . "tag")
                                           (attributes
                                            (name
                                             (en . "Award Winning"))
                                            (description)
                                            (group . "format")
                                            (version . 1))
                                           (relationships .
                                                          []))
                                          ((id . "eabc5b4c-6aff-42f3-b657-3e90cbd00b75")
                                           (type . "tag")
                                           (attributes
                                            (name
                                             (en . "Supernatural"))
                                            (description)
                                            (group . "theme")
                                            (version . 1))
                                           (relationships .
                                                          []))])
                                   (state . "published")
                                   (chapterNumbersResetOnNewVolume . :json-false)
                                   (createdAt . "2019-09-10T13:07:45+00:00")
                                   (updatedAt . "2024-11-21T20:47:29+00:00")
                                   (version . 109)
                                   (availableTranslatedLanguages .
                                                                 ["fa" "hu" "cs" "en"])
                                   (latestUploadedChapter . "538382fa-db41-45f6-9425-5e6336d46cd6"))
                                  (relationships .
                                                 [((id . "f85a5b93-3c87-4c61-9032-07ceacbb9e64")
                                                   (type . "author"))
                                                  ((id . "f85a5b93-3c87-4c61-9032-07ceacbb9e64")
                                                   (type . "artist"))
                                                  ((id . "394b92b9-056a-4b43-a8c4-4e46eac82ec4")
                                                   (type . "cover_art"))
                                                  ((id . "61325e33-e80b-443d-9b81-53b857b6ecdb")
                                                   (type . "manga")
                                                   (related . "doujinshi"))
                                                  ((id . "e896c48c-3150-437d-ba57-d8567eb399ae")
                                                   (type . "manga")
                                                   (related . "colored"))]))])
                          (limit . 10)
                          (offset . 0)
                          (total . 9)))
        (chapter-response '((result . "ok")
                            (response . "collection")
                            (data .
                                  [((id . "73af4d8d-1532-4a72-b1b9-8f4e5cd295c9")
                                    (type . "chapter")
                                    (attributes
                                     (volume . "1")
                                     (chapter . "1")
                                     (title . "A Dog and a Chainsaw")
                                     (translatedLanguage . "en")
                                     (externalUrl)
                                     (publishAt . "2020-06-30T00:05:51+00:00")
                                     (readableAt . "2020-06-30T00:05:51+00:00")
                                     (createdAt . "2020-06-30T00:05:51+00:00")
                                     (updatedAt . "2020-06-30T00:05:51+00:00")
                                     (pages . 53)
                                     (version . 1))
                                    (relationships .
                                                   [((id . "9348bde7-ed3e-43e7-88c6-0edcd1debb88")
                                                     (type . "scanlation_group"))
                                                    ((id . "a77742b1-befd-49a4-bff5-1ad4e6b0ef7b")
                                                     (type . "manga"))
                                                    ((id . "f183117e-6e9a-4e3f-a4ce-08fb5d7b6021")
                                                     (type . "user"))]))])
                            (limit . 100)
                            (offset . 0)
                            (total . 98))))

  (describe "hitto-search-manga"
    (before-each
      (spy-on 'hitto--mangadex-search-manga-title :and-return-value title-response)
      (spy-on 'hitto--mangadex-search-chapter :and-return-value chapter-response)
      (spy-on 'hitto--cache-chapter :and-return-value t)
      (spy-on 'hitto--save-metadata-to-buffer-local-vars)
      (spy-on 'hitto--read-start)
      (spy-on 'completing-read :and-return-value "Chapter 1 A Dog and a Chainsaw")
      (funcall-interactively #'hitto-search-manga "Chainsaw Man"))

    (it "calls mangadex"
      (expect 'hitto--mangadex-search-manga-title :to-have-been-called)
      (expect 'hitto--mangadex-search-chapter :to-have-been-called))

    (it "caches the chapter"
      (expect 'hitto--cache-chapter :to-have-been-called-with "73af4d8d-1532-4a72-b1b9-8f4e5cd295c9")))

  (describe "browsing"
    (describe "hitto--read-page"
      :var ((hitto--page-number nil)
            (hitto--chapter-id "blah-id")
            (buffer (get-buffer-create "*My Manga*")))
      (before-each
        (spy-on 'hitto--page-file-name :and-return-value "file-name01.png")
        (spy-on 'hitto--page-scale :and-return-value 1)
        (spy-on 'insert-image :and-return-value 1)
        (spy-on 'file-exists-p :and-return-value t)
        (hitto--read-page buffer 5))

      (it "inserts an image"
        (expect 'insert-image :to-have-been-called))

      (it "sets hitto--page-number"
        (expect hitto--page-number :to-be 5)))

    (describe "turning pages"
      (before-each
        (spy-on 'hitto--read-page)
        (setq hitto--page-number 3))

      (describe "hitto-next-page"
        (before-each
          (funcall-interactively #'hitto-next-page))
        (it "can turn the next page"
          (expect 'hitto--read-page :to-have-been-called-with (current-buffer) 4)))

      (describe "hitto-previous-page"
        (before-each
          (funcall-interactively #'hitto-previous-page))
        (it "can turn the next page"
          (expect 'hitto--read-page :to-have-been-called-with (current-buffer) 2))))

    (describe "hitto-next-chapter"
      (before-each
        (spy-on 'hitto--cache-chapter :and-return-value 1)
        (spy-on 'hitto--read-start)
        (spy-on 'hitto--mangadex-search-chapter :and-return-value chapter-response)
        (setq hitto--chapter-number 0)
        (setq hitto--manga-name "Chainsaw Man")
        (funcall-interactively #'hitto-next-chapter))

      (it "caches and reads next chapter"
        (expect 'hitto--cache-chapter :to-have-been-called-with "73af4d8d-1532-4a72-b1b9-8f4e5cd295c9")
        (expect 'hitto--read-start :to-have-been-called-with "Chainsaw Man")))))
