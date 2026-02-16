((acp :source "elpaca-menu-lock-file" :recipe
      (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth
                treeless :host github :ref
                "b9bc89948bb1242e613b8ed5d271c88c00e2ef4a"))
 (agent-shell :source "elpaca-menu-lock-file" :recipe
              (:package "agent-shell" :fetcher github :repo
                        "xenodium/agent-shell" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :host github :ref
                        "034baa3f6df4bc0cb8f2056339f50b658005647e"))
 (agent-shell-manager :source "elpaca-menu-lock-file" :recipe
                      (:source nil :protocol https :inherit t :depth
                               treeless :host github :repo
                               "jethrokuan/agent-shell-manager"
                               :package "agent-shell-manager" :ref
                               "53b73f13ed1ac9d2de128465a8504a7265490ea7"))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source "MELPA"
                :protocol https :inherit t :depth treeless :ref
                "0e94a06bb035953cbbb4242568b38ca15443ad4c"))
 (annalist :source "elpaca-menu-lock-file" :recipe
           (:package "annalist" :fetcher github :repo
                     "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (ansible-vault-with-editor :source "elpaca-menu-lock-file" :recipe
                            (:source nil :protocol https :inherit t
                                     :depth treeless :host github
                                     :repo
                                     "rynffoll/ansible-vault-with-editor"
                                     :package
                                     "ansible-vault-with-editor" :ref
                                     "6f1c0e1607551cbe849f9d3e4a71768992d4c5af"))
 (anzu :source "elpaca-menu-lock-file" :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu"
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "21cb5ab2295614372cb9f1a21429381e49a6255f"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "31cb2fea8f4bc7a593acd76187a89075d8075500"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth
                treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "2b2a5c5bef16eddcce507d9b5804e5a0cc9481ae"))
 (chatgpt-shell :source "elpaca-menu-lock-file" :recipe
                (:package "chatgpt-shell" :fetcher github :repo
                          "xenodium/chatgpt-shell" :files
                          ("*.el"
                           (:exclude "test_chatgpt-shell.el"
                                     "shell-maker.el"
                                     "ob-chatgpt-shell.el"
                                     "dall-e-shell.el"
                                     "ob-dall-e-shell.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "5dd003ec70e9f45b5e8179487bc42f32bfe1037e"))
 (cider :source "elpaca-menu-lock-file" :recipe
        (:package "cider" :fetcher github :repo "clojure-emacs/cider"
                  :files
                  ("lisp/*.el" "bin/*.sh" "*.el" "clojure.sh"
                   "lein.sh" (:exclude ".dir-locals.el"))
                  :old-names (nrepl) :source "MELPA" :protocol https
                  :inherit t :depth treeless :ref
                  "ae247d88455c79d6c039d9c322347c77ab3ad11b"))
 (claude-code :source "elpaca-menu-lock-file" :recipe
              (:package "claude-code" :fetcher github :repo
                        "stevemolitor/claude-code.el" :files
                        (:defaults (:exclude "install-deps.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :host github :ref
                        "4a9914bd4161eb43f489820f9174c62390e5adc8"))
 (claude-code-ide :source "elpaca-menu-lock-file" :recipe
                  (:source nil :protocol https :inherit t :depth
                           treeless :host github :repo
                           "manzaltu/claude-code-ide.el" :package
                           "claude-code-ide" :ref
                           "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364"))
 (clj-refactor :source "elpaca-menu-lock-file" :recipe
               (:package "clj-refactor" :fetcher github :repo
                         "clojure-emacs/clj-refactor.el" :files
                         (:defaults "CHANGELOG.md") :source "MELPA"
                         :protocol https :inherit t :depth treeless
                         :ref
                         "362cb46bf808dc42d2aaf022afe93048439680c4"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
               (:package "clojure-mode" :repo
                         "clojure-emacs/clojure-mode" :fetcher github
                         :files ("clojure-mode.el") :source "MELPA"
                         :protocol https :inherit t :depth treeless
                         :ref
                         "bddba12e969c456236e2b6a1881017a6cafe64b4"))
 (clojure-ts-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "clojure-ts-mode" :repo
                            "clojure-emacs/clojure-ts-mode" :fetcher
                            github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t
                            :depth treeless :ref
                            "96fdffcbe9e1b8ebf9ad14e23b06f62cc3422e22"))
 (colorful-mode :source "elpaca-menu-lock-file" :recipe
                (:package "colorful-mode" :repo
                          ("https://github.com/DevelopmentCool2449/colorful-mode"
                           . "colorful-mode")
                          :files ("*" (:exclude ".git")) :source
                          "GNU ELPA" :protocol https :inherit t :depth
                          treeless :ref
                          "865b92a60924554d04bad6b889f15fb0a88690de"))
 (compile-multi :source "elpaca-menu-lock-file" :recipe
                (:package "compile-multi" :fetcher github :repo
                          "mohkale/compile-multi" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "d111f99303ceb0354e37e2a5cd7f504d19f105f7"))
 (compile-multi-embark :source "elpaca-menu-lock-file" :recipe
                       (:package "compile-multi-embark" :fetcher
                                 github :repo "mohkale/compile-multi"
                                 :files
                                 ("extensions/compile-multi-embark/compile-multi-embark*.el")
                                 :source "MELPA" :protocol https
                                 :inherit t :depth treeless :ref
                                 "d111f99303ceb0354e37e2a5cd7f504d19f105f7"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth
             treeless :ref "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "d1d39d52151a10f7ca29aa291886e99534cc94db"))
 (consult-dir :source "elpaca-menu-lock-file" :recipe
              (:package "consult-dir" :fetcher github :repo
                        "karthink/consult-dir" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "1497b46d6f48da2d884296a1297e5ace1e050eb5"))
 (consult-eglot :source "elpaca-menu-lock-file" :recipe
                (:package "consult-eglot" :fetcher github :repo
                          "mohkale/consult-eglot" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "d8b444aac39edfc6473ffbd228df3e9119451b51"))
 (consult-eglot-embark :source "elpaca-menu-lock-file" :recipe
                       (:package "consult-eglot-embark" :fetcher
                                 github :repo "mohkale/consult-eglot"
                                 :files
                                 ("extensions/consult-eglot-embark/consult-eglot-embark*.el")
                                 :source "MELPA" :protocol https
                                 :inherit t :depth treeless :ref
                                 "d8b444aac39edfc6473ffbd228df3e9119451b51"))
 (consult-git-log-grep :source "elpaca-menu-lock-file" :recipe
                       (:package "consult-git-log-grep" :fetcher
                                 github :repo
                                 "ghosty141/consult-git-log-grep"
                                 :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "MELPA" :protocol https
                                 :inherit t :depth treeless :ref
                                 "5b1669ebaff9a91000ea185264cfcb850885d21f"))
 (consult-todo :source "elpaca-menu-lock-file" :recipe
               (:package "consult-todo" :fetcher github :repo
                         "eki3z/consult-todo" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t
                         :depth treeless :ref
                         "f9ba063a6714cb95ddbd886786ada93771f3c140"))
 (copilot :source "elpaca-menu-lock-file" :recipe
          (:package "copilot" :fetcher github :repo
                    "copilot-emacs/copilot.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :host github :ref
                    "984a7dd376ae133ea5d6704866939b2e5a733ec1"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  (:defaults "extensions/corfu-*.el") :fetcher github
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "abfe0003d71b61ffdcf23fc6e546643486daeb69"))
 (corfu-terminal :source "elpaca-menu-lock-file" :recipe
                 (:package "corfu-terminal" :repo
                           ("https://codeberg.org/akib/emacs-corfu-terminal"
                            . "corfu-terminal")
                           :files ("*" (:exclude ".git")) :source
                           "NonGNU ELPA" :protocol https :inherit t
                           :depth treeless :ref
                           "501548c3d51f926c687e8cd838c5865ec45d03cc"))
 (crux :source "elpaca-menu-lock-file" :recipe
       (:package "crux" :fetcher github :repo "bbatsov/crux" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "3b72275fce66162770b53cf72eb72515c3e68492"))
 (daemons :source "elpaca-menu-lock-file" :recipe
          (:package "daemons" :fetcher github :repo
                    "cbowdon/daemons.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "4900fe1ec64ab339da29082e8fd4545fc6e48ec4"))
 (dape :source "elpaca-menu-lock-file" :recipe
       (:package "dape" :repo
                 ("https://github.com/svaante/dape" . "dape") :files
                 ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
                 https :inherit t :depth treeless :ref
                 "995bd712eef01e9303bc2e3262ff7f20a401a9ed"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :protocol
                 https :inherit t :depth treeless :ref
                 "fb443e7a6e660ba849cafcd01021d9aac3ac6764"))
 (default-text-scale :source "elpaca-menu-lock-file" :recipe
                     (:package "default-text-scale" :fetcher github
                               :repo "purcell/default-text-scale"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https
                               :inherit t :depth treeless :ref
                               "e23b3f8d402dd3871ee5e6dacd10fda223128896"))
 (deflate :source "elpaca-menu-lock-file" :recipe
          (:package "deflate" :fetcher github :repo "skuro/deflate"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "d3863855d213f73dc7a1a54736d94a75f8f7e9c5"))
 (deft :source "elpaca-menu-lock-file" :recipe
       (:package "deft" :repo "jrblevin/deft" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "b369d7225d86551882568788a23c5497b232509c"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "rynffoll/diff-hl"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :host github :branch "dired-nested-paths"
                    :ref "0a3df5280d58cc81c6d866fbb63ec6bf5190b921"))
 (difftastic :source "elpaca-menu-lock-file" :recipe
             (:package "difftastic" :fetcher github :repo
                       "pkryger/difftastic.el" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "b33554a22c637f147d07c15fa9539c72bcfcfca0"))
 (dired-git-info :source "elpaca-menu-lock-file" :recipe
                 (:package "dired-git-info" :repo
                           ("https://github.com/clemera/dired-git-info"
                            . "dired-git-info")
                           :files ("*" (:exclude ".git")) :source
                           "GNU ELPA" :protocol https :inherit t
                           :depth treeless :ref
                           "91d57e3a4c5104c66a3abc18e281ee55e8979176"))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
                    (:package "dired-hacks-utils" :fetcher github
                              :repo "Fuco1/dired-hacks" :files
                              ("dired-hacks-utils.el") :source "MELPA"
                              :protocol https :inherit t :depth
                              treeless :ref
                              "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-sidebar :source "elpaca-menu-lock-file" :recipe
                (:package "dired-sidebar" :fetcher github :repo
                          "jojojames/dired-sidebar" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "3bc8927ed4d14a017eefc75d5af65022343e2ac1"))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
                (:package "dired-subtree" :fetcher github :repo
                          "Fuco1/dired-hacks" :files
                          ("dired-subtree.el") :source "MELPA"
                          :protocol https :inherit t :depth treeless
                          :ref
                          "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (diredfl :source "elpaca-menu-lock-file" :recipe
          (:package "diredfl" :fetcher github :repo "purcell/diredfl"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "fe72d2e42ee18bf6228bba9d7086de4098f18a70"))
 (dirvish :source "elpaca-menu-lock-file" :recipe
          (:package "dirvish" :fetcher github :repo
                    "alexluigit/dirvish" :files
                    (:defaults "extensions/*.el") :source "MELPA"
                    :protocol https :inherit t :depth treeless :ref
                    "d877433f957a363ad78b228e13a8e5215f2d6593"))
 (disk-usage :source "elpaca-menu-lock-file" :recipe
             (:package "disk-usage" :repo
                       ("https://gitlab.com/ambrevar/emacs-disk-usage"
                        . "disk-usage")
                       :files ("*" (:exclude ".git")) :source
                       "GNU ELPA" :protocol https :inherit t :depth
                       treeless :ref
                       "b0d803f2cec3afc2937840f9ba66e3f903d6c415"))
 (disproject :source "elpaca-menu-lock-file" :recipe
             (:package "disproject" :fetcher github :repo
                       "aurtzy/disproject" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "9282473adbf450785741fe6cd02abf6ffed7b9f0"))
 (docker :source "elpaca-menu-lock-file" :recipe
         (:package "docker" :fetcher github :repo "Silex/docker.el"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth
                   treeless :ref
                   "916686b86e83a3bd2281fbc5e6f98962aa747429"))
 (docker-compose-mode :source "elpaca-menu-lock-file" :recipe
                      (:package "docker-compose-mode" :repo
                                "meqif/docker-compose-mode" :fetcher
                                github :files
                                (:defaults
                                 (:exclude
                                  "docker-compose-mode-helpers.el"))
                                :source "MELPA" :protocol https
                                :inherit t :depth treeless :ref
                                "abaa4f3aeb5c62d7d16e186dd7d77f4e846e126a"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
                (:package "doom-modeline" :repo
                          "seagle0128/doom-modeline" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "313beafeabb79ae01ad88eb09918f09309e267c1"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo
                        "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el"
                                   "extensions/*.el")
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "53645a905dfb3055db52f5d418d5ef612027e062"))
 (doric-themes :source "elpaca-menu-lock-file" :recipe
               (:package "doric-themes" :repo
                         ("https://github.com/protesilaos/doric-themes"
                          . "doric-themes")
                         :files ("*" (:exclude ".git")) :source
                         "GNU ELPA" :protocol https :inherit t :depth
                         treeless :ref
                         "03c73b9250195e82c0c37055b7767ea39b8016ff"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo
                ("https://codeberg.org/akib/emacs-eat" . "eat") :files
                ("*" (:exclude ".git")) :source "NonGNU ELPA"
                :protocol https :inherit t :depth treeless :ref
                "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (edit-indirect :source "elpaca-menu-lock-file" :recipe
                (:package "edit-indirect" :fetcher github :repo
                          "Fanael/edit-indirect" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "82a28d8a85277cfe453af464603ea330eae41c05"))
 (ef-themes :source "elpaca-menu-lock-file" :recipe
            (:package "ef-themes" :repo
                      ("https://github.com/protesilaos/ef-themes"
                       . "ef-themes")
                      :files
                      ("*"
                       (:exclude ".git" "COPYING" "doclicense.texi"
                                 "contrast-ratios.org"))
                      :source "GNU ELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "af6a2a588f72a40212624458278d87f1c769405b"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs"
                       :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "1508298c1ed19c81fa4ebc5d22d945322e9e4c52" :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build (:not elpaca--compile-info)
                               :source "Elpaca extensions" :protocol
                               https :inherit t :depth treeless :ref
                               "1508298c1ed19c81fa4ebc5d22d945322e9e4c52"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github
                   :files ("embark.el" "embark-org.el" "embark.texi")
                   :source "MELPA" :protocol https :inherit t :depth
                   treeless :ref
                   "7b3b2fa239c34c2e304eab4367a4f5924c047e2b"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark"
                           :fetcher github :files
                           ("embark-consult.el") :source "MELPA"
                           :protocol https :inherit t :depth treeless
                           :ref
                           "7b3b2fa239c34c2e304eab4367a4f5924c047e2b"))
 (eros :source "elpaca-menu-lock-file" :recipe
       (:package "eros" :fetcher github :repo "xiongtx/eros" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "66ee90baa3162fea028f5101ddcc370f7d1d4fcf"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi"
                            (:exclude "evil-test-helpers.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :wait t :ref
                 "729d9a58b387704011a115c9200614e32da3cefc"))
 (evil-anzu :source "elpaca-menu-lock-file" :recipe
            (:package "evil-anzu" :fetcher github :repo
                      "emacsorphanage/evil-anzu" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "7309650425797420944075c9c1556c7c1ff960b3"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-collection" :fetcher github :repo
                            "emacs-evil/evil-collection" :files
                            (:defaults "modes") :source "MELPA"
                            :protocol https :inherit t :depth treeless
                            :ref
                            "d052ad2ec1f6a4b101f873f01517b295cd7dc4a9"))
 (evil-commentary :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-commentary" :repo
                            "linktohack/evil-commentary" :fetcher
                            github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t
                            :depth treeless :ref
                            "c5945f28ce47644c828aac1f5f6ec335478d17fb"))
 (evil-mc :source "elpaca-menu-lock-file" :recipe
          (:package "evil-mc" :fetcher github :repo "gabesoft/evil-mc"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "7e363dd6b0a39751e13eb76f2e9b7b13c7054a43"))
 (evil-org :source "elpaca-menu-lock-file" :recipe
           (:package "evil-org" :fetcher github :repo
                     "Somelauw/evil-org-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "b1f309726b1326e1a103742524ec331789f2bf94"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo
                          "emacs-evil/evil-surround" :fetcher github
                          :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (evil-terminal-cursor-changer :source "elpaca-menu-lock-file" :recipe
                               (:package
                                "evil-terminal-cursor-changer"
                                :fetcher github :repo
                                "7696122/evil-terminal-cursor-changer"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :protocol https
                                :inherit t :depth treeless :ref
                                "2358f3e27d89128361cf80fcfa092fdfe5b52fd8"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher
                                 github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "MELPA" :protocol https
                                 :inherit t :depth treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth
              treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (fish-mode :source "elpaca-menu-lock-file" :recipe
            (:package "fish-mode" :fetcher github :repo
                      "wwwjfy/emacs-fish" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))
 (flymake-shellcheck :source "elpaca-menu-lock-file" :recipe
                     (:package "flymake-shellcheck" :repo
                               "federicotdn/flymake-shellcheck"
                               :fetcher github :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https
                               :inherit t :depth treeless :ref
                               "d55666f55e126f0e861f7e886202c17c6a1cf8f3"))
 (focus :source "elpaca-menu-lock-file" :recipe
        (:package "focus" :fetcher github :repo "larstvei/Focus"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "a58e29e70948512dbcdbb24745e3fb0a59984925"))
 (free-keys :source "elpaca-menu-lock-file" :recipe
            (:package "free-keys" :fetcher github :repo
                      "Fuco1/free-keys" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "bed8e9c356c889cd98dd7a4a63c69d6c4960cf82"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (general :source "elpaca-menu-lock-file" :recipe
          (:package "general" :fetcher github :repo
                    "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :wait t :ref
                    "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (git-link :source "elpaca-menu-lock-file" :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "d9b375f79e6071a9926bf73bba64111adfc93bf5"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo
                      "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode
                                          gitignore-mode)
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "c3faeeea1982786f78d8c38397dec0f078eaec84"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo
                            "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t
                            :depth treeless :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (gotest :source "elpaca-menu-lock-file" :recipe
         (:package "gotest" :fetcher github :repo
                   "nlamirault/gotest.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth
                   treeless :ref
                   "490189e68d743a851bfb42d0017428a7550e8615"))
 (gotest-ts :source "elpaca-menu-lock-file" :recipe
            (:package "gotest-ts" :fetcher github :repo
                      "chmouel/gotest-ts.el" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "b12e08d925bab705792f14b29acdca9af550d9a8"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gptel :source "elpaca-menu-lock-file" :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "8879956648cc40e27c624ec6221971b7ab79d457"))
 (gptel-magit :source "elpaca-menu-lock-file" :recipe
              (:package "gptel-magit" :fetcher github :repo
                        "ragnard/gptel-magit" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "f27c01821b67ed99ddf705c2b995f78b71394d8b"))
 (gptel-quick :source "elpaca-menu-lock-file" :recipe
              (:source nil :protocol https :inherit t :depth treeless
                       :host github :repo "karthink/gptel-quick"
                       :package "gptel-quick" :ref
                       "018ff2be8f860a1e8fe3966eec418ad635620c38"))
 (grip-mode :source "elpaca-menu-lock-file" :recipe
            (:package "grip-mode" :repo "seagle0128/grip-mode"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "b8b9e603edbb258ab38a94a0518c4a8c7a22e53c"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hide-mode-line :source "elpaca-menu-lock-file" :recipe
                 (:package "hide-mode-line" :repo
                           "hlissner/emacs-hide-mode-line" :fetcher
                           github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t
                           :depth treeless :ref
                           "ddd154f1e04d666cd004bf8212ead8684429350d"))
 (highlight-defined :source "elpaca-menu-lock-file" :recipe
                    (:package "highlight-defined" :fetcher github
                              :repo "Fanael/highlight-defined" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit
                              t :depth treeless :ref
                              "4420bdda419875dacb065468aafe273b2022580e"))
 (highlight-indent-guides :source "elpaca-menu-lock-file" :recipe
                          (:package "highlight-indent-guides" :fetcher
                                    github :repo
                                    "DarthFennec/highlight-indent-guides"
                                    :files
                                    ("*.el" "*.el.in" "dir" "*.info"
                                     "*.texi" "*.texinfo" "doc/dir"
                                     "doc/*.info" "doc/*.texi"
                                     "doc/*.texinfo" "lisp/*.el"
                                     "docs/dir" "docs/*.info"
                                     "docs/*.texi" "docs/*.texinfo"
                                     (:exclude ".dir-locals.el"
                                               "test.el" "tests.el"
                                               "*-test.el"
                                               "*-tests.el" "LICENSE"
                                               "README*" "*-pkg.el"))
                                    :source "MELPA" :protocol https
                                    :inherit t :depth treeless :ref
                                    "802fb2eaf67ead730d7e3483b9a1e9639705f267"))
 (highlight-quoted :source "elpaca-menu-lock-file" :recipe
                   (:package "highlight-quoted" :fetcher github :repo
                             "Fanael/highlight-quoted" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "24103478158cd19fbcfb4339a3f1fa1f054f1469"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "9540fc414014822dde00f0188b74e17ac99e916d"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth
               treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hungry-delete :source "elpaca-menu-lock-file" :recipe
                (:package "hungry-delete" :fetcher github :repo
                          "nflath/hungry-delete" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "d919e555e5c13a2edf4570f3ceec84f0ade71657"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source "MELPA"
                  :protocol https :inherit t :depth treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (inflections :source "elpaca-menu-lock-file" :recipe
              (:package "inflections" :repo "eschulte/jump.el"
                        :fetcher github :files ("inflections.el")
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "55caa66a7cc6e0b1a76143fd40eff38416928941"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo
                       "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :host github :ref
                       "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (iqa :source "elpaca-menu-lock-file" :recipe
      (:package "iqa" :repo "a13/iqa.el" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth
                treeless :ref
                "b17ad22898ce36a60238668e28c9a48ba5f04f32"))
 (jinja2-mode :source "elpaca-menu-lock-file" :recipe
              (:package "jinja2-mode" :fetcher github :repo
                        "paradoxxxzero/jinja2-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1"))
 (jinx :source "elpaca-menu-lock-file" :recipe
       (:package "jinx" :repo "minad/jinx" :files
                 (:defaults "jinx-mod.c" "emacs-module.h") :fetcher
                 github :source "MELPA" :protocol https :inherit t
                 :depth treeless :ref
                 "75e8e4805fe6f4ab256bd59bec71464edbc23887"))
 (keycast :source "elpaca-menu-lock-file" :recipe
          (:package "keycast" :fetcher github :repo "tarsius/keycast"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "b831e380c4deb1d51ce5db0a965b96427aec52e4"))
 (kind-icon :source "elpaca-menu-lock-file" :recipe
            (:package "kind-icon" :repo
                      ("https://github.com/jdtsmith/kind-icon"
                       . "kind-icon")
                      :files ("*" (:exclude ".git")) :source
                      "GNU ELPA" :protocol https :inherit t :depth
                      treeless :ref
                      "556b0fb92aac24979b2c501431c7d48f75a5169f"))
 (ligature :source "elpaca-menu-lock-file" :recipe
           (:package "ligature" :fetcher github :repo
                     "mickeynp/ligature.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
 (link-hint :source "elpaca-menu-lock-file" :recipe
            (:package "link-hint" :fetcher github :repo
                      "noctuid/link-hint.el" :version-regexp
                      "none-since-rename" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "8fda5dcb9caff5a3c49d22b82e570ac9e29af7dd"))
 (list-environment :source "elpaca-menu-lock-file" :recipe
                   (:package "list-environment" :fetcher github :repo
                             "dgtized/list-environment.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "0a72a5a9c1abc090b25202a0387e3f766994b053"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "MELPA"
                  :protocol https :inherit t :depth treeless :ref
                  "2a89ba755b0459914a44b1ffa793e57f759a5b85"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "MELPA" :protocol https :inherit t
               :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "96d274457baea419fe7b3acbc955c8527d720024"))
 (magit-pre-commit :source "elpaca-menu-lock-file" :recipe
                   (:package "magit-pre-commit" :fetcher github :repo
                             "DamianB-BitFlipper/magit-pre-commit.el"
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "b1efec795c2d98dafba189b4311279685761d6d7"))
 (magit-prime :source "elpaca-menu-lock-file" :recipe
              (:package "magit-prime" :fetcher github :repo
                        "Azkae/magit-prime" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "c0d4d2b38c422d56ebd82fa9628b899dfd8ea09c"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "96d274457baea419fe7b3acbc955c8527d720024"))
 (magit-todos :source "elpaca-menu-lock-file" :recipe
              (:package "magit-todos" :fetcher github :repo
                        "alphapapa/magit-todos" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "7294a95580bddf7232f2d205efae312dc24c5f61"))
 (makefile-executor :source "elpaca-menu-lock-file" :recipe
                    (:package "makefile-executor" :repo
                              "Olivia5k/makefile-executor.el" :fetcher
                              github :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit
                              t :depth treeless :ref
                              "d1d98eaf522a767561f6c7cbd8d2526be58b3ec5"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "0d08fbea0f1182627891240780081ba528c1348b"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "9de2df5a9f2f864c82ec112d3369154767a2bb49"))
 (markdown-toc :source "elpaca-menu-lock-file" :recipe
               (:package "markdown-toc" :fetcher github :repo
                         "ardumont/markdown-toc" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t
                         :depth treeless :ref
                         "d22633b654193bcab322ec51b6dd3bb98dd5f69f"))
 (mason :source "elpaca-menu-lock-file" :recipe
        (:package "mason" :fetcher github :repo "deirn/mason.el"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "fe149182385e1f4c957c783e7c78fa6dc837809f"))
 (mcp :source "elpaca-menu-lock-file" :recipe
      (:package "mcp" :fetcher github :repo "lizqwerscott/mcp.el"
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth
                treeless :ref
                "2e947d2ddc8cbe655f846e23711e412d41f1bf6a"))
 (minions :source "elpaca-menu-lock-file" :recipe
          (:package "minions" :fetcher github :repo "tarsius/minions"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "5b73cd443c28a6e9c8e5ddd60ada38afdf40dfb9"))
 (mise :source "elpaca-menu-lock-file" :recipe
       (:package "mise" :fetcher github :repo "eki3z/mise.el" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "849c44b36594c80c57a555c5671dd1a5a83d3184"))
 (modus-themes :source "elpaca-menu-lock-file" :recipe
               (:package "modus-themes" :fetcher github :repo
                         "protesilaos/modus-themes" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t
                         :depth treeless :ref
                         "0ace30e471ce9db21590272624787272022f068c"))
 (monet :source "elpaca-menu-lock-file" :recipe
        (:source nil :protocol https :inherit t :depth treeless :host
                 github :repo "stevemolitor/monet" :package "monet"
                 :ref "72a18d372fef4b0971267bf13f127dcce681859a"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                             "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "ddd677091afc7d65ce56d11866e18aeded110ada"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo
                       "rainstormstudio/nerd-icons.el" :fetcher github
                       :files (:defaults "data") :source "MELPA"
                       :protocol https :inherit t :depth treeless :ref
                       "9a7f44db9a53567f04603bc88d05402cad49c64c"))
 (nerd-icons-completion :source "elpaca-menu-lock-file" :recipe
                        (:package "nerd-icons-completion" :repo
                                  "rainstormstudio/nerd-icons-completion"
                                  :fetcher github :files
                                  ("*.el" "*.el.in" "dir" "*.info"
                                   "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi"
                                   "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info"
                                   "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el"
                                             "test.el" "tests.el"
                                             "*-test.el" "*-tests.el"
                                             "LICENSE" "README*"
                                             "*-pkg.el"))
                                  :source "MELPA" :protocol https
                                  :inherit t :depth treeless :ref
                                  "d09ea987ed3d2cc64137234f27851594050e2b64"))
 (nerd-icons-corfu :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-corfu" :fetcher github :repo
                             "LuigiPiucco/nerd-icons-corfu" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "f821e953b1a3dc9b381bc53486aabf366bf11cb1"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo
                             "rainstormstudio/nerd-icons-dired"
                             :fetcher github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "929b62f01b93d30a3f42cc507fc45c84a2457b3f"))
 (nerd-icons-ibuffer :source "elpaca-menu-lock-file" :recipe
                     (:package "nerd-icons-ibuffer" :repo
                               "seagle0128/nerd-icons-ibuffer"
                               :fetcher github :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https
                               :inherit t :depth treeless :ref
                               "0cf63e4fa666cc9f3717e182f72342dca9f31f67"))
 (nerd-icons-xref :source "elpaca-menu-lock-file" :recipe
                  (:package "nerd-icons-xref" :fetcher github :repo
                            "hron/nerd-icons-xref" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t
                            :depth treeless :ref
                            "47db9ce08fe6514ddb36bdd714256f4e3985579a"))
 (ob-chatgpt-shell :source "elpaca-menu-lock-file" :recipe
                   (:package "ob-chatgpt-shell" :fetcher github :repo
                             "xenodium/ob-chatgpt-shell" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "0e592d19528f8f3283a93e0e2844299e9ea21fcc"))
 (olivetti :source "elpaca-menu-lock-file" :recipe
           (:package "olivetti" :fetcher github :repo "rnkn/olivetti"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "845eb7a95a3ca3325f1120c654d761b91683f598"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (org-appear :source "elpaca-menu-lock-file" :recipe
             (:package "org-appear" :fetcher github :repo
                       "awth13/org-appear" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
 (outline-indent :source "elpaca-menu-lock-file" :recipe
                 (:package "outline-indent" :fetcher github :repo
                           "jamescherti/outline-indent.el" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t
                           :depth treeless :ref
                           "344f18654e0111a64c779622fc76969a2427b251"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
               (:package "package-lint" :fetcher github :repo
                         "purcell/package-lint" :files
                         (:defaults "data" (:exclude "*flymake.el"))
                         :source "MELPA" :protocol https :inherit t
                         :depth treeless :ref
                         "1c37329703a507fa357302cf6fc29d4f2fe631a8"))
 (package-lint-flymake :source "elpaca-menu-lock-file" :recipe
                       (:package "package-lint-flymake" :fetcher
                                 github :repo "purcell/package-lint"
                                 :files ("package-lint-flymake.el")
                                 :source "MELPA" :protocol https
                                 :inherit t :depth treeless :ref
                                 "1c37329703a507fa357302cf6fc29d4f2fe631a8"))
 (page-break-lines :source "elpaca-menu-lock-file" :recipe
                   (:package "page-break-lines" :fetcher github :repo
                             "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "f54aa2b96f6ed249e103346cdb872c97c3c98054"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
                    "https://paredit.org/paredit.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "af075775af91f2dbc63b915d762b4aec092946c4"))
 (paren-face :source "elpaca-menu-lock-file" :recipe
             (:package "paren-face" :fetcher github :repo
                       "tarsius/paren-face" :files ("paren-face.el")
                       :old-names (parenface) :source "MELPA"
                       :protocol https :inherit t :depth treeless :ref
                       "2c279a236404b2eebacb435aa92d5e9c97939c03"))
 (parseclj :source "elpaca-menu-lock-file" :recipe
           (:package "parseclj" :repo "clojure-emacs/parseclj"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "6af22372e0fe14df882dd300b22b12ba2d7e00b0"))
 (parseedn :source "elpaca-menu-lock-file" :recipe
           (:package "parseedn" :repo "clojure-emacs/parseedn"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "3407e4530a367b6c2b857dae261cdbb67a440aaa"))
 (pcre2el :source "elpaca-menu-lock-file" :recipe
          (:package "pcre2el" :fetcher github :repo "joddie/pcre2el"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (persistent-scratch :source "elpaca-menu-lock-file" :recipe
                     (:package "persistent-scratch" :fetcher github
                               :repo "Fanael/persistent-scratch"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https
                               :inherit t :depth treeless :ref
                               "5ff41262f158d3eb966826314516f23e0cb86c04"))
 (plantuml-mode :source "elpaca-menu-lock-file" :recipe
                (:package "plantuml-mode" :fetcher github :repo
                          "skuro/plantuml-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "348e83ff193051d5ad332642100dd704f6e2a6d2"))
 (popon :source "elpaca-menu-lock-file" :recipe
        (:package "popon" :repo
                  ("https://codeberg.org/akib/emacs-popon" . "popon")
                  :files ("*" (:exclude ".git")) :source "NonGNU ELPA"
                  :protocol https :inherit t :depth treeless :ref
                  "bf8174cb7e6e8fe0fe91afe6b01b6562c4dc39da"))
 (proced-narrow :source "elpaca-menu-lock-file" :recipe
                (:package "proced-narrow" :repo
                          "travisjeffery/proced-narrow" :fetcher
                          github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref
                          "0e2a4dfb072eb0369d0020b429e820ae620d325e"))
 (project-tab-groups :source "elpaca-menu-lock-file" :recipe
                     (:package "project-tab-groups" :fetcher github
                               :repo "fritzgrabo/project-tab-groups"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https
                               :inherit t :depth treeless :ref
                               "2adc7d837c20e1a7c9f6655c9e1b1f365c011b4c"))
 (projection :source "elpaca-menu-lock-file" :recipe
             (:package "projection" :fetcher github :repo
                       "mohkale/projection" :files
                       (:defaults "src/*.el") :source "MELPA"
                       :protocol https :inherit t :depth treeless :ref
                       "482789397c5e11dbb95438c87ccd0cad3d37a33a"))
 (projection-multi :source "elpaca-menu-lock-file" :recipe
                   (:package "projection-multi" :fetcher github :repo
                             "mohkale/projection" :files
                             ("src/projection-multi/*.el") :source
                             "MELPA" :protocol https :inherit t :depth
                             treeless :ref
                             "482789397c5e11dbb95438c87ccd0cad3d37a33a"))
 (projection-multi-embark :source "elpaca-menu-lock-file" :recipe
                          (:package "projection-multi-embark" :fetcher
                                    github :repo "mohkale/projection"
                                    :files
                                    ("src/projection-multi-embark/projection-multi-embark*.el")
                                    :source "MELPA" :protocol https
                                    :inherit t :depth treeless :ref
                                    "482789397c5e11dbb95438c87ccd0cad3d37a33a"))
 (protobuf-ts-mode :source "elpaca-menu-lock-file" :recipe
                   (:package "protobuf-ts-mode" :fetcher github :repo
                             "emacsattic/protobuf-ts-mode" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "65152f5341ea4b3417390b3e60b195975161b8bc"))
 (queue :source "elpaca-menu-lock-file" :recipe
        (:package "queue" :repo
                  ("https://github.com/emacsmirror/gnu_elpa" . "queue")
                  :branch "externals/queue" :files
                  ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
                  https :inherit t :depth treeless :ref
                  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (recall :source "elpaca-menu-lock-file" :recipe
         (:package "recall" :fetcher github :repo "svaante/recall"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth
                   treeless :ref
                   "a8f961e9a5d6b609ee1934a0ae68ed003ee4987b"))
 (reverse-im :source "elpaca-menu-lock-file" :recipe
             (:package "reverse-im" :repo "a13/reverse-im.el" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "20d5f0514a761f0a06284b2adf0baf4bf7b93db2"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth
              treeless :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (sesman :source "elpaca-menu-lock-file" :recipe
         (:package "sesman" :repo "vspinu/sesman" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth
                   treeless :ref
                   "7bca68dbbab0af26a6a23be1ff5fa97f9a18e022"))
 (shackle :source "elpaca-menu-lock-file" :recipe
          (:package "shackle" :fetcher git :url
                    "https://depp.brause.cc/shackle.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "ae25e7e0e593520c8590440fe5e3c0ea8053dc26"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
              (:package "shell-maker" :fetcher github :repo
                        "xenodium/shell-maker" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "a7ff78f8cd29fba9a694b8d7bbee448c7a51472d"))
 (show-font :source "elpaca-menu-lock-file" :recipe
            (:package "show-font" :repo
                      ("https://github.com/protesilaos/show-font"
                       . "show-font")
                      :files
                      ("*"
                       (:exclude ".git" "COPYING" "doclicense.texi"))
                      :source "GNU ELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "0fb3b3a7dbec93345c7135847b467c32cc45cbfa"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
              (:package "shrink-path" :fetcher gitlab :repo
                        "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t
                        :depth treeless :ref
                        "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (sideline :source "elpaca-menu-lock-file" :recipe
           (:package "sideline" :repo "emacs-sideline/sideline"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "b4ada1ddf7da96c2e87453130f6ff7b7d577810f"))
 (sideline-flymake :source "elpaca-menu-lock-file" :recipe
                   (:package "sideline-flymake" :repo
                             "emacs-sideline/sideline-flymake"
                             :fetcher github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit
                             t :depth treeless :ref
                             "ad296589a97ad7225f3f1e9519443577b2c5a49b"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :files ("*" (:exclude ".git")) :source "GNU ELPA"
                    :protocol https :inherit t :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (ssh-config-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "ssh-config-mode" :fetcher github :repo
                            "peterhoeg/ssh-config-mode-el" :files
                            (:defaults "*.txt") :source "MELPA"
                            :protocol https :inherit t :depth treeless
                            :ref
                            "f21726d6f44a0e769a15f0a94620078a326774f7"))
 (standard-themes :source "elpaca-menu-lock-file" :recipe
                  (:package "standard-themes" :repo
                            ("https://github.com/protesilaos/standard-themes"
                             . "standard-themes")
                            :files
                            ("*"
                             (:exclude ".git" "COPYING"
                                       "doclicense.texi"))
                            :source "GNU ELPA" :protocol https
                            :inherit t :depth treeless :ref
                            "b528704f79bdd3146a58e0dc7e0a24364b2fa5f3"))
 (string-inflection :source "elpaca-menu-lock-file" :recipe
                    (:package "string-inflection" :fetcher github
                              :repo "akicho8/string-inflection" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit
                              t :depth treeless :ref
                              "4a2f87d7b47f5efe702a78f8a40a98df36eeba13"))
 (svg-lib :source "elpaca-menu-lock-file" :recipe
          (:package "svg-lib" :repo
                    ("https://github.com/rougier/svg-lib" . "svg-lib")
                    :files ("*" (:exclude ".git")) :source "GNU ELPA"
                    :protocol https :inherit t :depth treeless :ref
                    "925ed4a0215c197ba836e7810a93905b34bea777"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo
                    "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (tempel :source "elpaca-menu-lock-file" :recipe
         (:package "tempel" :repo "minad/tempel" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth
                   treeless :ref
                   "8d9a1646fb4014a5d34f3f19b7c894310f06a1e8"))
 (tempel-collection :source "elpaca-menu-lock-file" :recipe
                    (:package "tempel-collection" :repo
                              "Crandel/tempel-collection" :fetcher
                              github :files (:defaults "templates")
                              :source "MELPA" :protocol https :inherit
                              t :depth treeless :ref
                              "fb5759fbaadde45dd55c2c84dae3ead17212f7e0"))
 (toc-org :source "elpaca-menu-lock-file" :recipe
          (:package "toc-org" :fetcher github :repo "snosov1/toc-org"
                    :old-names (org-toc) :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth
                    treeless :ref
                    "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd"))
 (track-changes :source "elpaca-menu-lock-file" :recipe
                (:package "track-changes" :repo
                          ("https://github.com/emacs-mirror/emacs"
                           . "track-changes")
                          :branch "master" :files
                          ("lisp/emacs-lisp/track-changes.el"
                           (:exclude ".git"))
                          :source "GNU ELPA" :protocol https :inherit
                          t :depth treeless :ref
                          "f84fb38a8241e94efdc2c191084822e74a57f806"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "1f7039ef8d548d6fe858084fcbeae7588eba4190"))
 (try :source "elpaca-menu-lock-file" :recipe
      (:package "try" :fetcher github :repo "larstvei/Try" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth
                treeless :ref
                "8831ded1784df43a2bd56c25ad3d0650cdb9df1d"))
 (ultra-scroll :source "elpaca-menu-lock-file" :recipe
               (:package "ultra-scroll" :fetcher github :repo
                         "jdtsmith/ultra-scroll" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t
                         :depth treeless :ref
                         "08758c6772c5fbce54fb74fb5cce080b6425c6ce"))
 (undo-fu-session :source "elpaca-menu-lock-file" :recipe
                  (:package "undo-fu-session" :fetcher codeberg :repo
                            "ideasman42/emacs-undo-fu-session" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t
                            :depth treeless :ref
                            "92d733a5b162a70c572fac17b9f9e872426df547"))
 (verb :source "elpaca-menu-lock-file" :recipe
       (:package "verb" :repo "federicotdn/verb" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "f45e31b2bcdea2a859bb28cbb1819469978457c9"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "MELPA" :protocol https :inherit t
                    :depth treeless :ref
                    "93f15873d7d6244d72202c5dd7724a030a2d5b9a"))
 (vimrc-mode :source "elpaca-menu-lock-file" :recipe
             (:package "vimrc-mode" :fetcher github :repo
                       "mcandre/vimrc-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "f594392a0834193a1fe1522d007e1c8ce5b68e43"))
 (vterm :source "elpaca-menu-lock-file" :recipe
        (:package "vterm" :fetcher github :repo
                  "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h"
                   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
                   "vterm-module.c" "vterm-module.h")
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "a01a2894a1c1e81a39527835a9169e35b7ec5dec"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo
                  ("https://github.com/casouri/vundo" . "vundo")
                  :files ("*" (:exclude ".git" "test")) :source
                  "GNU ELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (web-server :source "elpaca-menu-lock-file" :recipe
             (:package "web-server" :fetcher github :repo
                       "eschulte/emacs-web-server" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t
                       :depth treeless :ref
                       "6357a1c2d1718778503f7ee0909585094117525b"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "03d1cca4bd910a8df73e4ec637836c6ac25213a2"))
 (winum :source "elpaca-menu-lock-file" :recipe
        (:package "winum" :fetcher github :repo "deb0ch/emacs-winum"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth
                  treeless :ref
                  "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth
             treeless :ref "902b4d572af2c2f36060da01e3c33d194cdec32b"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "f2369fb4985ed054be47ae111760ff2075dff72a"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t
                      :depth treeless :ref
                      "d91f878729312a6beed77e6637c60497c5786efa"))
 (yaml-pro :source "elpaca-menu-lock-file" :recipe
           (:package "yaml-pro" :repo "zkry/yaml-pro" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth
                     treeless :ref
                     "9b9509188e5b88bb933e98ab36ab992519b9554b"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet"
                      :fetcher github :files
                      ("yasnippet.el" "snippets") :source "MELPA"
                      :protocol https :inherit t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (zoom :source "elpaca-menu-lock-file" :recipe
       (:package "zoom" :repo "cyrus-and/zoom" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref
                 "36f9db90941b10d34bac976aee35dfe25242cd03")))
