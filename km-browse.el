;;; km-browse.el --- Misc browse utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-browse
;; Version: 0.1.0
;; Keywords: hypermedia
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Misc browse utils

;;; Code:


(require 'url)
(require 'url-http)

(declare-function ansi-color-apply-on-region "ansi-color")

(defcustom km-browse-search-engine "http://www.google.com/search?q=%s"
  "Default search engine."
  :type 'url
  :group 'km-browse)

(defcustom km-browse-pandoc-options '("--quiet"
                                      "--self-contained"
                                      "--wrap=none")
  "Extra pandoc options."
  :group 'km-browse
  :type '(repeat string))


(defvar km-browse-url-re
  "\\(\\(http[s]?://\\|git@\\|file:/~*\\)*\\(www\\.\\)?\\([a-z0-9-]+\\(\\(:[0-9]*\\)\\|\\.[a-z]+\\)+/?[a-z]?[^;\s\t\n\r\f|\\]]*[a-z0-9-]+\\)\\)"
  "Url regexp.")

(defvar km-browse-chrome-history-file nil
  "Chrome history SQLite database file.")

(defvar km-browse-chrome-history-hash (make-hash-table :test 'equal))
(defvar km-browse-chrome-bookmarks-hash (make-hash-table :test 'equal))
(defvar km-browse-eww-bookmarks-hash (make-hash-table :test 'equal))
(defvar km-browse-chrome-history-hash (make-hash-table :test 'equal))

(defvar km-browse-minibuffer-history nil)


(defvar km-browse-multi-source-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") #'km-browse-multi-source-select-next)
    (define-key map (kbd "C-<") #'km-browse-multi-source-select-prev)
    (define-key map (kbd "C-.") #'km-browse-multi-source-read-source)
    map)
  "Keymap to use in minibuffer.")

(defcustom km-browse-multi-source-restore-last-input t
  "Whether to insert last typed source input."
  :group 'km-browse-multi-source
  :type 'boolean)

(defvar km-browse-multi-source-last-input nil
  "Last typed input in minibuffer.")

(defvar km-browse-multi-source--current-index nil
  "Index of active source.")

(defvar km-browse-multi-source--sources-list nil
  "Normalized sources.")

(defun km-browse-multi-source-select-prev ()
  "Select the previous item in a multi-source browser."
  (interactive)
  (throw 'next
         -1))

(defun km-browse-multi-source-select-next ()
  "Throw to the catch tag ='next with 1."
  (interactive)
  (throw 'next
         1))


(defun km-browse-multi-source-read-source ()
  "Prompt for a source and calculate its relative position."
  (interactive)
  (let* ((source-label
          (completing-read "Source: "
                           (mapcar #'car km-browse-multi-source--sources-list)))
         (pos (seq-position (nth 1 km-browse-multi-source--sources-list)
                            source-label)))
    (throw 'next (- pos km-browse-multi-source--current-index))))

(defun km-browse-multi-source-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))
        (t current-index)))

(defun km-browse-multi-source-set-last-input ()
  "Save last typed input in mininubbfer."
  (when (minibufferp)
    (setq km-browse-multi-source-last-input
          (buffer-substring (minibuffer-prompt-end)
                            (point)))))

(defun km-browse-multi-source-map-sources (sources)
  "Normalize SOURCES to list of functions, labels and arguments."
  (let ((curr)
        (labels)
        (args)
        (fns))
    (while (setq curr (pop sources))
      (pcase curr
        ((pred stringp)
         (let ((fn (pop sources)))
           (push curr labels)
           (push fn fns)
           (push nil args)))
        ((pred functionp)
         (let ((label
                (if (symbolp curr)
                    (symbol-name curr)
                  "")))
           (push label labels)
           (push curr fns)
           (push nil args)))
        ((pred listp)
         (let* ((label (car curr))
                (rest (cdr curr))
                (fn (if (listp rest)
                        (car rest)
                      rest))
                (extra-args (when (listp rest)
                              (cdr rest))))
           (push label labels)
           (push (if (or (functionp fn)
                         (symbolp fn))
                     fn
                   `(lambda () ,fn))
                 fns)
           (push extra-args args)))))
    (list (reverse fns)
          (reverse labels)
          (reverse args))))

(defun km-browse-multi-source-read (sources)
  "Combine minibuffer SOURCES into a command with several alternatives.

Every alternative should be a function that reads data from minibuffer.

By default the first source is called and user can switch between
alternatives dynamically with commands:

 `km-browse-multi-source-select-next'
 \(bound to \\<km-browse-multi-source-minibuffer-map>\
`\\[km-browse-multi-source-select-next]') - select next alternative.
 `km-browse-multi-source-select-prev'
\(bound to \\<km-browse-multi-source-minibuffer-map>\
`\\[km-browse-multi-source-select-prev]') - select previus alternative.
 `km-browse-multi-source-read-source'
\(bound to \\<km-browse-multi-source-minibuffer-map>\
`\\[km-browse-multi-source-read-source]') - select from completions list.

Allowed forms for SOURCES are
 - a list of functions
 - a plist of backend's name and corresponding function,
-  an alist of backend's name, corresponding function and optionally extra
 arguments to pass."
  (setq km-browse-multi-source--sources-list (km-browse-multi-source-map-sources
                                              sources))
  (setq km-browse-multi-source--current-index 0)
  (setq km-browse-multi-source-last-input nil)
  (let ((curr)
        (fns (nth 0 km-browse-multi-source--sources-list))
        (args (nth 2 km-browse-multi-source--sources-list)))
    (while
        (numberp
         (setq curr
               (catch 'next
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (use-local-map
                        (let ((map
                               (copy-keymap
                                km-browse-multi-source-minibuffer-map)))
                          (set-keymap-parent map (current-local-map))
                          map))
                       (when (minibufferp)
                         (when km-browse-multi-source-restore-last-input
                           (if (not km-browse-multi-source-last-input)
                               (setq km-browse-multi-source-last-input
                                     (buffer-substring-no-properties
                                      (minibuffer-prompt-end)
                                      (line-end-position)))
                             (delete-region (minibuffer-prompt-end)
                                            (line-end-position))
                             (insert
                              km-browse-multi-source-last-input)))
                         (add-hook
                          'post-self-insert-hook
                          #'km-browse-multi-source-set-last-input
                          nil t)))
                   (apply (nth km-browse-multi-source--current-index fns)
                          (nth km-browse-multi-source--current-index args))))))
      (setq km-browse-multi-source--current-index
            (km-browse-multi-source-switcher curr
                                             km-browse-multi-source--current-index
                                             fns)))
    (setq km-browse-multi-source-last-input nil)
    (setq km-browse-multi-source--sources-list nil)
    curr))

(defun km-browse-chrome-guess-config-file ()
  "Guess the newest Chrome config file path."
  (car
   (seq-sort
    #'file-newer-than-file-p
    (seq-filter
     #'file-exists-p
     `("~/Library/Application Support/Google/Chrome"
       "~/Library/Application Support/Google/Chrome"
       "~/AppData/Local/Google/Chrome"
       "~/snap/chromium"
       "~/.config/google-chrome"
       "~/.config/chromium"
       ,(substitute-in-file-name
         "$LOCALAPPDATA/Google/Chrome/")
       ,(substitute-in-file-name
         "$USERPROFILE/Local Settings/Application Data/Google/Chrome/"))))))

(defun km-browse-chrome-guess-history-file ()
  "Guess and return the newest Chrome history file path."
  (car
   (seq-sort
    #'file-newer-than-file-p
    (seq-filter
     #'file-exists-p
     `("~/Library/Application Support/Google/Chrome/Profile 1/History"
       "~/Library/Application Support/Google/Chrome/Default/History"
       "~/AppData/Local/Google/Chrome/User Data/Default/History"
       "~/snap/chromium/common/chromium/Default/History"
       "~/.config/google-chrome/Default/History"
       "~/.config/chromium/Default/History"
       ,(substitute-in-file-name
         "$LOCALAPPDATA/Google/Chrome/User Data/Default/History")
       ,(substitute-in-file-name
         "$USERPROFILE/Local Settings/Application Data/Google/Chrome/User Data/Default/History"))))))


(defun km-browse-open-ports (&optional service)
  "Return alist of open ports and services with `nmap' program.
Optional argument SERVICE is a filter."
  (let* ((lines (split-string
                 (shell-command-to-string
                  "nmap localhost | grep [0-9]*/tcp")
                 "[\n]" t))
         (filtered-lines (if service
                             (seq-filter (lambda (it)
                                           (equal service
                                                  (car (last (split-string
                                                              it)))))
                                         lines)
                           lines)))
    (mapcar (lambda (it)
              (let ((parts (split-string it "[/\s\t\n]")))
                (cons (car parts)
                      (car (reverse parts)))))
            filtered-lines)))

;;;###autoload
(defun km-browse-print-open-ports ()
  "Display open ports on localhost using nmap in a new buffer."
  (interactive)
  (require 'ansi-color)
  (let* ((mini-wind (minibuffer-selected-window))
         (buff-name (concat "*km-browse-nmap*"))
         (buff (progn
                 (when (get-buffer buff-name)
                   (kill-buffer buff-name))
                 (get-buffer-create buff-name)))
         (proc
          (start-process buff-name buff "nmap" "localhost")))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (set-process-sentinel
     proc
     (lambda (process _)
       (let ((buff (process-buffer process)))
         (when (and (buffer-live-p buff)
                    (or (not mini-wind)
                        (eq mini-wind
                            (minibuffer-selected-window))))
           (with-current-buffer buff
             (ansi-color-apply-on-region (point-min)
                                         (point-max))
             (with-selected-window (km-browse-window-right-get-or-create)
               (pop-to-buffer-same-window buff)))))))
    (set-process-filter
     proc
     (lambda (proc string)
       (when-let* ((buf (process-buffer proc)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (point-max))
               (insert string)))))))
    proc))


(defun km-browse-convert-webkit-to-date-in-seconds (webkit-timestamp)
  "Convert WEBKIT-TIMESTAMP to date in seconds."
  (when (stringp webkit-timestamp)
    (setq webkit-timestamp (string-to-number webkit-timestamp)))
  (let ((date-in-seconds (- (round (/ webkit-timestamp 1 1000000))
                            11644473600)))
    date-in-seconds))

(defun km-browse-chrome-guess-bookmarks-file ()
  "Return the most newer chrome bookmarks file."
  (car
   (seq-sort
    #'file-newer-than-file-p
    (seq-filter
     #'file-exists-p
     `("~/Library/Application Support/Google/Chrome/Profile 1/Bookmarks"
       "~/Library/Application Support/Google/Chrome/Default/Bookmarks"
       "~/AppData/Local/Google/Chrome/User Data/Default/Bookmarks"
       "~/snap/chromium/common/chromium/Default/Bookmarks"
       "~/.config/google-chrome/Default/Bookmarks"
       "~/.config/chromium/Default/Bookmarks"
       ,(substitute-in-file-name
         "$LOCALAPPDATA/Google/Chrome/User Data/Default/Bookmarks")
       ,(substitute-in-file-name
         "$USERPROFILE/Local Settings/Application Data/Google/Chrome/User Data/Default/Bookmarks"))))))



(defun km-browse-read-chrome-bookmarks ()
  "Read chrome bookmarks and store result in `km-browse-chrome-bookmarks-hash'.
Return hashkeys sorted by time."
  (when-let* ((file (km-browse-chrome-guess-bookmarks-file))
              (pl (when (file-exists-p file)
                    (if (and (fboundp 'json-parse-string)
                             (fboundp 'json-available-p)
                             (json-available-p))
                        (json-parse-string (with-temp-buffer (insert-file-contents
                                                              file)
                                                             (buffer-string))
                                           :object-type 'plist
                                           :array-type 'list)
                      (require 'json)
                      (when (fboundp 'json-read-file)
                        (let ((json-object-type 'plist)
                              (json-array-type 'list))
                          (json-read-file
                           file))))))
              (roots (seq-filter #'listp (cdr (plist-get pl :roots)))))
    (let ((children)
          (current))
      (while (setq current (pop roots))
        (if (and current (listp current)
                 (plist-get current :children))
            (setq roots (append roots (plist-get current :children)))
          (setq children (push current children))))
      (setq children
            (delete nil
                    (mapcar
                     (lambda (it)
                       (let ((url (plist-get it :url))
                             (title (plist-get it :name))
                             (meta (plist-get it :meta_info))
                             (time (plist-get it :date_added))
                             (protocol)
                             (port)
                             (host)
                             (urlobj)
                             (hash-key)
                             (result))
                         (when meta
                           (setq title (or title (plist-get meta :url)))
                           (setq url (or url (plist-get meta :name)))
                           (setq time (or time (plist-get meta :date_added))))
                         (when url
                           (setq urlobj (url-generic-parse-url url))
                           (setq protocol (url-type urlobj))
                           (setq port (url-port-if-non-default urlobj))
                           (setq host (url-host urlobj))
                           (setq hash-key
                                 url)
                           (setq result `(:title ,title
                                                 :url ,url
                                                 :time ,time
                                                 :host ,host
                                                 :hash-key ,hash-key
                                                 :protocol ,protocol
                                                 :port ,port))
                           (puthash hash-key result
                                    km-browse-chrome-bookmarks-hash))
                         result))
                     children)))
      (mapcar (lambda (it)
                (plist-get it :hash-key))
              (seq-sort-by
               (lambda (it)
                 (km-browse-convert-webkit-to-date-in-seconds
                  (plist-get it :time)))
               #'> children)))))

(defun km-browse-get-region ()
  "Return current active region as string or nil."
  (when (and (region-active-p)
             (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning)
                  (region-end)))))

(defun km-browse-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Return stdout output if command existed with zero status, nil otherwise."
  (let ((buff (generate-new-buffer command)))
    (with-current-buffer buff
      (let ((status (apply #'call-process command nil t nil
                           (flatten-list args))))
        (let ((result (string-trim (buffer-string))))
          (if (= 0 status)
              (prog1 result (kill-current-buffer))
            (message result) nil))))))

(defun km-browse-url-to-localhost (url &optional port)
  "Replace hostname in URL to localhost with PORT (default value is 3000)."
  (let ((url-struct (url-generic-parse-url url)))
    (url-recreate-url (url-parse-make-urlobj "http"
                                             (url-user url-struct)
                                             (url-password url-struct)
                                             "localhost"
                                             (if (stringp port)
                                                 (string-to-number port)
                                               (or port 3000))
                                             (url-filename url-struct)
                                             (url-target url-struct)
                                             (url-attributes url-struct)
                                             (url-fullness url-struct)))))


(defun km-browse-format-to-google-search (string)
  "Return url with URI-encoded STRING to search in google."
  (format km-browse-search-engine (url-hexify-string string)))
(defun km-browse-web-url-p (str)
  "Check if STR is url, possible without protocol."
  (and str
       (not (string-match-p "[\s\t\n;]\\|\\\\[(]" str))
       (string-match-p "^[a-z0-9-]" str)
       (if-let* ((m (string-match-p "http[s]?:" str)))
           (equal m 0)
         t)
       (string-match-p km-browse-url-re
                       (string-trim str) 0)))

(defun km-browse-make-chrome-history-item (item-list)
  "Convert ITEM-LIST to plist and store it to `km-browse-chrome-history-hash'."
  (let* ((url (string-trim (car item-list)))
         (title (nth 1 item-list))
         (time (and (nth 2 item-list)))
         (urlobj (url-generic-parse-url url))
         (hash-key url)
         (pl `(:url ,url
                    :title ,title
                    :time
                    ,time
                    :protocol ,(url-type urlobj)
                    :port ,(url-port-if-non-default urlobj)
                    :host ,(url-host urlobj))))
    (puthash hash-key pl km-browse-chrome-history-hash)
    url))

(defun km-browse-group-urls (urls)
  "Group URLS by url type and host."
  (seq-group-by (lambda (it)
                  (let ((obj (url-generic-parse-url it)))
                    (concat (url-type obj)
                            "://"
                            (url-host obj))))
                (copy-tree urls)))
(defun km-browse-all-urls ()
  "Return all known urls."
  (seq-uniq
   (append
    (km-browse-urls-from-buffer-and-kill-ring)
    (km-browse-read-chrome-bookmarks)
    (km-browse-chrome-session-dump-get-active-tabs)
    (km-browse-init-chrome-history-candidates))))

(defun km-browse-all-urls-groupped-alist ()
  "Return all known urls urls groupped by url type and hostname."
  (km-browse-group-urls
   (km-browse-all-urls)))


(defun km-browse-read-chrome-history ()
  "Read `km-browse-chrome-history-file'."
  (unless km-browse-chrome-history-file
    (setq km-browse-chrome-history-file
          (km-browse-chrome-guess-history-file)))
  (when (or (null km-browse-chrome-history-file)
            (not (file-exists-p km-browse-chrome-history-file)))
    (user-error "'%s' doesn't exist, reset `km-browse-chrome-history-file'"
                km-browse-chrome-history-file))
  (with-temp-buffer
    (erase-buffer)
    (let ((tmp (make-temp-name
                (expand-file-name (temporary-file-directory)
                                  "km-browse-chrome-history"))))
      (copy-file
       km-browse-chrome-history-file tmp)
      (if
          (zerop
           (call-process
            "sqlite3" nil t nil
            "-ascii"
            tmp
            "SELECT url, title, last_visit_time FROM urls ORDER BY last_visit_time DESC"))
          (let (result)
            (goto-char (point-min)) ;; -ascii delimited by 0x1F and 0x1E
            (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
              (let ((parts (split-string (match-string 1) "\x1f")))
                (push (km-browse-make-chrome-history-item parts) result)))
            (delete-file tmp)
            (nreverse result))
        (error "Command sqlite3 failed: %s" (buffer-string))))))

(defun km-browse-init-chrome-history-candidates ()
  "Build chrome history candidates."
  (require 'subr-x)
  (km-browse-read-chrome-history)
  (reverse (hash-table-keys km-browse-chrome-history-hash)))

(defun km-browse-format-webkit-timestamp (webkit-timestamp &optional format-str)
  "Convert WEBKIT-TIMESTAMP to FORMAT-STR."
  (when (stringp webkit-timestamp)
    (setq webkit-timestamp (string-to-number webkit-timestamp)))
  (let ((date-in-seconds (- (round (/ webkit-timestamp 1 1000000))
                            11644473600)))
    (format-time-string (or format-str "%a %b %d %H:%M:%S %Y")
                        (seconds-to-time date-in-seconds))))

(defun km-browse-get-hash (key)
  "Look up KEY in the chrome or eww bookmarks or history.
See variables `km-browse-chrome-bookmarks-hash', `km-browse-eww-bookmarks-hash'
or `km-browse-chrome-history-hash'."
  (or (gethash key km-browse-chrome-bookmarks-hash)
      (gethash key km-browse-eww-bookmarks-hash)
      (gethash key km-browse-chrome-history-hash)))


(defun km-browse-change-localhost-port (localhost-str)
  "Change port in LOCALHOST-STR to currently open port.
If a several open ports found, read it with comletion.
LOCALHOST-STR should match \"localhost:\"."
  (unless (string-match-p "^https?://" localhost-str)
    (setq localhost-str (concat "http://" localhost-str)))
  (let* ((ports (km-browse-get-nodejs-open-ports))
         (alist (append
                 (list (cons (km-browse-url-to-localhost
                              localhost-str)
                             "(Exact)"))
                 (mapcar (lambda (p)
                           (cons
                            (km-browse-url-to-localhost
                             localhost-str
                             p)
                            "(Active)"))
                         ports)))
         (annotf
          (lambda (str)
            (format " %s"
                    (cdr
                     (assoc
                      str
                      alist))))))
    (completing-read "Browse: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function .
                                                  ,annotf))
                         (complete-with-action
                          action alist str pred))))))

(defun km-browse-format-to-url (string)
  "If STRING is valid url return it otherwise format to google search url."
  (if (and (not (km-browse-web-url-p string))
           (not (file-exists-p string)))
      (km-browse-format-to-google-search string)
    (cond ((string-match-p "^\\(file|chrome\\):/" string)
           string)
          ((string-match-p "^git@" string)
           string)
          ((string-match-p "^\\(http://\\)?localhost:" string)
           (if (ignore-errors (url-http-file-exists-p string))
               string
             (km-browse-change-localhost-port string)))
          ((and (not (string-empty-p string))
                (file-exists-p string))
           (concat "file://" string))
          ((string-match-p "^https?://" string)
           string)
          (t
           (let ((url (if (string-match-p "^https?://" string)
                          string
                        (concat "https://" string)))
                 (exist))
             (condition-case
                 nil
                 (setq exist
                       (url-http-file-exists-p
                        (with-temp-buffer
                          (erase-buffer)
                          (insert url)
                          (goto-char (point-min))
                          (re-search-forward "://" nil t 1)
                          (re-search-forward "/" nil t 1)
                          (backward-char 1)
                          (let ((end (point)))
                            (skip-chars-backward "^[\s\t\n]")
                            (buffer-substring-no-properties
                             (point) end)))))
               (error (setq exist nil)))
             (if exist url (km-browse-format-to-google-search
                            string)))))))

(defun km-browse-action-default (url)
  "Browse URL with function `browse-url-generic'."
  (require 'browse-url)
  (if-let* ((wind (seq-find (lambda (w)
                             (with-selected-window w
                               (memq major-mode '(xwidget-webkit-mode))))
                           (window-list))))
      (with-selected-window wind
        (browse-url url))
    (browse-url url)))

(defun km-browse-get-nodejs-open-ports ()
  "Return list of integers which are open ports for nodejs program."
  (mapcar (lambda (it)
            (when-let* ((local-addr (seq-find (apply-partially #'string-match-p
                                                              ":[0-9]+")
                                             (split-string it "[\s\t]" t))))
              (string-to-number (car (last (split-string local-addr ":" t))))))
          (seq-filter
           (lambda (it)
             (and (string-match-p "\\_<\\([0-9]+/node\\)\\_>" it)))
           (split-string (km-browse-call-process "netstat"
                                                 "-tlnp")
                         "\n" t))))

(defun km-browse-localhost (url)
  "Replace host in URL to localhost and browse it."
  (let ((new-url (km-browse-url-to-localhost url
                                             (or
                                              (when-let*
                                                  ((ports
                                                    (km-browse-get-nodejs-open-ports)))
                                                (if (> (length ports) 1)
                                                    (completing-read
                                                     "Port" ports)
                                                  (car ports)))
                                              3000))))
    (km-browse-action-default new-url)))

(defun km-browse-annotate (hashkey)
  "Convert HASHKEY to host annotated with title and time."
  (if-let* ((pl (km-browse-get-hash hashkey)))
      (let ((title (or (plist-get pl :title)
                       (plist-get pl :host)))
            (time (plist-get pl :time)))
        (propertize
         (concat (propertize " " 'display '(space :align-to 80)) title " "
                 (or
                  (when time
                    (km-browse-format-webkit-timestamp
                     time
                     "%D"))
                  ""))
         'face 'completions-annotations))
    ""))

(defun km-browse-chrome-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun km-browse-chrome-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (km-browse-chrome-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun km-browse-chrome-minibuffer-auto-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (km-browse-chrome-get-metadata) 'category)
       all))))

(defun km-browse-chrome-default-top-minibuffer-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target.

This target finder is meant for the default completion UI and
completion UI highly compatible with it, like Icomplete.
Many completion UIs can still work with Embark but will need
their own target finder.  See for example
`embark--vertico-selected' or `embark--selectrum-selected'."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (km-browse-chrome-minibuffer-auto-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar km-browse-chrome-targets-finders
  '(km-browse-chrome-ivy-selected-cand
    km-browse-chrome-default-top-minibuffer-completion))

(defun km-browse-chrome-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'km-browse-chrome-targets-finders
     (lambda (fun)
       (when-let* ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun km-browse-chrome-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (km-browse-chrome-get-current-candidate)))
    (progn
      (run-with-timer 0 nil action current)
      (abort-minibuffers))))

(defun km-browse-web-restore-completions-wind ()
  "Restore *Completions* window height."
  (when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook #'km-browse-web-restore-completions-wind)
    (when-let* ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun km-browse-chrome-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (km-browse-chrome-get-current-candidate)))
    (when-let* ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook #'km-browse-web-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))

(defun km-browse-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of BUFFER-LIST with `major-mode' listed in MODES.
MODES can be either list of modes, or a mode.

If DERIVED-P is non-nil, test with `derived-mode-p', otherwise use `eq'."
  (unless (proper-list-p modes)
    (setq modes (list modes)))
  (seq-filter (if derived-p
                  (lambda (buf)
                    (apply #'provided-mode-derived-p
                           (buffer-local-value 'major-mode buf)
                           modes))
                (lambda (buf)
                  (memq (buffer-local-value 'major-mode buf) modes)))
              (or buffer-list (buffer-list))))

(defun km-browse-window-right-get-or-create ()
  "Return right window or split current window.
Minibuffer and completions windows are ignored."
  (let ((exclude-winds (delete
                        nil
                        (list (get-buffer-window "*Completions*" 0)
                              (active-minibuffer-window)))))
    (cond ((= 1 (length (seq-difference (window-list) exclude-winds)))
           (if (active-minibuffer-window)
               (with-minibuffer-selected-window
                 (split-window-right))
             (split-window-right)))
          (t
           (if (active-minibuffer-window)
               (with-minibuffer-selected-window
                 (or (window-right (selected-window))
                     (when (window-left (selected-window))
                       (selected-window))))
             (or (window-right (selected-window))
                 (when (window-left (selected-window))
                   (selected-window))))))))

(defun km-browse-with-xwidget-or-fallback-browse (url)
  "Try to browse URL with `eaf-open-browser', `xwidget' or `eww'."
  (cond ((not (display-graphic-p))
         (browse-url url))
        ((fboundp 'eaf-open-browser)
         (eaf-open-browser url))
        ((featurep 'xwidget-internal)
         (require 'xwidget)
         (cond ((and (boundp 'xwidget-webkit-last-session-buffer)
                     (bufferp xwidget-webkit-last-session-buffer)
                     (buffer-live-p xwidget-webkit-last-session-buffer))
                (pop-to-buffer-same-window xwidget-webkit-last-session-buffer)
                (with-current-buffer xwidget-webkit-last-session-buffer
                  (when url
                    (xwidget-webkit-browse-url url))))
               (t (xwidget-webkit-browse-url url))))
        (t
         (require 'eww)
         (let ((buff (km-browse-buffers-in-mode 'eww-mode)))
           (cond ((and buff url)
                  (with-current-buffer buff
                    (eww-browse-url url)
                    (pop-to-buffer-same-window (current-buffer) 'norecord)))
                 ((and buff)
                  (pop-to-buffer-same-window buff))
                 (t
                  (eww-browse-url
                   url)))))))

(defun km-browse-xwidget-in-other-window (&optional fn &rest args)
  "Apply FN with ARGS in left or right window.
If windows doesn't exists, split current window."
  (select-window
   (if-let* ((xwidget-buff (car (km-browse-buffers-in-mode 'xwidget-webkit-mode
                                                          (delete-dups
                                                           (mapcar
                                                            #'window-buffer
                                                            (window-list)))))))
       (get-buffer-window xwidget-buff)
     (let ((other-wind (or (if (and (active-minibuffer-window)
                                    (minibuffer-selected-window))
                               (with-minibuffer-selected-window
                                 (let ((wind (selected-window)))
                                   (or
                                    (window-right wind)
                                    (window-left wind)
                                    (split-window-horizontally))))
                             (let ((wind (selected-window)))
                               (or
                                (window-right wind)
                                (window-left wind)
                                (split-window-horizontally))))
                           (km-browse-window-right-get-or-create))))
       other-wind)))
  (when fn (apply fn args)))


(defun km-browse-xwidget-browse (&optional url &rest _ignored)
  "Open URL with xvidget in right window."
  (interactive)
  (require 'xwidget)
  (if (active-minibuffer-window)
      (with-selected-window (selected-window)
        (km-browse-xwidget-in-other-window
         #'km-browse-with-xwidget-or-fallback-browse url))
    (km-browse-xwidget-in-other-window
     #'km-browse-with-xwidget-or-fallback-browse url)))


(defun km-browse-chrome-xwidget-and-exit ()
  "Exit minibuffer and `km-browse-xwidget-browse'."
  (interactive)
  (km-browse-chrome-exit-with-action #'km-browse-xwidget-browse))


(defun km-browse-eww-and-exit ()
  "Call `eww' with the current candidate and exit the minibuffer."
  (interactive)
  (km-browse-chrome-exit-with-action #'eww))


(defun km-browse-chrome-pdf-and-exit ()
  "Exit minibuffer and `km-browse-action-pdf'."
  (interactive)
  (km-browse-chrome-exit-with-action #'km-browse-action-pdf))

(defun km-browse-chrome-localhost-and-exit ()
  "Exit minibuffer and `km-browse-localhost'."
  (interactive)
  (km-browse-chrome-exit-with-action #'km-browse-localhost))



(defun km-browse-insert-action (item &optional separator)
  "Insert or complete ITEM and SEPARATOR.
If word at point is prefix of ITEM, complete it, else insert ITEM.
Optional argument SEPARATOR is a string to insert just after ITEM.
Default value of SEPARATOR is space."
  (let ((parts))
    (setq parts
          (if-let* ((current-word (let* ((end (point))
                                        (beg (+ end (save-excursion
                                                      (skip-chars-backward
                                                       "^'\"\s\t\n)(}{][")))))
                                   (when (< beg end)
                                     (buffer-substring-no-properties beg end)))))
              (progn
                (if (string-prefix-p current-word item)
                    (list (substring-no-properties item (length current-word)))
                  (list (or separator "\n") item)))
            (list item)))
    (apply #'insert parts)))

(defun km-browse-action-pdf (url &optional output-file)
  "Save URL as pdf to OUTPUT-FILE."
  (if-let* ((chrome (seq-find #'executable-find
                             '("google-chrome" "chrome" "chromium"))))
      (let* ((out-file-base
              "preview")
             (out-file (or output-file
                           (expand-file-name (concat out-file-base ".pdf")
                                             (temporary-file-directory))))
             (args `(,@'("--headless" "--run-all-compositor-stages-before-draw"
                         "--disable-gpu"
                         "--print-to-pdf-no-header"
                         "--enable-logging")
                     ,(format "--print-to-pdf=%s"
                       (shell-quote-argument out-file))
                     ,url)))
        (let ((proc (get-process chrome))
              (buffer (generate-new-buffer (format "*%s*" chrome))))
          (when proc
            (kill-buffer (process-buffer proc))
            (kill-process chrome))
          (progn
            (with-current-buffer buffer
              (setq proc (apply #'start-process
                                (append
                                 (list chrome buffer chrome)
                                 args)))
              (shell-command-save-pos-or-erase)
              (require 'shell)
              (when (fboundp 'shell-mode)
                (shell-mode)))
            (set-process-sentinel
             proc
             (lambda (process _state)
               (let ((output (with-current-buffer
                                 (process-buffer process)
                               (buffer-string))))
                 (kill-buffer (process-buffer process))
                 (if (equal (process-exit-status process) 0)
                     (with-selected-window (selected-window)
                       (find-file-other-window out-file))
                   (user-error (format "%s\n%s" chrome output))))))
            (when (fboundp 'comint-output-filter)
              (set-process-filter proc #'comint-output-filter)))))
    (error "Can not find chrome executable")))


(defun km-browse-insert ()
  "Insert current url and exit minibuffer."
  (interactive)
  (km-browse-chrome-exit-with-action #'km-browse-insert-action))


(defun km-browse-chrome-copy ()
  "Copy current url without exiting minibuffer."
  (interactive)
  (pcase-let ((`(,_category . ,current)
               (km-browse-chrome-get-current-candidate)))
    (when current
      (kill-new current)
      (message "Copied %s" current))))


(defun km-browse-chrome-xwidget-no-exit ()
  "Preview current minibuffer url without exiting minibuffer."
  (interactive)
  (km-browse-chrome-action-no-exit #'km-browse-xwidget-browse))

(defun km-browse-with-eww-no-exit ()
  "Open the current minibuffer candidate in EWW without exiting the minibuffer."
  (interactive)
  (km-browse-chrome-action-no-exit #'eww))

(defvar km-browse-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'km-browse-with-eww-no-exit)
    (define-key map (kbd "C-<return>") #'km-browse-chrome-browse-new)
    (define-key map (kbd "C-c C-o e") #'km-browse-eww-and-exit)
    (define-key map (kbd "C-c C-o x") #'km-browse-chrome-xwidget-and-exit)
    (define-key map (kbd "C-c C-p") #'km-browse-chrome-pdf-and-exit)
    (define-key map (kbd "C-c C-l") #'km-browse-chrome-localhost-and-exit)
    (define-key map (kbd "C-c C-i") #'km-browse-insert)
    (define-key map (kbd "M-w") #'km-browse-chrome-copy)
    map))


;;;###autoload
(defun km-browse-chrome-browse-new (&optional action)
  "Execute ACTION (default to `km-browse-action-default') on the minibuffer text.

If there are no minibuffer input, or it is empty, read it in in minibuffer.

If nil, `km-browse-action-default' is used by default."
  (interactive)
  (if (active-minibuffer-window)
      (pcase-let ((`(,_category . ,current)
                   (km-browse-chrome-get-current-candidate))
                  (text (buffer-substring-no-properties (minibuffer-prompt-end)
                                                        (line-end-position))))
        (progn (run-with-timer 0.1 nil
                               (lambda (str)
                                 (let ((url
                                        (km-browse-format-to-url
                                         (or str
                                             (read-string
                                              "Browse: ")))))
                                   (funcall #'km-browse-action-default
                                            url)))
                               (if (string-empty-p text)
                                   current
                                 text))
               (abort-minibuffers)))
    (let ((url
           (minibuffer-with-setup-hook
               (lambda ()
                 (when (minibufferp)
                   (let ((map (make-composed-keymap km-browse-actions-map
                                                    (current-local-map))))
                     (use-local-map map))))
             (browse-url (read-string "URL: ")))))
      (funcall (or action #'km-browse-action-default) url))))

(defun km-browse-completing-read (prompt urls &optional annotate-fn keymap)
  "Read URLS in minibuffer with PROMPT, ANNOTATE-FN and KEYMAP."
  (let ((collection (copy-tree (if (functionp urls)
                                   (funcall urls)
                                 urls))))
    (let ((url
           (minibuffer-with-setup-hook
               (lambda ()
                 (when (minibufferp)
                   (let ((map (make-composed-keymap (or keymap
                                                        km-browse-actions-map)
                                                    (current-local-map))))
                     (use-local-map map))))
             (completing-read prompt
                              (lambda (str pred action)
                                (if (eq action 'metadata)
                                    `(metadata
                                      (annotation-function .
                                       ,(if annotate-fn
                                            annotate-fn
                                          'km-browse-annotate))
                                      (category . url))
                                  (complete-with-action action collection str
                                                        pred)))
                              nil nil
                              (or
                               (km-browse-get-region)
                               (when (and (bound-and-true-p iedit-mode)
                                          (fboundp
                                           'iedit-current-occurrence-string))
                                 (iedit-current-occurrence-string))
                               (thing-at-point 'url)
                               (thing-at-point 'word t))
                              'km-browse-minibuffer-history))))
      url)))


;;;###autoload
(defun km-browse-chrome-history (&optional action)
  "Open a URL from Chrome history.

Optional argument ACTION is a function to call with the URL selected from the
Chrome history. If not provided, it defaults to `km-browse-action-default' when
called interactively, or `identity' otherwise."
  (interactive)
  (let ((url
         (km-browse-completing-read
          "History"
          (km-browse-init-chrome-history-candidates))))
    (funcall (or action (if (called-interactively-p 'any)
                            #'km-browse-action-default
                          #'identity))
             (km-browse-format-to-url url))))


;;;###autoload
(defun km-browse-chrome-bookmarks (&optional action)
  "Open a Chrome bookmark URL.

Optional argument ACTION is a function to call with the selected URL."
  (interactive)
  (let ((url
         (km-browse-completing-read
          "Bookmarks: "
          (km-browse-read-chrome-bookmarks)
          'km-browse-annotate)))
    (funcall (or action (if (called-interactively-p 'any)
                            #'km-browse-action-default
                          #'identity))
             (km-browse-format-to-url url))))

;;;###autoload
(defun km-browse-chrome-other (&optional action)
  "Open a URL in a non-default browser.

Optional argument ACTION is a function to call with the URL. If not provided,
`km-browse-action-default' is used as the default function."
  (interactive)
  (let ((url
         (minibuffer-with-setup-hook
             (lambda ()
               (when (minibufferp)
                 (let ((map (make-composed-keymap km-browse-actions-map
                                                  (current-local-map))))
                   (use-local-map map))))
           (read-string "Other URL: "))))
    (funcall (or action #'km-browse-action-default) url)))

(defun km-browse-urls-from-kill-ring ()
  "Collect URLs found in the kill ring."
  (with-temp-buffer
    (insert (mapconcat (lambda (str)
                         (substring-no-properties str 0))
                       kill-ring "\n"))
    (km-browse-urls-from-buffer)))

(defun km-browse-urls-from-buffer ()
  "Collect URLs found in the current buffer."
  (let ((links)
        (re  (rx (group
                  (group
                   (or (group "http"
                              (opt "s")
                              "://"
                              (opt (group "www.")))
                       "git@"
                       (seq "file:/"
                            (zero-or-more "~"))))
                  (group
                   (one-or-more
                    (any "0-9a-z" "-"))
                   (one-or-more
                    (group
                     (or (group ":"
                                (zero-or-more
                                 (any "0-9")))
                         (seq "."
                              (one-or-more
                               (any "0-9a-z"))))))
                   (opt "/")
                   (any "a-z")
                   (zero-or-more
                    (not (any "\t\n\f\r ;|")))
                   (one-or-more
                    (any "0-9a-z" "-")))))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              re
              nil t 1)
        (push (match-string-no-properties 0)
              links))
      links)))

(defun km-browse-urls-from-buffer-and-kill-ring ()
  "Seearch for urls in current buffer and kill ring."
  (delete-dups (append (km-browse-urls-from-kill-ring)
                       (km-browse-urls-from-buffer))))

(defcustom km-browse-url-sources
  '(("All" . km-browse-all-urls)
    ("Chrome Bookmarks " . km-browse-read-chrome-bookmarks)
    ("Chrome History" . km-browse-init-chrome-history-candidates)
    ("Buffer and kill ring urls" . km-browse-urls-from-buffer-and-kill-ring)
    ("Hosts" .
     (lambda ()
       (mapcar #'car
               (km-browse-all-urls-groupped-alist)))))
  "Alist of source names and either list of urls or functions.
If value of cons is function, it will be called without arguments and
should return list of urls."
  :type '(alist
          :key-type (string :tag "Prompt")
          :value-type (choice
                       (repeat :tag "List of urls"
                               (string :tag "Url"))
                       (function :tag "Function")))
  :group 'km-browse)


;;;###autoload
(defun km-browse-from-all-sources (&optional action)
  "Browse URLs or search from multiple sources.

Optional argument ACTION is a function to be called with the URL."
  (interactive)
  (funcall (or action #'km-browse-action-default)
           (km-browse-format-to-url
            (km-browse-multi-source-read
             (append (mapcar (lambda (it)
                               `(,(car it) km-browse-completing-read
                                 ,(format "%s: "
                                   (car
                                    it))
                                 ,(cdr it)))
                             km-browse-url-sources)
                     (list
                      (list "Other" #'km-browse-chrome-other
                            #'identity)))))))

;;;###autoload
(defun km-browse-groupped (&optional action)
  "Open a grouped URL or search query.

Optional argument ACTION is a function to call with the selected URL. If nil,
`km-browse-action-default' is used as the default action."
  (interactive)
  (funcall (or action #'km-browse-action-default)
           (km-browse-format-to-url
            (let* ((alist
                    (km-browse-all-urls-groupped-alist))
                   (annotf (lambda (str)
                             (format " (%s)" (length (cdr (assoc str alist))))))
                   (group
                    (km-browse-completing-read "Candidates: "
                                               (mapcar #'car alist)
                                               annotf))
                   (cell
                    (assoc-string group alist)))
              (when cell (setq cell
                               (seq-uniq cell)))
              (if (= 1 (length cell))
                  (car cell)
                (km-browse-completing-read (format "Url:  %s" group)
                                           cell))))))

(defvar browse-url--browser-defcustom-type)

(defun km-browse-url-get-functions-alist ()
  "Return alist of descriptions and function for browse url."
  (require 'browse-url)
  (let ((choices (delete nil
                         (mapcar (lambda (it)
                                   (when-let* ((pl
                                                (when (eq (car it)
                                                          'function-item)
                                                  (cdr it)))
                                               (tag (plist-get pl :tag))
                                               (value (plist-get pl :value)))
                                     (when (fboundp value)
                                       (cons tag value))))
                                 (cdr (copy-tree
                                       browse-url--browser-defcustom-type))))))
    (when (executable-find "xdg-open")
      (push '("XDG open" . km-browse-xdg-open) choices))
    (when (and (fboundp 'xwidget-webkit-browse-url)
               (not (derived-mode-p 'xwidget-webkit-mode)))
      (push '("X widget" . km-browse-xwidget-browse) choices))
    choices))

(defun km-browse-prompt-browser (prompt)
  "PROMPT user to select a browser function and return its associated value.

Argument PROMPT is a string used to request user input for selecting a browser
function."
  (let* ((choices (km-browse-url-get-functions-alist)))
    (cdr (assoc (completing-read prompt choices) choices))))

(defun km-browse-xdg-open (file)
  "Open FILE with xdg-open."
  (and file (shell-command (concat "xdg-open " (shell-quote-argument file)))))

(defun km-browse--open-url-in-browser (&optional url browse-fn)
  "Open a URL in a browser, handling file URLs appropriately.

Optional argument URL is the web address or file path to be opened.

Optional argument BROWSE-FN is the function used to open the URL."
  (if (and
       url
       (not (string-prefix-p "file:/" url))
       (file-exists-p url)
       (not (eq browse-fn 'km-browse-xdg-open)))
      (funcall browse-fn (concat "file:///" url))
    (funcall browse-fn url)))

(defvar km-browse-chrome-sesssion-dump-buffer "*chrome-session-dump*")
(defun km-browse-chrome-install-session-dump ()
  "Install chrome-sesion-dump to /usr/bin/chrome-session-dump."
  (let ((default-directory "/sudo::")
        (inhibit-read-only t))
    (async-shell-command
     "sudo curl -o /usr/bin/chrome-session-dump -L 'https://github.com/lemnos/chrome-session-dump/releases/download/v0.0.2/chrome-session-dump-linux' && sudo chmod 755 /usr/bin/chrome-session-dump"
     km-browse-chrome-sesssion-dump-buffer)))


(defun km-browse-chrome-session-dump-get-active-tabs ()
  "Return list of active tabs in google-chrome."
  (when-let* ((file (km-browse-chrome-guess-config-file)))
    (when (executable-find "chrome-session-dump")
      (split-string
       (shell-command-to-string
        (concat "chrome-session-dump\s"
                (shell-quote-argument
                 (expand-file-name
                  file))))
       "\n" t))))

(defun km-browse-url-get-all-urls ()
  "Return list of urls from `kill-ring', buffer, chrome history, bookmarks etc."
  (let ((extra-sources
         (seq-filter
          #'km-browse-web-url-p
          (mapcar #'substring-no-properties
                  (append (append (copy-tree kill-ring)
                                  (copy-tree minibuffer-history))
                          (delq nil (list (ignore-errors (thing-at-point
                                                          'url t))
                                          (ignore-errors
                                            (gui-get-selection)))))))))
    (append
     extra-sources
     (km-browse-chrome-session-dump-get-active-tabs)
     (km-browse-read-chrome-bookmarks)
     (km-browse-init-chrome-history-candidates))))

(defun km-browse-read-url (&optional prompt)
  "Read an url with PROMPT and completion from `km-browse-url-get-all-urls'."
  (km-browse-completing-read
   (or prompt "Url\s")
   (km-browse-url-get-all-urls)))


(defun km-browse-url-to-download-base-name (url)
  "Convert URL to download name without extension."
  (replace-regexp-in-string
   "[-]\\{1\\}+"
   "-"
   (replace-regexp-in-string
    split-string-default-separators
    "-"
    (car (last (split-string (file-name-sans-extension
                              url)
                             "/" t))))))

(defun km-browse-download-as-org (url filename)
  "Download URL in org mode format and save it as FILENAME."
  (let ((str (shell-command-to-string (concat
                                       "pandoc -f html -t org "
                                       url))))
    (write-region str nil filename)
    (when (file-exists-p filename)
      (find-file filename))))

(defun km-browse-f-change-ext (file new-ext)
  "Replace extension of FILE with NEW-EXT."
  (concat (file-name-sans-extension file) "." new-ext))

(defun km-browse-download-url (url)
  "Download URL and return string."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (prog1
        (with-temp-buffer
          (insert
           (with-current-buffer download-buffer
             (set-buffer download-buffer)
             (goto-char (point-min))
             (re-search-forward "\r?\n\r?\n")
             (delete-region (point-min)
                            (point))
             (buffer-string)))
          (decode-coding-region (point-min)
                                (point-max) 'dos)
          (buffer-string))
      (kill-buffer download-buffer))))

(defun km-browse-download-url-to-file (url file)
  "Download URL to FILE."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point)
                    (point-max) file
                    nil
                    'silent)
      file)))

(defun km-browse-pandoc-from-string (string input-type output-type &rest
                                            options)
  "Execute `pandoc' on STRING in INPUT-TYPE to OUTPUT-TYPE additional OPTIONS."
  (setq options (delete nil (flatten-list options)))
  (let ((args (append
               (list (executable-find "pandoc") t t nil)
               (list "-f" input-type "-t" output-type) options)))
    (with-temp-buffer
      (insert string)
      (let ((success (zerop (apply #'call-process-region (append (list
                                                                  (point-min)
                                                                  (point-max))
                                                                 args)))))
        (cond (success
               (pcase output-type
                 ("org" (km-browse--strip-org-properties)))
               (buffer-string))
              (t (minibuffer-message "Pandoc error: %s"
                                     (buffer-string))
                 nil))))))

(defun km-browse--strip-org-properties ()
  "Remove specific Org properties from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward
              ":PROPERTIES:[\n]+[\s\t]+:CUSTOM_ID:[\s\t][^\n]+[\n]+[\s\t]+:END:[\n]"
              nil t
              1)
        (replace-match "")))))


;;;###autoload
(defun km-browse-download-file (&optional url directory download-name)
  "Download file at URL into DIRECTORY under DOWNLOAD-NAME."
  (interactive (list (km-browse-read-url "Download url: ")))
  (require 'url)
  (let* ((content (km-browse-download-url url))
         (filename (expand-file-name
                    (or download-name
                        (read-string "Save as "
                                     (km-browse-url-to-download-base-name
                                      url)))
                    (or directory (read-directory-name "Save to: ")))))
    (write-region (if (equal (file-name-extension filename) "org")
                      (apply #'km-browse-pandoc-from-string content
                             "html" "org"
                             km-browse-pandoc-options)
                    content)
                  nil filename
                  nil
                  'silent)
    (when (file-exists-p filename)
      (find-file filename))))


;;;###autoload
(defun km-browse-open-current-file-in-browser (filename fn)
  "Open the current file or URL in a selected web browser.

Argument FILENAME is the file path or URL to be opened in the browser.

Argument FN is the function used to open the file or URL."
  (interactive
   (let* ((file
           (pcase (seq-find #'derived-mode-p '(dired-mode org-mode
                                               xwidget-webkit-mode))
             ('dired-mode
              (when (and (fboundp 'dired-get-marked-files)
                         (fboundp 'dired-file-name-at-point))
                (or (dired-get-marked-files)
                    (dired-file-name-at-point))))
             ('org-mode (or (cdr-safe (seq-find (lambda (it)
                                                  (eq (car-safe it)
                                                      :uri))
                                                (text-properties-at
                                                 (point))))))
             ('xwidget-webkit-mode
              (when-let* ((sess
                           (when (fboundp
                                  'xwidget-webkit-current-session)
                             (xwidget-webkit-current-session))))
                (xwidget-webkit-uri sess)))
             (_ (or buffer-file-name
                    (read-file-name "File: ")))))
          (fn (km-browse-prompt-browser (format "Open %s with: "
                                                file))))
     (list file fn)))
  (if (listp filename)
      (dolist (u filename)
        (when u
          (km-browse--open-url-in-browser u fn)))
    (km-browse--open-url-in-browser filename fn)))

(provide 'km-browse)
;;; km-browse.el ends here