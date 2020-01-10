;;; netoraji.el --- Netoraji Client for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Yohei Kuchiki

;; Author: Yohei Kuchiki <extentlambda@gmail.com>
;; Keywords: Internet-Radio
;; Version: 0.1.0
;; Homepage: https://github.com/yohekuch/netoraji.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Read headlines of netoraji and Listen radio.
;; フォーマットデータについての詳細は <http://ladio.wiki.fc2.com>
;;
;; Enjoy!

;; TODO
;; フックを定義する
;; listenするとき、既に聞いている番組があれば先にkillする
;; done killするとき、フェイスを元に戻す
;; done reloadするとき、listen中のフェイスを維持する
;; done reload後、ポイントを番組名の先頭に持ってく
;; done タイトル以外でplay-channelすると放送は聞けないが、フェイスが変わってしまうのを何とかする。
;; 関数名に統一性を持たせる
;; 関数の説明文を書く
;; タイトル->DJ名->ジャンル・・・と次の情報へ移動する関数を作る
;; 放送が終了した時、再生中フェイスを元に戻す。或いはreloadしてもいいかもしれない。
;; たぶん番兵を使って実現できる。

;;; Code:

(require 's)
(require 'url)

(defgroup netoraji nil
  "Simple netoraji client."
  :group 'external
  :prefix "netoraji-")

;;;; Faces

(defface netoraji-item-current-listener
  '((t :foreground "white"))
  "Face used for the number of current listeners."
  :group 'netoraji)

(defface netoraji-item-total-listener
  '((t :foreground "white"))
  "Face used for the number of total listeners."
  :group 'netoraji)

(defface netoraji-item-max-listener
  '((t :foreground "white"))
  "Face used for the number of max listeners."
  :group 'netoraji)

(defface netoraji-item-elapsed-time
  '((t :foreground "white"))
  "Face used for elapsed time."
  :group 'netoraji)

(defface netoraji-item-name
  '((t :foreground "cyan"))
  "Face used for the program name."
  :group 'netoraji)

(defface netoraji-item-genre
  '((t :foreground "PaleGreen"))
  "Face used for the genres."
  :group 'netoraji)

(defface netoraji-item-description
  '((t :foreground "LightSalmon"))
  "Face used for the descriptions."
  :group 'netoraji)

(defface netoraji-item-broadcasting-url
  '((t :foreground "goldenrod"))
  "Face used for the broadcasting URLs."
  :group 'netoraji)

(defface netoraji-item-relevant-url
  '((t :foreground "goldenrod"))
  "Face used for the relevant URLs."
  :group 'netoraji)

(defface netoraji-item-dj
  '((t :foreground "LightGoldenrod"))
  "Face used for the DJ names."
  :group 'netoraji)

(defface netoraji-item-song
  '((t :foreground "LightSkyBlue"))
  "Face used for song titles."
  :group 'netoraji)

(defface netoraji-item-playing-name
  '((t :foreground "white"
       :weight bold))
  "Face used for the name of the playing program."
  :group 'netoraji)

;;;; User options

(defcustom netoraji-item-format "%3l %t %n %d%g%e%u%s"
  "ねとらじバッファにおける各番組情報の表示方法を指定するフォーマット。
`format-spec'へ第一引数として渡される。%変換指定子は次の通り解釈される。

%l - The number of current listener
%c - The number of total listener
%m - The number of max listener
%t - Elapsed time
%n - Program name
%g - Genre
%e - Description
%b - Broadcasting URL
%u - Relevant URL
%d - DJ's name
%s - SONG"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-current-listener-format "[%3s]"
  "Format specification for displaying the number of current listener."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-total-listener-format "[%s]"
  "Format specification for displaying the number of total listener."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-max-listener-format "[%s]"
  "Format specification for displaying the number of max listener."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-elapsed-time-format "%s"
  "Format specification for displaying the elapsed time."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-name-format "%s"
  "Format specification for displaying the program name."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-genre-format "[%s]"
  "Format specification for displaying the genre of programs."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-description-format "[%s]"
  "Format specification for displaying the description of programs."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-broadcasting-url-format "[%s]"
  "Format specification for displaying the broadcasting url."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-relevant-url-format "[%s]"
  "Format specification for displaying the relevant url."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-dj-format "[%s]"
  "Format specification for displaying the DJ's name."
  :group 'netoraji
  :type 'string)

(defcustom netoraji-song-format "[%s]"
  "Format specification for displaying the song being broadcasted."
  :group 'netoraji
  :type 'string)

;;;; Internal definitions

(defconst netoraji-headlines-url "http://yp.ladio.net/stats/list.v2.dat"
  "Headlines' URL")

(defvar netoraji-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'netoraji)
    (define-key map (kbd "n") #'netoraji-next-item)
    (define-key map (kbd "p") #'netoraji-previous-item)
    (define-key map (kbd "t") #'netoraji--browse-url)
    (define-key map (kbd "l") #'netoraji--play-listen-radio)
    (define-key map (kbd "k") #'netoraji--play-quit-radio)
    (define-key map (kbd "+") #'netoraji--play-increase-volume)
    (define-key map (kbd "-") #'netoraji--play-decrease-volume)
    map)
  "Keymap used in netoraji buffer.")

(defcustom netoraji-after-render-hook ()
  ""
  :group 'netoraji
  :type 'hook)

;;;; Utils

;;;; Motion

(defun netoraji-next-item ()
  "Move to the next program name."
  (interactive)
  (let* ((pos (point))
         (burlp (get-text-property pos 'burl)))
    (when burlp
      (setq pos (next-single-property-change pos 'burl)))
    (setq pos (next-single-property-change pos 'burl))
    (when pos
      (goto-char pos))))

(defun netoraji-previous-item ()
  "Move to the previous program name."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'burl)))
    (when pos
      (unless (get-text-property pos 'burl)
        (setq pos (previous-single-property-change pos 'burl)))
      (goto-char pos))))

;;;; Play

(defun netoraji--play-listen-radio ()
  "Play program at point with MPlayer.
If it is playing another program, quit the process beforehand."
  (interactive)
  (let ((url (button-get (point) 'burl))
        (inhibit-read-only t))
    (netoraji--play-quit-radio)
    (when url
      (set (make-local-variable 'netoraji-play-process)
           (start-process "netoraji-play-proc"
                          "netoraji-play-buf"
                          "mplayer"
                          "-quiet" "-slave"
                          (format "%s" url)))
      (add-text-properties (previous-single-property-change (+ (point) 1) 'burl)
                           (next-single-property-change (point) 'burl)
                           '(face netoraji-item-playing-name play t))
      (message "Start playing %s" url))))

(defun netoraji--play-increase-volume ()
  "Increase volume."
  (interactive)
  (ignore-errors (process-send-string netoraji-play-process "volume 1\n")))

(defun netoraji--play-decrease-volume ()
  "Decrease volume."
  (interactive)
  (ignore-errors (process-send-string netoraji-play-process "volume -1\n")))

(defun netoraji--play-quit-radio ()
  "Quit playing."
  (interactive)
  (ignore-errors (process-send-string netoraji-play-process "quit\n"))
  (let ((spos (next-single-property-change (point-min) 'play))
        (inhibit-read-only t))
    (when spos
      (save-excursion
        (goto-char spos)
        (setq epos (next-single-property-change (point) 'play))
        (put-text-property spos epos 'face 'netoraji-item-name)
        (remove-text-properties spos epos '(play nil)))
      (message "Player has been stopped."))))

;;;; UI

(defun netoraji-fix-time (tims)
  "yy/mm/dd HH:MM:SS を YYYY-mm-dd HH:MM:SS に変換した文字列を返す。"
  (s-replace "/" "-" (concat "20" tims)))

(defun netoraji-elapsed-time (tims)
  "放送開始時刻を元に現在までの経過時間 HHH:MM:SS を文字列で返す。
ただし、1000時間以上のものは 999:59:59 固定とする。
ねとらじの規約上或いはシステム上そこまでの長時間放送が可能なのかは不明。"
  (let* ((stime (netoraji-fix-time tims))
         (encoded-stime (apply 'encode-time (parse-time-string stime)))
         (elsec (float-time (time-subtract nil encoded-stime)))
         (els (mod elsec 60))
         (elm (mod (/ elsec 60) 60))
         (elh (/ elsec 60 60)))
    (if (>= elh 1000)
        "999:59:59"
      (format "%3d:%02d:%02d" elh elm els))))

(defun netoraji--browse-url ()
  "Open relevant URL within eww browser."
  (interactive)
  (eww (button-get (point) 'rurl)))

(defun netoraji--get-playing-channel ()
  "一覧からテキストプロパティを元に再生中の番組を探し、id(concat 放送開始時刻 再生URL)を返す。
再生中の番組がなければnilを返す。"
  (let ((spos (next-single-property-change (point-min) 'play)))
    (ignore-errors (get-text-property spos 'id))))

(defun netoraji--set-playing-channel (id)
  "idを持つ番組名にテキストプロパティ(フェイス、プレイ中フラグ)を付ける。"
  (let ((spos (point-min))
        epos)
    (while (let (tid)
             (setq spos (next-single-property-change spos 'id))
             (when spos
               (setq tid (get-text-property spos 'id))
               (not (equal id tid)))))
    (setq epos (next-single-property-change spos 'id))
    (add-text-properties spos epos
                         '(face netoraji-item-playing-name play t))))

(defun netoraji--render-item (channel)
  "整形された1アイテム分のテキストをプロパティ付きで返す。"
  (let* ((current-listener (gethash "CLN"  channel))
         (total-listener   (gethash "CLNS" channel))
         (max-listener     (gethash "MAX"  channel))
         (start-time       (gethash "TIMS" channel))
         (elapsed-time     (netoraji-elapsed-time start-time))
         (name             (if (string-empty-p (gethash "NAM" channel))
                               "None"
                             (gethash "NAM" channel)))
         (genre            (gethash "GNL"  channel))
         (description      (gethash "DESC" channel))
         (broadcasting-url (concat "http://"
                                   (gethash "SRV" channel)
                                   ":"
                                   (gethash "PRT" channel)
                                   (gethash "MNT" channel)))
         (relevant-url     (gethash "URL"  channel))
         (dj               (gethash "DJ"   channel))
         (song             (gethash "SONG" channel)))
    (s-trim-right
     (format-spec netoraji-item-format
                  (format-spec-make
                   ?l (propertize (format netoraji-current-listener-format
                                          current-listener)
                                  'face 'netoraji-item-current-listener
                                  'type 'CLN)
                   ?c (propertize (format netoraji-total-listener-format
                                          total-listener)
                                  'face 'netoraji-item-total-listener
                                  'type 'CLNS)
                   ?m (propertize (format netoraji-max-listener-format
                                          max-listener)
                                  'face 'netoraji-item-max-listener
                                  'type 'MAX)
                   ?t (propertize (format netoraji-elapsed-time-format
                                          elapsed-time)
                                  'face 'netoraji-item-elapsed-time
                                  'type 'ETIM)
                   ?n (propertize (format netoraji-name-format
                                          name)
                                  'face 'netoraji-item-name
                                  'burl broadcasting-url
                                  'rurl relevant-url
                                  'type 'NAM
                                  'id (concat start-time broadcasting-url))
                   ?g (if (string-empty-p genre)
                          ""
                        (propertize (format netoraji-genre-format
                                            genre)
                                    'face 'netoraji-item-genre
                                    'type 'GNR))
                   ?e (if (string-empty-p description)
                          ""
                        (propertize (format netoraji-description-format
                                            description)
                                    'face 'netoraji-item-description
                                    'type 'DESC))
                   ?b (propertize (format netoraji-broadcasting-url-format
                                          broadcasting-url)
                                  'face 'netoraji-item-broadcasting-url
                                  'type 'BURL)
                   ?u (if (string-empty-p relevant-url)
                          ""
                        (propertize (format netoraji-relevant-url-format
                                            relevant-url)
                                    'face 'netoraji-item-relevant-url
                                    'type 'RURL))
                   ?d (if (string-empty-p dj)
                          ""
                        (propertize (format netoraji-dj-format
                                            dj)
                                    'face 'netoraji-item-dj
                                    'type 'DJ))
                   ?s (if (string-empty-p song)
                          ""
                        (propertize (format netoraji-song-format
                                            song)
                                    'face 'netoraji-item-song
                                    'type 'SONG))
                   )))))

(defun netoraji--display-item (item)
  "Display channel items on the current buffer."
  (insert (netoraji--render-item item) "\n")
  (sort-lines t (point-min) (point-max)))

(define-derived-mode netoraji-mode special-mode "NTRJ"
  "Major mode for netoraji."
  :group 'netoraji
  (setq truncate-lines t)
  (buffer-disable-undo))

;;;; Retrieval

(defun netoraji--parse-headlines ()
  "ヘッドライン取得用のdatをパースし放送中の各番組情報が入ったリストを返す。
リストの各要素は番組情報を構成するハッシュテーブルである。"
  (let ((whole-contents
         (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (mapcar (lambda (str)
              (let ((pg (make-hash-table :test 'equal)))
                (mapcar (lambda (st)
                          (puthash (cadr st) (caddr st) pg))
                        (s-match-strings-all "^\\([A-Z]+\\)=\\(.*\\)$" str))
                pg))
            (mapcar 's-trim (s-split "^$" whole-contents)))))

(defun netoraji--read-contents ()
  "Retrieve and read contents of netoraji headlines datfile."
  (with-temp-buffer
    (url-insert-file-contents netoraji-headlines-url)
    (netoraji--parse-headlines)))

(defun netoraji--load-headlines ()
  "Retrieve and render netoraji channels from headlines"
  (with-current-buffer (get-buffer-create "*netoraji*")
    (let ((inhibit-read-only t)
          (id (netoraji--get-playing-channel)))
      (erase-buffer)
      (unless (derived-mode-p #'netoraji-mode)
        (netoraji-mode))
      (mapcar #'netoraji--display-item (netoraji--read-contents))
      (goto-char (point-min))
      (netoraji-next-item)
      (when id
        (netoraji--set-playing-channel id))
      (pop-to-buffer (current-buffer))
      (message "Headlines retrieved."))))

;;;###autoload
(defun netoraji ()
  "Read netoraji headlines."
  (interactive)
  (netoraji--load-headlines))

(provide 'netoraji)

;;; netoraji.el ends here
