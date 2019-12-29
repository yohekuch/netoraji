;;; netoraji.el --- Netoraji Client for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019-2019 The netoraji.el Authors

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

;; Read headlines of netoraji and Listen them from Emacs.
;; フォーマットデータについての詳細は <http://ladio.wiki.fc2.com>
;;
;; Enjoy!

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
  "現在リスナー数のフェイス"
  :group 'netoraji)

(defface netoraji-item-total-listener
  '((t :foreground "white"))
  "延べリスナー数のフェイス"
  :group 'netoraji)

(defface netoraji-item-max-listener
  '((t :foreground "white"))
  "最大リスナー数のフェイス"
  :group 'netoraji)

(defface netoraji-item-elapsed-time
  '((t :foreground "white"))
  "経過時間のフェイス"
  :group 'netoraji)

(defface netoraji-item-name
  '((t :foreground "cyan"))
  "番組名のフェイス"
  :group 'netoraji)

(defface netoraji-item-genre
  '((t :foreground "PaleGreen"))
  "ジャンルのフェイス"
  :group 'netoraji)

(defface netoraji-item-description
  '((t :foreground "LightSalmon"))
  "説明のフェイス"
  :group 'netoraji)

(defface netoraji-item-broadcasting-url
  '((t :foreground "goldenrod"))
  "放送URLのフェイス"
  :group 'netoraji)

(defface netoraji-item-relevant-url
  '((t :foreground "goldenrod"))
  "関連URLのフェイス"
  :group 'netoraji)

(defface netoraji-item-dj
  '((t :foreground "LightGoldenrod"))
  "DJ名のフェイス"
  :group 'netoraji)

(defface netoraji-item-song
  '((t :foreground "LightSkyBlue"))
  "SONGのフェイス"
  :group 'netoraji)

;;;; User options

(defcustom netoraji-item-format "%3l %t %n %d%g%e%u%s"
  "ねとらじバッファにおける各番組情報の表示方法を指定するフォーマット。
`format-spec'へ第一引数として渡される。%変換指定子は次の通り解釈される。

%l - 現在リスナー数
%c - 延べリスナー数
%m - 最大リスナー数
%t - 放送開始時刻からの経過時間
%n - 番組名
%g - ジャンル
%e - 説明
%b - 放送URL
%u - 関連URL
%d - DJ名
%s - SONG
"
  :group 'netoraji
  :type 'string)

;;;; Internal definitions

(defconst netoraji-headlines-url "http://yp.ladio.net/stats/list.v2.dat"
  "ねとらじヘッドラインのURL")

(defvar netoraji-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'netoraji)
    (define-key map (kbd "n") #'netoraji-next-item)
    (define-key map (kbd "p") #'netoraji-previous-item)
    (define-key map (kbd "t") #'netoraji--browse-url)
    (define-key map (kbd "l") #'netoraji--play-channel)
    (define-key map (kbd "k") #'kill-ladio)
    (define-key map (kbd "+") #'netoraji-play-increase-volume)
    (define-key map (kbd "-") #'netoraji-play-decrease-volume)
    map)
  "ねとらじバッファ上でのキーマップ")

(defcustom netoraji-after-render-hook ()
  ""
  :group 'netoraji
  :type 'hook)

;;;; Utils

;;;; Motion

(defun netoraji-next-item ()
  ""
  (interactive)
  (let* ((pos (point))
         (burlp (get-text-property pos 'burl)))
    (when burlp
      (setq pos (next-single-property-change pos 'burl)))
    (setq pos (next-single-property-change pos 'burl))
    (when pos
      (goto-char pos))))

(defun netoraji-previous-item ()
  "一つ前のねとらじタイトルの先頭へポイントを移動する。"
  (interactive)
  (let ((pos (previous-single-property-change (point) 'burl)))
    (when pos
      (progn
        (unless (get-text-property pos 'burl)
          (setq pos (previous-single-property-change pos 'burl)))
        (goto-char pos)))))

;;;; Listen

(defcustom netoraji-play-format "mplayer -really-quiet %s 2>/dev/null"
  "ねとらじを再生するプレーヤーのコマンド"
  :group 'netoraji)

(defun netoraji--play-channel ()
  (interactive)
  (let ((url (button-get (point) 'burl))
        (inhibit-read-only t))
    (start-process-shell-command "prc-netoraji"
                                 "buf-netoraji"
                                 (format netoraji-play-format url))
    (put-text-property (point-at-bol) (point-at-eol)
                       'face 'bold)
    (beginning-of-line)))

(defun netoraji-play-increase-volume ()
  (interactive)
  (process-send-string "prc-netoraji" "*"))

(defun netoraji-play-decrease-volume ()
  (interactive)
  (process-send-string "prc-netoraji" "/"))

(defun kill-ladio ()
  (interactive)
  (kill-process "prc-netoraji")
  (message "Player has beed stopped."))

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

(define-derived-mode netoraji-mode special-mode "NTRJ"
  "Major mode for netoraji."
  :group 'netoraji
  (setq truncate-lines t)
  (buffer-disable-undo))

;;;; Retrieval

(defun netoraji--browse-url ()
  "関連URLをブラウザで開く"
  (interactive)
  (eww (button-get (point) 'rurl)))

(defcustom netoraji-current-listener-format "[%3s]"
  "現在リスナー数の表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-total-listener-format "[%s]"
  "延べリスナー数の表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-max-listener-format "[%s]"
  "最大リスナー数の表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-elapsed-time-format "%s"
  "経過時間の表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-name-format "%s"
  "番組名の表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-genre-format "[%s]"
  "ジャンルの表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-description-format "[%s]"
  "説明の表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-broadcasting-url-format "[%s]"
  "放送URLの表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-relevant-url-format "[%s]"
  "関連URLの表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-dj-format "[%s]"
  "DJ名の表示形式"
  :group 'netoraji
  :type 'string)

(defcustom netoraji-song-format "[%s]"
  "SONGの表示形式"
  :group 'netoraji
  :type 'string)

(defun netoraji--render-item (channel)
  "整形された1アイテム分のテキストをプロパティ付きで返す。"
  (let ((current-listener (gethash "CLN"  channel))
        (total-listener   (gethash "CLNS" channel))
        (max-listener     (gethash "MAX"  channel))
        (elapsed-time     (netoraji-elapsed-time (gethash "TIMS" channel)))
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
                                  'type 'NAM)
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
  "番組情報を表示する。"
  (insert (netoraji--render-item item) "\n")
  (goto-char (point-min))
  (sort-lines t (point-min) (point-max)))

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
            (mapcar 's-trim (s-split "^$" whole-contents)))
    ))

(defun netoraji--read-contents ()
  "Retrieve and read contents of netoraji headlines datfile."
  (with-temp-buffer
    (url-insert-file-contents netoraji-headlines-url)
    (netoraji--parse-headlines)))

(defun netoraji--load-headlines ()
  "Retrieve and render netoraji channels from headlines"
  (let ()
    (with-current-buffer (get-buffer-create "*netoraji*")
      (let ((inhibit-read-only t)
            (sorted-contents))
        (erase-buffer)
        (netoraji-mode)
        (mapcar #'netoraji--display-item (netoraji--read-contents))
        (pop-to-buffer (current-buffer))
        (message "Headlines retrieved.")))))

;;;; Feeds

;;;###autoload
(defun netoraji ()
  "Read netoraji headlines."
  (interactive)
  (netoraji--load-headlines))

(provide 'netoraji)

;;; netoraji.el ends here