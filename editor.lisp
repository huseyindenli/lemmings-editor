;;; editor.lisp

(in-package #:lemmings-editor)

;;;------------------------------------------------------------
;;; Grid / Level verisi (esnek geometri)
;;;------------------------------------------------------------

;; Grid boyutu (hücre sayısı) için varsayılanlar
(defparameter *grid-width*  20)  ;; x yönü (sütun)
(defparameter *grid-height* 15)  ;; y yönü (satır)

;; Her hücrenin piksel boyutu için varsayılan
(defparameter *tile-size* 24)

;; Tile tipleri: anahtarlar
;; :empty
;; :ground
;; :steel
;; :spawn
;; :exit
;; :can-be-excavated
;; :contacts
;; :decorations-and-background-anims

(defparameter *grid*  nil)  ;; 2D array: [y, x]
(defparameter *rects* nil)  ;; canvas rectangle id’leri

;; Aktif araç (palette’den seçilecek)
(defparameter *current-tool* :ground)

(defun tile->color (tile)
  "Tile tipine göre Tk renk ismi döndür."
  (ecase tile
    (:empty                         "black")
    (:ground                        "sienna3")   ;; normal zemin
    (:steel                         "slategray3") ;; kırılamaz blok
    (:spawn                         "deepskyblue2") ;; giriş
    (:exit                          "gold2")     ;; çıkış
    (:can-be-excavated              "peru")      ;; kazılabilir zemin
    (:contacts                      "red3")      ;; temas eden şeyler
    (:decorations-and-background-anims "darkolivegreen4"))) ;; dekor

(defun reconfigure-grid (&key (cols *grid-width*)
                              (rows *grid-height*)
                              (tile-size *tile-size*))
  "Grid geometrisini yeniden ayarla ve *grid* / *rects* dizilerini yeniden oluştur.
COLS: sütun sayısı
ROWS: satır sayısı
TILE-SIZE: her hücrenin piksel boyutu"
  (setf *grid-width*  cols
        *grid-height* rows
        *tile-size*   tile-size
        *grid*  (make-array (list *grid-height* *grid-width*)
                            :initial-element :empty)
        *rects* (make-array (list *grid-height* *grid-width*)
                            :initial-element nil)))

;; Yükleme anında varsayılan geometri ile grid oluştur
(reconfigure-grid)

;;;------------------------------------------------------------
;;; Level’i dosyaya yazmak
;;;------------------------------------------------------------

(defun level->sexp ()
  "Şu anki *grid*’i basit bir s-exp veri yapısına çevir.
Format: (:width W :height H :tiles ((row0) (row1) ...))"
  (let ((rows '()))
    (dotimes (y *grid-height*)
      (let ((row '()))
        (dotimes (x *grid-width*)
          (push (aref *grid* y x) row))
        (push (nreverse row) rows)))
    `(:width  ,*grid-width*
      :height ,*grid-height*
      :tiles  ,(nreverse rows))))

(defun save-level-to-file (pathname)
  "Mevcut level’i verilen pathname’e yaz."
  (with-open-file (out pathname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (print (level->sexp) out))))

(defun save-level-dialog ()
  "Kullanıcıdan dosya ismi iste ve oraya kaydet."
  (let* ((path (get-save-file :defaultextension ".lisp"
                              :filetypes '(("Lisp files" "*.lisp")
                                           ("All files" "*.*")))))
    (when path
      (save-level-to-file path)
      (format t "Level kaydedildi: ~A~%" path))))

;;;------------------------------------------------------------
;;; Canvas ve tile güncelleme
;;;------------------------------------------------------------

(defun update-tile-on-canvas (canvas gx gy)
  "Grid (gx,gy) hücresinin rengini canvas’ta güncelle."
  (let ((tile (aref *grid* gy gx))
        (rect (aref *rects* gy gx)))
    (when rect
      ;; LTK’de option isimleri string, renk de string olmalı
      (itemconfigure canvas rect "fill" (tile->color tile)))))

(defun handle-canvas-click (canvas evt)
  "Canvas’e tıklandığında çağrılır. Pikselden tile koordinatına çevirip toggle yapar."
  (multiple-value-bind (gx _) (floor (event-x evt) *tile-size*)
    (declare (ignore _))
    (multiple-value-bind (gy __) (floor (event-y evt) *tile-size*)
      (declare (ignore __))
      (when (and (>= gx 0) (< gx *grid-width*)
                 (>= gy 0) (< gy *grid-height*))
        ;; TIKLAMA DAVRANIŞI: toggle
        (let* ((old (aref *grid* gy gx))
               (new (if (eql old *current-tool*)
                        :empty
                        *current-tool*)))
          (setf (aref *grid* gy gx) new)
          (update-tile-on-canvas canvas gx gy))))))

(defun handle-canvas-paint (canvas evt)
  "Sol tuş basılı iken sürükleme (B1-Motion) ile boyama.
Toggle DEĞİL; hücreyi doğrudan *current-tool* ile boyar."
  (multiple-value-bind (gx _) (floor (event-x evt) *tile-size*)
    (declare (ignore _))
    (multiple-value-bind (gy __) (floor (event-y evt) *tile-size*)
      (declare (ignore __))
      (when (and (>= gx 0) (< gx *grid-width*)
                 (>= gy 0) (< gy *grid-height*))
        ;; SÜRÜKLEYEREK BOYAMA: direkt aracı uygula
        (setf (aref *grid* gy gx) *current-tool*)
        (update-tile-on-canvas canvas gx gy)))))

(defun init-canvas (parent)
  "Grid çizen ve tıklamaları yakalayan canvas yarat."
  (let* ((w (* *grid-width*  *tile-size*))
         (h (* *grid-height* *tile-size*))
         (canvas (make-instance 'canvas
                                :master parent
                                :width  w
                                :height h
                                :background "gray20")))
    ;; Hücreleri çiz ve rect id’lerini *rects* içine yaz
    (dotimes (row *grid-height*)
      (dotimes (col *grid-width*)
        (let* ((x0 (* col *tile-size*))
               (y0 (* row *tile-size*))
               (x1 (+ x0 *tile-size*))
               (y1 (+ y0 *tile-size*))
               (id (create-rectangle canvas x0 y0 x1 y1)))
          ;; Çerçeve ve dolgu renklerini ayarla
          (itemconfigure canvas id "outline" "gray30")
          (itemconfigure canvas id "fill" (tile->color :empty))
          ;; Canvas id’sini kaydet
          (setf (aref *rects* row col) id))))

    ;; Mouse sol tık: toggle
    (bind canvas "<Button-1>"
          (lambda (evt)
            (handle-canvas-click canvas evt)))

    ;; Mouse sol tuş basılı iken sürükleme: fırça gibi boyama
    (bind canvas "<B1-Motion>"
          (lambda (evt)
            (handle-canvas-paint canvas evt)))

    canvas))

;;;------------------------------------------------------------
;;; Toolbar (palette + kaydet)
;;;------------------------------------------------------------

(defun init-toolbar (master)
  "Üstte basit bir toolbar: araç seçimi + kaydet."
  (let ((frame (make-instance 'frame :master master)))
    ;; 1. satır: temel şeyler
    ;; Boş
    (let ((btn-empty (make-instance 'button
                          :master frame
                          :text "Boş"
                          :command (lambda ()
                                     (setf *current-tool* :empty)))))
      (grid btn-empty 0 0 :padx 4 :pady 4))

    ;; Zemin
    (let ((btn-ground (make-instance 'button
                           :master frame
                           :text "Zemin"
                           :command (lambda ()
                                      (setf *current-tool* :ground)))))
      (grid btn-ground 0 1 :padx 4 :pady 4))

    ;; Steel
    (let ((btn-steel (make-instance 'button
                          :master frame
                          :text "Steel"
                          :command (lambda ()
                                     (setf *current-tool* :steel)))))
      (grid btn-steel 0 2 :padx 4 :pady 4))

    ;; Spawn
    (let ((btn-spawn (make-instance 'button
                          :master frame
                          :text "Spawn"
                          :command (lambda ()
                                     (setf *current-tool* :spawn)))))
      (grid btn-spawn 0 3 :padx 4 :pady 4))

    ;; Exit
    (let ((btn-exit (make-instance 'button
                         :master frame
                         :text "Exit"
                         :command (lambda ()
                                    (setf *current-tool* :exit)))))
      (grid btn-exit 0 4 :padx 4 :pady 4))

    ;; 2. satır: diğer tipler
    ;; Kazılabilir
    (let ((btn-excav (make-instance 'button
                           :master frame
                           :text "Kazılabilir"
                           :command (lambda ()
                                      (setf *current-tool* :can-be-excavated)))))
      (grid btn-excav 1 0 :padx 4 :pady 4))

    ;; Contacts
    (let ((btn-contacts (make-instance 'button
                              :master frame
                              :text "Contacts"
                              :command (lambda ()
                                         (setf *current-tool* :contacts)))))
      (grid btn-contacts 1 1 :padx 4 :pady 4))

    ;; Dekor
    (let ((btn-decor (make-instance 'button
                           :master frame
                           :text "Dekor"
                           :command (lambda ()
                                      (setf *current-tool* :decorations-and-background-anims)))))
      (grid btn-decor 1 2 :padx 4 :pady 4))

    ;; Kaydet butonu (sağ tarafa)
    (let ((btn-save (make-instance 'button
                         :master frame
                         :text "Kaydet…"
                         :command #'save-level-dialog)))
      (grid btn-save 1 4 :padx 10 :pady 4))

    frame))

;;;------------------------------------------------------------
;;; Ana giriş fonksiyonu
;;;------------------------------------------------------------

(defun start-editor (&key (cols *grid-width*)
                          (rows *grid-height*)
                          (tile-size *tile-size*))
  "Basit Lemmings seviye editörü iskeleti: grid + toolbar.

Anahtar argümanlar:
  :cols      -> grid sütun sayısı
  :rows      -> grid satır sayısı
  :tile-size -> her hücrenin piksel boyutu

Örnek:
  (start-editor :cols 60 :rows 15 :tile-size 16)"
  ;; UI açılmadan önce geometriyi ayarla
  (reconfigure-grid :cols cols :rows rows :tile-size tile-size)
  (with-ltk ()
    ;; Ana pencere başlığı
    (wm-title *tk* "REAKT Lemmings Level Editor")

    ;; Ana frame
    (let* ((main    (make-instance 'frame :master *tk*))
           (toolbar (init-toolbar main))
           (canvas  (init-canvas main)))
      ;; Toolbar üstte, canvas altta
      (pack toolbar :side :top :fill :x)
      (pack canvas  :side :top :fill :both :expand t)
      (pack main    :side :top :fill :both :expand t))))
