;;; editor.lisp

(in-package #:lemmings-editor)

;; (start-editor :cols 40 :rows 30 :tile-size 24)
;; tam ekran
;; (start-editor :cols 80 :rows 45 :tile-size 24) 

;; Kullanım
;; Ok tuşları: pan
;; Shift + ok: hızlı pan
;; + / =: zoom in
;; -: zoom out
;; 0: zoom reset (1.0)

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

(defun reinit-grid (cols rows tile-size)
  "Grid boyutunu ve array’leri yeniden oluştur."
  (setf *grid-width*  cols
        *grid-height* rows
        *tile-size*   tile-size
        *grid*  (make-array (list rows cols) :initial-element :empty)
        *rects* (make-array (list rows cols) :initial-element nil)))

(defparameter *rects* nil)  ;; canvas rectangle id’leri

(defparameter *canvas* nil) ;; aktif canvas objesi (load için kullanacağız)

;; Aktif araç (palette’den seçilecek)
(defparameter *current-tool* :ground)

;;;------------------------------------------------------------
;;; Level / metadata
;;;------------------------------------------------------------

;; Level özellikleri (metadata)
(defparameter *level-meta*
  (list :name              "Yeni Level"
        :lemmings-total    10
        :lemmings-required 10
        :time-limit-sec    300))

(defun normalize-level-meta (meta)
  "Eksik anahtarları varsayılanlarla doldur."
  (let ((meta (copy-list (or meta '()))))
    (labels ((ensure (key default)
               (unless (getf meta key)
                 (setf (getf meta key) default))))
      (ensure :name             "Yeni Level")
      (ensure :lemmings-total   10)
      (ensure :lemmings-required 10)
      (ensure :time-limit-sec   300))
    meta))

;; Hangi level dosyasıyla çalışıyoruz?
(defparameter *current-level-path* nil)

(defparameter *default-level-filename* "test-level.lisp")

(defun default-level-path ()
  "Proje dizininde varsayılan level dosyası."
  (merge-pathnames *default-level-filename*
                   *default-pathname-defaults*))

(defun parse-int-or-default (string default)
  (handler-case
      (let ((n (parse-integer string :junk-allowed t)))
        (if n n default))
    (error () default)))

;;;------------------------------------------------------------
;;; Tile tipleri ve renkler
;;;------------------------------------------------------------

(defparameter *tile-types*
  '((:empty   "Boş"                             "black")
    (:ground  "Zemin"                           "sienna3")
    (:steel   "Çelik (kırılamaz)"               "lightsteelblue3")
    (:spawn   "Çıkış Noktası (Spawn)"           "cyan3")
    (:exit    "Çıkış Kapısı"                    "gold")
    (:can-be-excavated "Kazılabilir zemin"      "chocolate3")
    (:contacts "Temas (tehlike/bonus)"          "red3")
    (:decorations-and-background-anims
               "Dekor / Arkaplan"               "forest green")))

(defparameter *current-tool* :ground)

(defun tile->color (tile)
  "Tile tipine göre Tk renk ismi döndür."
  (let ((entry (assoc tile *tile-types*)))
    (if entry
        (third entry)
        "magenta")))  ;; tanımsız tile’lar mor görünsün.

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
  "Şu anki *grid* ve *level-meta*’yı s-exp veri yapısına çevir.

Format:
  (:width W :height H :tile-size TS
   :meta (:name ... :lemmings-total ... ...)
   :tiles ((row0) (row1) ...))"
  (let ((rows '()))
    (dotimes (y *grid-height*)
      (let ((row '()))
        (dotimes (x *grid-width*)
          (push (aref *grid* y x) row))
        (push (nreverse row) rows)))
    (list :width      *grid-width*
          :height     *grid-height*
          :tile-size  *tile-size*
          :meta       *level-meta*
          :tiles      (nreverse rows))))

(defun save-level-to-file (pathname)
  "Mevcut level’i verilen pathname’e yaz ve *current-level-path*’i güncelle."
  (with-open-file (out pathname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (print (level->sexp) out)))
  (setf *current-level-path* (truename pathname))
  (format t "Level kaydedildi: ~A~%" *current-level-path*)
  t)

(defun save-level-dialog ()
  "Kullanıcıdan dosya ismi iste ve oraya kaydet."
  (let* ((path (get-save-file
                :filetypes '(("Lisp level files" "*.lisp")
                             ("All files"       "*.*")))))
    (when path
      (save-level-to-file path))))

;;;------------------------------------------------------------
;;; Level yükleme (dosyadan okuma)
;;;------------------------------------------------------------

(defun apply-level-to-current-grid (level &optional canvas)
  "LEVEL s-exp’ini mevcut *grid* (+ opsiyonel CANVAS) üzerine uygular.

LEVEL formatı eski / yeni olabilir:
  (:width W :height H :tiles ((row0) (row1) ...))
veya
  (:width W :height H :tile-size TS
   :meta (:name ... :lemmings-total ... ...)
   :tiles ((row0) (row1) ...))"
  (let* ((w     (getf level :width))
         (h     (getf level :height))
         (tiles (getf level :tiles))
         (meta  (normalize-level-meta (getf level :meta)))
         (ts    (or (getf level :tile-size) *tile-size*)))
    ;; Basit format kontrolleri
    (unless (and (integerp w) (integerp h)
                 (> w 0) (> h 0)
                 (listp tiles))
      (error "Geçersiz level formatı: ~S" level))

    ;; Geometri uyumu kontrolü (şimdilik zorunlu)
    (when (or (/= w *grid-width*)
              (/= h *grid-height*))
      (error "Level boyutu uymuyor. Dosya: (~D x ~D), Editor: (~D x ~D)"
             w h *grid-width* *grid-height*))

    ;; Tile-size farklıysa şimdilik sadece uyar, değeri koru.
    (when (/= ts *tile-size*)
      (format *error-output*
              "~&[LEVEL-LOAD] Uyarı: tile-size dosyada ~D, editörde ~D.~%"
              ts *tile-size*))

    ;; metadata'yı güncelle
    (setf *level-meta* meta)
    (when canvas
      (wm-title *tk*
                (format nil
                        "REAKT Lemmings Level Editor - ~A"
                        (getf *level-meta* :name))))

    ;; Grid’i doldur
    (dotimes (y *grid-height*)
      (let ((row (nth y tiles)))
        (dotimes (x *grid-width*)
          (let* ((tile (and row (nth x row)))
                 ;; Bilinmeyen tile tiplerini :empty'ye düşürelim
                 (safe-tile (if (member tile
                                        '(:empty :ground :steel :spawn :exit
                                          :can-be-excavated :contacts
                                          :decorations-and-background-anims))
                                tile
                                :empty)))
            (setf (aref *grid* y x) safe-tile)
            (when canvas
              (update-tile-on-canvas canvas x y))))))))

(defun load-level-from-file (pathname &optional canvas)
  "PATHNAME’den level s-exp’ini oku ve mevcut grid’e uygula."
  (handler-case
      (with-open-file (in pathname :direction :input)
        (let ((form (read in nil nil)))
          (unless form
            (error "Dosya boş: ~A" pathname))
          (apply-level-to-current-grid form canvas)
          (setf *current-level-path* (truename pathname))
          (format t "Level yüklendi: ~A~%" *current-level-path*)
          t))
    (error (e)
      (format *error-output* "~&[LEVEL-LOAD] Hata: ~A~%" e)
      nil)))

(defun load-level-dialog ()
  "Dosya seçim diyalogu aç ve seçilen level’i yükle."
  (let ((path (get-open-file
               :filetypes '(("Lisp level files" "*.lisp")
                            ("All files" "*.*")))))
    (when path
      (load-level-from-file path *canvas*))))

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

;;;------------------------------------------------------------
;;; Canvas kamera (pan) + zoom  (LTK sürüm bağımsız: SEND-WISH)
;;;------------------------------------------------------------

(defparameter *scroll-step-tiles* 1)
(defparameter *scroll-step-tiles-fast* 6)

;; Kamera offset’i (ekranda görünen sol-üst köşenin world px karşılığı)
(defparameter *canvas-cam-x* 0.0)
(defparameter *canvas-cam-y* 0.0)

;; Zoom (1.0 = normal). Tile-size küçükken büyütmek için.
(defparameter *canvas-zoom* 1.0)
(defparameter *canvas-zoom-min* 0.25)
(defparameter *canvas-zoom-max* 8.0)
(defparameter *canvas-zoom-step* 0.25)

(defun effective-tile-size ()
  "Ekrandaki (zoom uygulanmış) tile piksel boyutu."
  (* (float *tile-size* 1.0) (float *canvas-zoom* 1.0)))

(defun %canvas-tk-path (canvas)
  (let* ((path-sym (%ltk-fbound "WIDGET-PATH")))
    (or (and path-sym (funcall (symbol-function path-sym) canvas))
        (ignore-errors (slot-value canvas (find-symbol "PATH" (find-package :ltk))))
        (ignore-errors (slot-value canvas (find-symbol "NAME" (find-package :ltk))))
        (error "Canvas Tk path bulunamadı."))))

(defun %send-wish (fmt &rest args)
  (let ((send-sym (%ltk-fbound "SEND-WISH")))
    (unless send-sym
      (error "LTK'da SEND-WISH bulunamadı."))
    (funcall (symbol-function send-sym)
             (apply #'format nil fmt args))))

(defun %canvas-scale-all (canvas ratio)
  "Canvas içindeki tüm item’ları oranla ölçekle. (0,0 etrafında)"
  (let ((path (%canvas-tk-path canvas)))
    ;; ratio float olabilir
    (%send-wish "~a scale all 0 0 ~,6F ~,6F" path (float ratio 1.0) (float ratio 1.0))
    t))

(defun set-canvas-zoom! (canvas new-zoom)
  "Zoom değiştir: tüm item’ları scale et + kamera offset’ini aynı oranda çarp."
  (let* ((old (float *canvas-zoom* 1.0))
         (nz  (float new-zoom 1.0))
         (nz  (max *canvas-zoom-min* (min *canvas-zoom-max* nz)))
         (ratio (if (zerop old) 1.0 (/ nz old))))
    (when (and canvas (not (= ratio 1.0)))
      (%canvas-scale-all canvas ratio)
      (setf *canvas-cam-x* (* *canvas-cam-x* ratio)
            *canvas-cam-y* (* *canvas-cam-y* ratio)))
    (setf *canvas-zoom* nz)
    nz))

(defun zoom-in! (canvas)
  (set-canvas-zoom! canvas (+ *canvas-zoom* *canvas-zoom-step*)))

(defun zoom-out! (canvas)
  (set-canvas-zoom! canvas (- *canvas-zoom* *canvas-zoom-step*)))

(defun zoom-reset! (canvas)
  (set-canvas-zoom! canvas 1.0))

(defun default-zoom-for-tile-size (ts)
  "Tile-size çok küçükse başlangıçta otomatik büyüt."
  (let ((z (/ 24.0 (max 1.0 (float ts 1.0)))))
    (max 1.0 (min 6.0 z))))

(defun canvas-pan-tiles (canvas dx-tiles dy-tiles &key fast?)
  "Ok tuşlarıyla pan: dünyayı kaydır (kamera). Zoom’a göre px hesabı."
  (let* ((step   (if fast? *scroll-step-tiles-fast* *scroll-step-tiles*))
         (tilepx (effective-tile-size))
         (dx     (* (float dx-tiles 1.0) (float step 1.0) tilepx))
         (dy     (* (float dy-tiles 1.0) (float step 1.0) tilepx)))
    (when (or (/= dx 0.0) (/= dy 0.0))
      (incf *canvas-cam-x* dx)
      (incf *canvas-cam-y* dy)
      ;; görünen sabit kalsın diye item'ları ters yönde kaydır
      (%canvas-move-all canvas (- dx) (- dy)))))

;;;------------------------------------------------------------
;;; Canvas "kamera" (scroll) + event coord fix
;;;------------------------------------------------------------

(defparameter *scroll-step-tiles* 1)
(defparameter *scroll-step-tiles-fast* 6)

;;;------------------------------------------------------------
;;; Canvas "kamera" (pan) + event coord fix  (LTK sürüm bağımsız)
;;;------------------------------------------------------------

(defparameter *canvas-cam-x* 0.0)  ;; world pixel offset (viewport origin -> world)
(defparameter *canvas-cam-y* 0.0)

(defparameter *scroll-step-tiles* 1)
(defparameter *scroll-step-tiles-fast* 6)

(defun %canvas-move-all (canvas dx dy)
  "Canvas içindeki tüm item’ları dx/dy kadar kaydır. (HER ZAMAN send-wish)"
  (let ((path (%canvas-tk-path canvas)))
    (%send-wish "~a move all ~,6F ~,6F"
                path
                (float dx 1.0)
                (float dy 1.0))
    t))

(defun handle-canvas-click (canvas evt)
  "Canvas’e tıkla: (kamera+zoom) world’e çevirip tile bulur."
  (focus canvas)
  (multiple-value-bind (world-x world-y) (event->world-xy evt)
    (let ((tile-px (effective-tile-size)))
      (multiple-value-bind (grid-x remainder-x) (floor world-x tile-px)
        (declare (ignore remainder-x))
        (multiple-value-bind (grid-y remainder-y) (floor world-y tile-px)
          (declare (ignore remainder-y))
          (when (and (>= grid-x 0) (< grid-x *grid-width*)
                     (>= grid-y 0) (< grid-y *grid-height*))
            (let* ((old (aref *grid* grid-y grid-x))
                   (new (if (eql old *current-tool*) :empty *current-tool*)))
              (setf (aref *grid* grid-y grid-x) new)
              (update-tile-on-canvas canvas grid-x grid-y))))))))

(defun handle-canvas-paint (canvas evt)
  "Sol tuş basılı sürükleme: fırça. (kamera+zoom) world’e çevir."
  (focus canvas)
  (multiple-value-bind (world-x world-y) (event->world-xy evt)
    (let ((tile-px (effective-tile-size)))
      (multiple-value-bind (grid-x remainder-x) (floor world-x tile-px)
        (declare (ignore remainder-x))
        (multiple-value-bind (grid-y remainder-y) (floor world-y tile-px)
          (declare (ignore remainder-y))
          (when (and (>= grid-x 0) (< grid-x *grid-width*)
                     (>= grid-y 0) (< grid-y *grid-height*))
            (setf (aref *grid* grid-y grid-x) *current-tool*)
            (update-tile-on-canvas canvas grid-x grid-y)))))))

;; ------------------------------------------------------------
;; Canvas "kamera" (pan)  (LTK sürüm bağımsız)
;; ------------------------------------------------------------

(defparameter *scroll-step-tiles* 1)
(defparameter *scroll-step-tiles-fast* 6)

(defparameter *canvas-cam-x* 0.0)  ;; world pixel offset (viewport origin -> world)
(defparameter *canvas-cam-y* 0.0)

(defun reset-canvas-camera! ()
  (setf *canvas-cam-x* 0.0
        *canvas-cam-y* 0.0))

(defun %ltk-fbound (name)
  "LTK paketinde NAME isimli fbound sembolü bul (export edilse de edilmese de)."
  (let ((pkg (find-package :ltk)))
    (when pkg
      (multiple-value-bind (sym status) (find-symbol name pkg)
        (declare (ignore status))
        (when (and sym (fboundp sym)) sym)))))

(defun event->world-xy (evt)
  "Event (window) coords -> world coords (kamera offset eklenmiş)."
  (values (+ (float (event-x evt) 1.0) *canvas-cam-x*)
          (+ (float (event-y evt) 1.0) *canvas-cam-y*)))

(defun init-canvas (parent)
  "Grid çizen ve tıklamaları yakalayan canvas yarat.
Ok tuşları: pan.  +/- : zoom. 0: reset zoom."
  (let* ((canvas (make-instance 'canvas
                                :master parent
                                :width  800
                                :height 600
                                :background "gray20")))
    (setf *canvas* canvas)

    ;; Kamera/zoom başlangıç
    (reset-canvas-camera!)
    (setf *canvas-zoom* 1.0)

    ;; rect'leri oluştur (BASE coords: *tile-size* ile)
    (dotimes (gy *grid-height*)
      (dotimes (gx *grid-width*)
        (let* ((x0 (* gx *tile-size*))
               (y0 (* gy *tile-size*))
               (x1 (+ x0 *tile-size*))
               (y1 (+ y0 *tile-size*))
               (tile (aref *grid* gy gx))
               (rect (create-rectangle canvas x0 y0 x1 y1)))
          (setf (aref *rects* gy gx) rect)
          (itemconfigure canvas rect "fill" (tile->color tile))
          (itemconfigure canvas rect "outline" "gray35"))))

    (pack canvas :side :top :fill :both :expand t)

    ;; Tile-size küçükse otomatik zoom (8 gibi)
    (set-canvas-zoom! canvas (default-zoom-for-tile-size *tile-size*))

    ;; focus
    (bind canvas "<Enter>"
          (lambda (evt) (declare (ignore evt)) (focus canvas)))

    ;; mouse
    (bind canvas "<Button-1>"
          (lambda (evt) (handle-canvas-click canvas evt)))
    (bind canvas "<B1-Motion>"
          (lambda (evt) (handle-canvas-paint canvas evt)))

    ;; ok tuşları: kamera
    (bind canvas "<KeyPress-Up>"    (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas 0 -1)))
    (bind canvas "<KeyPress-Down>"  (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas 0  1)))
    (bind canvas "<KeyPress-Left>"  (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas -1 0)))
    (bind canvas "<KeyPress-Right>" (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas  1 0)))

    ;; Shift+Ok: hızlı
    (bind canvas "<Shift-KeyPress-Up>"    (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas 0 -1 :fast? t)))
    (bind canvas "<Shift-KeyPress-Down>"  (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas 0  1 :fast? t)))
    (bind canvas "<Shift-KeyPress-Left>"  (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas -1 0 :fast? t)))
    (bind canvas "<Shift-KeyPress-Right>" (lambda (evt) (declare (ignore evt)) (canvas-pan-tiles canvas  1 0 :fast? t)))

    ;; ZOOM: + / = (zoom in), - (zoom out), 0 (reset)
    (bind canvas "<KeyPress-plus>"  (lambda (evt) (declare (ignore evt)) (zoom-in! canvas)))
    (bind canvas "<KeyPress-equal>" (lambda (evt) (declare (ignore evt)) (zoom-in! canvas)))
    (bind canvas "<KeyPress-minus>" (lambda (evt) (declare (ignore evt)) (zoom-out! canvas)))
    (bind canvas "<KeyPress-0>"     (lambda (evt) (declare (ignore evt)) (zoom-reset! canvas)))

    (focus canvas)
    canvas))

;;;------------------------------------------------------------
;;; Toolbar (palette + kaydet + özellikler)
;;;------------------------------------------------------------

(defun open-level-properties-dialog ()
  "Level adı / lemming sayıları / süre için basit bir özellikler penceresi."
  (let* ((tl    (make-instance 'toplevel :master *tk*))
         (frame (make-instance 'frame :master tl)))
    (wm-title tl "Seviye Özellikleri")
    (pack frame :side :top :fill :both :expand t)

    ;; Entry widget’larını oluştur
    (let* ((name-entry     (make-instance 'entry :master frame))
           (total-entry    (make-instance 'entry :master frame))
           (required-entry (make-instance 'entry :master frame))
           (time-entry     (make-instance 'entry :master frame)))
      ;; Mevcut değerleri entry’lere yaz
      (setf (text name-entry)
            (or (getf *level-meta* :name) "Adsız level"))
      (setf (text total-entry)
            (princ-to-string (or (getf *level-meta* :lemmings-total) 10)))
      (setf (text required-entry)
            (princ-to-string (or (getf *level-meta* :lemmings-required) 10)))
      (setf (text time-entry)
            (princ-to-string (or (getf *level-meta* :time-limit-sec) 300)))

      ;; Satır yerleşimi
      (labels ((row (r label widget)
                 (let ((lbl (make-instance 'label :master frame :text label)))
                   (grid lbl    r 0 :sticky "w"  :padx 4 :pady 2)
                   (grid widget r 1 :sticky "ew" :padx 4 :pady 2))))
        (row 0 "Level adı:"              name-entry)
        (row 1 "Toplam lemming:"        total-entry)
        (row 2 "Gerekli lemming:"       required-entry)
        (row 3 "Süre (sn, 0=limitsiz):" time-entry))

      (grid-columnconfigure frame 1 :weight 1)

      ;; OK / İptal butonları
      (let ((btn-frame (make-instance 'frame :master frame)))
        (grid btn-frame 4 0 :columnspan 2 :sticky "e" :padx 4 :pady 4)
        (let ((ok (make-instance 'button
                      :master btn-frame
                      :text "Tamam"
                      :command
                      (lambda ()
                        (let* ((name (string-trim " " (or (text name-entry) "")))
                               (total (parse-int-or-default
                                       (text total-entry)
                                       (or (getf *level-meta* :lemmings-total)
                                           10)))
                               (required (parse-int-or-default
                                          (text required-entry)
                                          (or (getf *level-meta* :lemmings-required)
                                              10)))
                               (time (parse-int-or-default
                                      (text time-entry)
                                      (or (getf *level-meta* :time-limit-sec)
                                          300))))
                          (setf *level-meta*
                                (normalize-level-meta
                                 (list :name              name
                                       :lemmings-total    total
                                       :lemmings-required required
                                       :time-limit-sec    time)))
                          ;; Ana pencere başlığını güncelle
                          (wm-title *tk*
                                    (format nil
                                            "REAKT Lemmings Level Editor - ~A"
                                            (getf *level-meta* :name)))
                          (destroy tl)))))
              (cancel (make-instance 'button
                        :master btn-frame
                        :text "İptal"
                        :command (lambda () (destroy tl)))))
          (pack ok     :side :right :padx 4 :pady 2)
          (pack cancel :side :right :padx 4 :pady 2))))))

(defun init-toolbar (master)
  "Üstte basit bir toolbar: araç seçimi + Aç/Kaydet + Özellikler."
  (let* ((frame (make-instance 'frame :master master))
         (n     (length *tile-types*)))
    ;; Tile butonları (Boş, Zemin, Çelik, Spawn, Exit, ...)
    (loop for (key label _color) in *tile-types*
          for col from 0 do
            ;; ÖNEMLİ: key'i her buton için ayrı bir değişkende yakalıyoruz
            (let* ((tool-key key)
                   (btn (make-instance 'button
                                       :master frame
                                       :text   label
                                       :command (lambda ()
                                                  (setf *current-tool* tool-key)))))
              (grid btn 0 col :padx 4 :pady 4)))

    ;; Aç / Kaydet / Özellikler butonları
    (let ((btn-load (make-instance 'button
                                   :master frame
                                   :text "Aç…"
                                   :command #'load-level-dialog))
          (btn-save (make-instance 'button
                                   :master frame
                                   :text "Kaydet…"
                                   :command #'save-level-dialog))
          (btn-props (make-instance 'button
                                    :master frame
                                    :text "Özellikler…"
                                    :command #'open-level-properties-dialog)))
      (grid btn-load 0 n        :padx 10 :pady 4)
      (grid btn-save 0 (1+ n)   :padx 4  :pady 4)
      (grid btn-props 0 (+ n 2) :padx 4  :pady 4))
    frame))

;;;------------------------------------------------------------
;;; Ana giriş fonksiyonu
;;;------------------------------------------------------------

(defun start-editor (&key (cols *grid-width*)
                          (rows *grid-height*)
                          (tile-size *tile-size*))
  "Basit Lemmings seviye editörü: grid + toolbar.

Anahtar argümanlar:
  :cols      -> grid sütun sayısı
  :rows      -> grid satır sayısı
  :tile-size -> her hücrenin piksel boyutu

Örnek:
  (start-editor :cols 60 :rows 15 :tile-size 16)"
  ;; UI açılmadan önce geometriyi ayarla
  (reconfigure-grid :cols cols :rows rows :tile-size tile-size)
  (with-ltk ()
    ;; Ana pencere başlığı (mevcut metadata'dan)
    (wm-title *tk*
              (format nil "REAKT Lemmings Level Editor - ~A"
                      (getf *level-meta* :name)))

    ;; Ana frame
    (let* ((main   (make-instance 'frame :master *tk*))
           (toolbar (init-toolbar main))
           (canvas (init-canvas main)))
      ;; Canvas’ı globale yaz ki 'Aç…' butonu kullanabilsin
      (setf *canvas* canvas)

      ;; Toolbar üstte, canvas altta
      (pack toolbar :side :top :fill :x)
      (pack canvas  :side :top :fill :both :expand t)
      (pack main    :side :top :fill :both :expand t))))
