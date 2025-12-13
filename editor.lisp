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
  (list :name             "Yeni Level"
        :lemmings-total   10
        :lemmings-required 10
        :time-limit-sec   300))

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
    (:spawn   "Çıkış Noktası (Spawn)"          "cyan3")
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

LEVEL formatı:
  (:width W :height H :tiles ((row0) (row1) ...))"
  (let* ((w (getf level :width))
         (h (getf level :height))
         (tiles (getf level :tiles)))
    ;; Basit format kontrolleri
    (unless (and (integerp w) (integerp h)
                 (> w 0) (> h 0)
                 (listp tiles))
      (error "Geçersiz level formatı: ~S" level))

    ;; Şimdilik: dosyadaki boyutlar mevcut grid ile aynı olmalı
    (when (or (/= w *grid-width*)
              (/= h *grid-height*))
      (error "Level boyutu uymuyor. Dosya: (~D x ~D), Editor: (~D x ~D)"
             w h *grid-width* *grid-height*))

    ;; Grid’i doldur
    (dotimes (y *grid-height*)
      (let ((row (nth y tiles)))
        (dotimes (x *grid-width*)
          (let* ((tile (and row (nth x row)))
                 ;; Bilinmeyen tile tiplerini :empty’ye düşürelim
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
          (format t "Level yüklendi: ~A~%" pathname)))
    (error (e)
      (format *error-output* "~&[LEVEL-LOAD] Hata: ~A~%" e))))

(defun load-level-dialog (&optional canvas)
  "Dosya seçim diyalogu aç ve seçilen level’i yükle."
  (let ((path (get-open-file
               :filetypes '(("Lisp files" "*.lisp")
                            ("All files" "*.*")))))
    (when path
      (load-level-from-file path canvas))))

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
                   (grid lbl    r 0 :sticky "w" :padx 4 :pady 2)
                   (grid widget r 1 :sticky "ew" :padx 4 :pady 2))))
        (row 0 "Level adı:"          name-entry)
        (row 1 "Toplam lemming:"    total-entry)
        (row 2 "Gerekli lemming:"   required-entry)
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
                                  (list :name             name
                                        :lemmings-total   total
                                        :lemmings-required required
                                        :time-limit-sec   time)))
                           ;; Ana pencere başlığını güncelle
                           (wm-title *tk*
                                     (format nil
                                             "REAKT Lemmings Level Editor - ~A"
                                             (getf *level-meta* :name)))
                           (destroy tl))))
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
            (let ((btn (make-instance 'button
				      :master frame
				      :text   label
				      :command (lambda ()
						 (setf *current-tool* key)))))
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
      (grid btn-load 0 n       :padx 10 :pady 4)
      (grid btn-save 0 (1+ n)  :padx 4  :pady 4)
      (grid btn-props 0 (+ n 2) :padx 4  :pady 4))
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
    (let* ((main   (make-instance 'frame :master *tk*))
           (canvas (init-canvas main))
           (toolbar (init-toolbar main)))
      ;; Canvas’ı globale yaz ki 'Yükle…' butonu kullanabilsin
      (setf *canvas* canvas)

      ;; Toolbar üstte, canvas altta
      (pack toolbar :side :top :fill :x)
      (pack canvas  :side :top :fill :both :expand t)
      (pack main    :side :top :fill :both :expand t))))

(defun apply-level-from-sexp (spec)
  "(:width ... :height ... :tile-size ... :meta ... :tiles ...) şeklindeki
s-exp’ten grid ve *level-meta*’yı doldurur.

Eski format dosyalarda (:tile-size ve :meta yoksa) varsayılan değerler kullanılır."
  (let* ((w     (or (getf spec :width)  *grid-width*))
         (h     (or (getf spec :height) *grid-height*))
         (ts    (or (getf spec :tile-size) *tile-size*))
         (meta  (normalize-level-meta (getf spec :meta)))
         (tiles (getf spec :tiles)))
    (reinit-grid w h ts)
    (setf *level-meta* meta)
    ;; tiles içeriğini *grid*’e yaz
    (when tiles
      (dotimes (y (min h (length tiles)))
        (let ((row (nth y tiles)))
          (when row
            (dotimes (x (min w (length row)))
              (setf (aref *grid* y x) (nth x row))))))))
  t)

(defun load-level-from-file (pathname)
  "Verilen dosyadaki s-exp level tanımını okuyup uygular."
  (handler-case
      (with-open-file (in pathname :direction :input)
        (let ((form (read in nil nil)))
          (if (null form)
              (progn
                (format *error-output*
                        "[LEVEL-LOAD] Hata: Dosya boş: ~A~%" pathname)
                nil)
              (progn
                (apply-level-from-sexp form)
                (setf *current-level-path* (truename pathname))
                (format t "Level yüklendi: ~A~%" *current-level-path*)
                t))))
    (error (e)
      (format *error-output* "[LEVEL-LOAD] Hata: ~A~%" e)
      nil)))

(defun load-level-dialog ()
  "Kullanıcıya dosya seçtirip level yükler."
  (let ((path (get-open-file
               :filetypes '(("Lisp level files" "*.lisp")
                            ("All files"       "*.*")))))
    (when path
      (load-level-from-file path))))

(defun ensure-level-loaded-or-new (cols rows tile-size)
  "Varsayılan level dosyası varsa onu yükler; yoksa yeni boş level oluşturur."
  (let ((path (default-level-path)))
    (if (probe-file path)
        ;; Dosya varsa: okumaya çalış
        (unless (load-level-from-file path)
          ;; Yükleme başarısızsa boş level oluştur
          (reinit-grid cols rows tile-size)
          (setf *level-meta* (normalize-level-meta nil)))
        ;; Dosya yoksa: boş level oluştur ve hemen kaydet
        (progn
          (reinit-grid cols rows tile-size)
          (setf *level-meta* (normalize-level-meta nil))
          (save-level-to-file path)))))

(defun start-editor (&key (cols 40) (rows 15) (tile-size 24))
  "Basit Lemmings seviye editörü: grid + toolbar.

İlk açılışta varsayılan test-level.lisp varsa onu yükler,
yoksa boş bir level oluşturup kaydeder."
  (with-ltk ()
    ;; Level verisini hazırla (grid + meta)
    (ensure-level-loaded-or-new cols rows tile-size)

    ;; Pencere başlığında level ismini göster
    (wm-title *tk*
              (format nil "REAKT Lemmings Level Editor - ~A"
                      (getf *level-meta* :name)))

    ;; Ana frame
    (let* ((main    (make-instance 'frame :master *tk*))
           (toolbar (init-toolbar main))
           (canvas  (init-canvas main)))
      (declare (ignore toolbar))
      ;; Toolbar üstte, canvas altta
      (pack toolbar :side :top :fill :x)
      (pack canvas  :side :top :fill :both :expand t)
      (pack main    :side :top :fill :both :expand t))))
