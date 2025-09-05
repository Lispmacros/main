(defun c:СКУД_линия_и_обновление (/ *error* main-layer controller-block controller-ptag
                                  controller-tag start-pt polyline-points continue
                                  next-pt last-pt frame-block blocks-in-frame
                                  frame-number new-number base-number frame-bounds)

  ; Определение слоя для линий
  (setq main-layer "СКУД")
  
  ; Функция обработки ошибок
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )
  
  ; Проверка существования слоя, создание если нет
  (if (not (tblsearch "LAYER" main-layer))
    (command "._-LAYER" "_M" main-layer "_C" "1" main-layer "")
  )
  
  ; Выбор контроллера (главного блока)
  (princ "\nВыберите контроллер (главный блок): ")
  (setq controller-block (car (entsel)))
  
  (if (not controller-block)
    (progn
      (princ "\nКонтроллер не выбран!")
      (exit)
    )
  )
  
  ; Получение атрибутов контроллера
  (setq controller-ptag (get-block-attribute-value controller-block "P_TAG")
        controller-tag (get-block-attribute-value controller-block "TAG"))
  
  (if (not controller-ptag)
    (setq controller-ptag controller-tag)
  )
  
  (princ (strcat "\nP_TAG контроллера: " controller-ptag))
  
  ; Запрос номера рамки у пользователя
  (setq frame-number (get-frame-number))
  
  (if (not frame-number)
    (progn
      (princ "\nНомер рамки не указан!")
      (exit)
    )
  )
  
  ; Извлекаем базовый номер из P_TAG контроллера
  (setq base-number (get-base-number controller-ptag))
  
  (if (not base-number)
    (progn
      (princ "\nНе удалось извлечь базовый номер из P_TAG контроллера!")
      (exit)
    )
  )
  
  ; Формируем полный номер для рамки
  (setq new-number (strcat base-number "." frame-number))
  
  (princ (strcat "\nНомер для рамки: " new-number))
  
  ; Рисование ломаной линии от контроллера
  (princ "\nРисуйте ломаную линию от контроллера к рамке...")
  (princ "\nУкажите точки линии (Enter для завершения):")
  
  ; Начальная точка - точка вставки контроллера
  (setq start-pt (get-block-insertion-point controller-block))
  (setq polyline-points (list start-pt))
  (setq last-pt start-pt)
  
  ; Установка слоя и цвета
  (command "._-LAYER" "_S" main-layer "")
  (command "._COLOR" "1")
  
  ; Рисование линии с использованием getpoint в цикле
  (setq continue T)
  
  (while continue
    (initget "Enter")
    (setq next-pt (getpoint last-pt "\nСледующая точка или Enter для завершения: "))
    
    (if next-pt
      (progn
        (command "._LINE" last-pt next-pt "")
        (setq last-pt next-pt)
        (setq polyline-points (cons next-pt polyline-points))
      )
      (setq continue nil)
    )
  )
  
  ; Автоматический поиск рамки по конечной точке линии
  (setq frame-block (find-frame-at-point last-pt))
  
  (if (not frame-block)
    (progn
      (princ "\nРамка не найдена в конечной точке линии!")
      (exit)
    )
  )
  
  ; Получаем границы рамки
  (setq frame-bounds (get-frame-bounds frame-block))
  
  ; Выбираем блоки ТОЛЬКО внутри конкретной рамки
  (setq blocks-in-frame (ssget "_WP" frame-bounds '((0 . "INSERT"))))
  
  ; Преобразуем selection set в список
  (setq blocks-in-frame (ss->list blocks-in-frame))
  
  ; ФИЛЬТРУЕМ БЛОКИ: оставляем только те, которые действительно находятся внутри рамки
  (setq blocks-in-frame (filter-blocks-inside-frame blocks-in-frame frame-block))
  
  ; Удаляем контроллер из списка блоков для обновления
  (setq blocks-in-frame (vl-remove controller-block blocks-in-frame))
  
  ; Обновляем атрибуты P_TAG
  (update-only-ptag-attributes blocks-in-frame new-number)
  
  (princ "\nОбновление завершено!")
  (princ)
)

;;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ;;;

; Функция фильтрации блоков внутри рамки
(defun filter-blocks-inside-frame (blocks-list frame-ent / filtered-list blk blk-pt)
  (setq filtered-list '())
  
  (foreach blk blocks-list
    (setq blk-pt (get-block-insertion-point blk))
    
    ; Проверяем, находится ли точка вставки блока внутри рамки
    (if (and blk-pt (point-inside-frame-p blk-pt frame-ent))
      (setq filtered-list (cons blk filtered-list))
    )
  )
  
  (reverse filtered-list)
)

; Функция проверки, находится ли точка внутри рамки
(defun point-inside-frame-p (pt frame-ent / frame-type frame-points)
  (setq frame-type (cdr (assoc 0 (entget frame-ent))))
  
  (cond
    ((wcmatch frame-type "LWPOLYLINE,POLYLINE")
     ; Для полилинии используем алгоритм проверки точки внутри полигона
     (setq frame-points (get-polyline-points frame-ent))
     (point-inside-polygon-p pt frame-points))
    
    ((= frame-type "INSERT")
     ; Для блока используем bounding box
     (point-inside-bbox-p pt frame-ent))
    
    (t nil)
  )
)

; Алгоритм проверки точки внутри полигона (ray casting)
(defun point-inside-polygon-p (pt points / i j n inside x yi yj)
  (setq n (length points)
        i 0
        j (1- n)
        inside nil
        x (car pt)
        y (cadr pt))
  
  (while (< i n)
    (setq yi (cadr (nth i points))
          yj (cadr (nth j points)))
    
    (if (and (< yi y) (>= yj y) (or (> yi yj) (<= yi yj)))
      (setq x-int (+ (car (nth i points))
                     (/ (* (- (car (nth j points)) (car (nth i points)))
                           (- y yi))
                        (- yj yi))))
      
      (if (<= x x-int)
        (setq inside (not inside))
      )
    )
    
    (setq j i
          i (1+ i))
  )
  
  inside
)

; Проверка точки внутри bounding box блока
(defun point-inside-bbox-p (pt block-ent / minpt maxpt)
  (vla-getboundingbox (vlax-ename->vla-object block-ent) 'minpt 'maxpt)
  (setq minpt (vlax-safearray->list minpt)
        maxpt (vlax-safearray->list maxpt))
  
  (and (<= (car minpt) (car pt) (car maxpt))
       (<= (cadr minpt) (cadr pt) (cadr maxpt)))
)

; Функция проверки, является ли символ буквой (включая кириллицу)
(defun is-alpha-char (char)
  (or (wcmatch char "[A-Za-z]")
      (wcmatch char "[А-Яа-я]"))
)

; Функция проверки, является ли символ цифрой
(defun is-digit-char (char)
  (wcmatch char "[0-9]")
)

; Функция для получения точек полилинии/блока рамки
(defun get-frame-bounds (ent / ent-type points)
  (setq ent-type (cdr (assoc 0 (entget ent))))
  
  (cond
    ((wcmatch ent-type "LWPOLYLINE,POLYLINE")
     (get-polyline-points ent))
    ((= ent-type "INSERT")
     (get-block-points ent))
    (t nil)
  )
)

; Функция для получения точек полилинии
(defun get-polyline-points (ent / ent-data points)
  (setq ent-data (entget ent)
        points '())
  
  (foreach item ent-data
    (if (= (car item) 10)
      (setq points (cons (cdr item) points))
    )
  )
  
  (reverse points)
)

; Функция для получения точек блока (по bounding box)
(defun get-block-points (ent / minpt maxpt)
  (vla-getboundingbox (vlax-ename->vla-object ent) 'minpt 'maxpt)
  (setq minpt (vlax-safearray->list minpt)
        maxpt (vlax-safearray->list maxpt))
  
  (list minpt
        (list (car maxpt) (cadr minpt))
        maxpt
        (list (car minpt) (cadr maxpt)))
)

; Функция преобразования selection set в список
(defun ss->list (ss / i result)
  (setq i 0
        result '())
  
  (if ss
    (repeat (sslength ss)
      (setq result (cons (ssname ss i) result)
            i (1+ i))
    )
  )
  
  (reverse result)
)

; Функция обновления только атрибутов P_TAG
(defun update-only-ptag-attributes (blocks-list new-number / blk attrs ent-data old-ptag new-ptag has-space i char)
  (foreach blk blocks-list
    (setq attrs (entnext blk))
    (while attrs
      (setq ent-data (entget attrs))
      (if (and (= (cdr (assoc 0 ent-data)) "ATTRIB")
               (wcmatch (strcase (cdr (assoc 2 ent-data))) "P_TAG"))
        (progn
          ; Получаем текущее значение атрибута
          (setq old-ptag (cdr (assoc 1 ent-data)))
          
          ; Проверяем наличие пробела между буквенной и числовой частями
          (setq has-space nil)
          (if (and old-ptag (> (strlen old-ptag) 1))
            (progn
              (setq i 1)
              (while (and (<= i (strlen old-ptag)) (not has-space))
                (setq char (substr old-ptag i 1))
                
                ; Ищем позицию, где буква сменяется цифрой через пробел
                (if (and (> i 1)
                         (is-alpha-char (substr old-ptag (1- i) 1))
                         (eq char " ")
                         (< i (strlen old-ptag))
                         (is-digit-char (substr old-ptag (1+ i) 1)))
                  (setq has-space t)
                )
                (setq i (1+ i))
              )
            )
          )
          
          ; Обновляем только если есть пробел между буквами и цифрами
          (if has-space
            (progn
              ; Формируем новое значение с сохранением префикса
              (setq new-ptag (format-new-ptag old-ptag new-number))
              
              (if new-ptag
                (progn
                  (entmod (subst (cons 1 new-ptag) (assoc 1 ent-data) ent-data))
                  (entupd attrs)
                  (princ (strcat "\nОбновлен P_TAG: " old-ptag " -> " new-ptag))
                )
              )
            )
            (princ (strcat "\nПропущен блок (нет пробела в P_TAG): " old-ptag))
          )
        )
      )
      (setq attrs (entnext attrs))
    )
  )
  (princ)
)

; Функция запроса номера рамки у пользователя
(defun get-frame-number (/ input)
  (initget 1) ; Запрет пустого ввода
  (setq input (getstring "\nВведите номер рамки (только цифры): "))
  
  ; Проверяем, что введены только цифры
  (if (numberp (read input))
    input
    (progn
      (princ "\nОшибка: необходимо ввести только цифры!")
      (get-frame-number) ; Рекурсивный вызов для повторного ввода
    )
  )
)

; Функция извлечения базового номера из P_TAG
(defun get-base-number (ptag / result char i has-digit)
  (if ptag
    (progn
      (setq result "")
      (setq i 1)
      (setq has-digit nil)
      
      (while (<= i (strlen ptag))
        (setq char (substr ptag i 1))
        
        (if (wcmatch char "[0-9.]")
          (progn
            (setq result (strcat result char))
            (if (wcmatch char "[0-9]")
              (setq has-digit T)
            )
          )
          (if has-digit
            (setq i (strlen ptag)) ; Прерываем если уже были цифры и встретили не-цифру
          )
        )
        (setq i (1+ i))
      )
      
      ; Убираем точку в конце если есть
      (if (and (> (strlen result) 0) 
               (wcmatch (substr result (strlen result) 1) "."))
        (setq result (substr result 1 (1- (strlen result))))
      )
      
      (if (and (> (strlen result) 0) has-digit)
        result
        nil
      )
    )
    nil
  )
)

; Функция формирования нового P_TAG с сохранением префикса
(defun format-new-ptag (old-ptag new-number / prefix i char has-digit)
  (if old-ptag
    (progn
      ; Извлекаем буквенный префикс (все до первой цифры или точки)
      (setq prefix "")
      (setq i 1)
      (setq has-digit nil)
      
      (while (and (<= i (strlen old-ptag)) 
                 (not (wcmatch (substr old-ptag i 1) "[0-9.]")))
        (setq prefix (strcat prefix (substr old-ptag i 1)))
        (setq i (1+ i))
      )
      
      ; Убираем пробелы в конце префикса
      (while (and (> (strlen prefix) 0) 
                 (wcmatch (substr prefix (strlen prefix) 1) "[ ]"))
        (setq prefix (substr prefix 1 (1- (strlen prefix))))
      )
      
      ; Формируем новый P_TAG: префикс + пробел + номер
      (if (> (strlen prefix) 0)
        (strcat prefix " " new-number)
        new-number
      )
    )
    new-number
  )
)

; Функция поиска рамки по точке
(defun find-frame-at-point (pt / ss i ent ent-type found-frame)
  (setq found-frame nil)
  (setq ss (ssget "_C" (list (- (car pt) 0.1) (- (cadr pt) 0.1)) 
                         (list (+ (car pt) 0.1) (+ (cadr pt) 0.1))
                         '((0 . "INSERT,LWPOLYLINE,POLYLINE"))))
  
  (if ss
    (progn
      (setq i 0)
      (while (and (< i (sslength ss)) (not found-frame))
        (setq ent (ssname ss i))
        (setq ent-type (cdr (assoc 0 (entget ent))))
        
        ; Ищем рамку (блок или полилинию)
        (if (or (= ent-type "INSERT") 
                (= ent-type "LWPOLYLINE") 
                (= ent-type "POLYLINE"))
          (setq found-frame ent)
        )
        (setq i (1+ i))
      )
    )
  )
  found-frame
)

; Функция получения значения атрибута блока
(defun get-block-attribute-value (ent attr-name / attrs att-value)
  (setq attrs (get-block-attributes ent))
  (setq att-value (cdr (assoc attr-name attrs)))
  att-value
)

; Функция получения всех атрибутов блока
(defun get-block-attributes (ent / ent-data attrs att-list)
  (setq ent-data (entget ent))
  (if (= (cdr (assoc 0 ent-data)) "INSERT")
    (progn
      (setq attrs (entnext ent))
      (while (and attrs (= (cdr (assoc 0 (entget attrs))) "ATTRIB"))
        (setq att-list (cons (cons (cdr (assoc 2 (entget attrs))) 
                                  (cdr (assoc 1 (entget attrs)))) 
                            att-list))
        (setq attrs (entnext attrs))
      )
    )
  )
  att-list
)

; Функция получения точки вставки блока
(defun get-block-insertion-point (ent / ent-data)
  (setq ent-data (entget ent))
  (if (and (= (cdr (assoc 0 ent-data)) "INSERT")
           (assoc 10 ent-data))
    (cdr (assoc 10 ent-data))
    nil
  )
)

; Упрощенная версия для быстрого использования
(defun c:СКУД ()
  (c:СКУД_линия_и_обновление)
)

(princ "\nМакрос СКУД загружен. Введите СКУД для запуска.")
(princ)