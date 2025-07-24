(defun c:outRubezh (/ *error* targetLayer ss i ent handle filePath file xmlContent uniquePart fullHandle dwgId 
                     entData xdata rubezhData newXdataItem blk attrs attr pstnValue vlaObj effectiveName found 
                     newXdata ugoData code1070 hasValidXdata app data zoneNumber dwgIdFile)
  ;; Функция обработки ошибок
  (defun *error* (msg)
    (if (and file (not (eq (type file) 'SYM))) (close file))
    (if (and dwgIdFile (not (eq (type dwgIdFile) 'SYM))) (close dwgIdFile))
    (princ (strcat "\nОшибка: " msg))
    (princ)
  )
  
  ;; Функция для разделения строки по разделителю (аналог vl-string-split)
  (defun string-split (str delim / pos result)
    (setq result '())
    (while (setq pos (vl-string-search delim str))
      (setq result (cons (substr str 1 pos) result))
      (setq str (substr str (+ pos 2)))
    )
    (reverse (cons str result))
  )
  
  ;; Функция для извлечения и форматирования номера зоны из строки (из скобок)
  (defun formatZoneNumber (str / start end zoneStr parts)
    (setq start (vl-string-position (ascii "(") str))
    (setq end (vl-string-position (ascii ")") str))
    (if (and start end (> end start))
      (progn
        (setq zoneStr (substr str (+ start 2) (- end start 1)))
        (setq parts (string-split zoneStr "."))
        (cond
          ((= (length parts) 1) (strcat ".." zoneStr))      ; (4) → ..4
          ((= (length parts) 2) (strcat "." zoneStr))        ; (2.3) → .2.3
          (t zoneStr)                                       ; (1.2.3) → 1.2.3
        )
      )
      "..4"  ; значение по умолчанию, если скобок нет
    )
  )
  
  (vl-load-com)
  (setq targetLayer "RC-Извещатели_пожарные")
  (setq filePath (strcat (getvar "dwgprefix") "db_proj_equip.xml"))
  
  ;; Генерация случайного 9-значного числа для dwgId
  (setq dwgId (itoa (+ 100000000 (rem (* (getvar "MILLISECS") 900000000) 900000000))))
  
  ;; Запись dwgId в файл dwgid.txt
  (setq dwgIdFile (open (strcat (getvar "dwgprefix") "dwgid.txt") "w"))
  (if dwgIdFile
    (progn
      (write-line dwgId dwgIdFile)
      (close dwgIdFile)
    )
    (princ "\nНе удалось создать файл dwgid.txt!")
  )
  
  ;; Функция генерации уникального номера
  (defun generateUniquePart (handle / sum ch randomPart)
    (setq sum 0)
    (foreach ch (vl-string->list handle)
      (setq sum (+ sum ch))
    )
    (setq randomPart (rem (getvar "MILLISECS") 10000))
    (rem (+ (* sum 1000) randomPart) 10000000)
  )
  
  (if (setq file (open filePath "w"))
    (progn
      (setq xmlContent "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<EquipmentList>\n")
      
      ;; Упрощенный фильтр - только наличие приложения "rubezh"
      (setq filter (list '(0 . "INSERT") 
                         (cons 8 targetLayer)
                         '(-3 ("rubezh"))))
      
      (if (setq ss (ssget "_X" filter))
        (progn
          (setq i 0)
          (setq found nil)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq i (1+ i))
            (setq handle (cdr (assoc 5 (entget ent))))
            (setq pstnValue nil)
            (setq hasValidXdata nil)
            (setq zoneNumber "..4")  ; значение по умолчанию
            
            ;; Получаем эффективное имя блока
            (setq vlaObj (vlax-ename->vla-object ent))
            (setq effectiveName (vla-get-effectivename vlaObj))
            
            ;; Получаем атрибуты
            (setq attrs (vlax-invoke vlaObj 'GetAttributes))
            (foreach attr attrs
              (if (eq (vla-get-TagString attr) "PSTN")
                (setq pstnValue (vla-get-TextString attr))
              )
            )
            
            ;; Форматируем номер зоны из PSTN
            (if pstnValue
              (setq zoneNumber (formatZoneNumber pstnValue))
            )
            
            ;; Получаем XDATA
            (setq entData (entget ent '("*")))
            (setq xdata (assoc -3 entData))
            
            ;; Проверяем XDATA вручную
            (if xdata
              (foreach app (cdr xdata)
                (if (eq (car app) "rubezh")
                  (foreach data (cdr app)
                    (if (and (= (car data) 1070) 
                         (= (cdr data) 5))
                      (setq hasValidXdata T)
                    )
                  )
                )
              )
            )
            
            (if (not hasValidXdata)
              (progn
                (princ (strcat "\nПропущен блок " handle " - не соответствует XDATA (1070 != 5)"))
                (setq pstnValue nil)
              )
            )
            
            (if (and pstnValue hasValidXdata)
              (progn
                (setq found T)
                
                ;; Обработка XDATA с генерацией
                (setq rubezhData (cdr (assoc "rubezh" (cdr xdata))))
                
                (if (setq uniquePart (cdr (assoc 1071 rubezhData)))
                  (setq uniquePart (itoa uniquePart))
                  (progn
                    ;; Генерация нового уникального номера
                    (setq uniquePart (generateUniquePart handle))
                    (setq newXdataItem (list "rubezh" (cons 1000 "equipment") (cons 1071 uniquePart)))
                    
                    ;; Обновляем XDATA
                    (if rubezhData
                      (setq newXdata (subst newXdataItem (assoc "rubezh" (cdr xdata)) (cdr xdata)))
                      (setq newXdata (cons newXdataItem (cdr xdata)))
                    )
                    
                    ;; Обновляем объект
                    (entmod (subst (cons -3 newXdata) xdata entData))
                    (setq uniquePart (itoa uniquePart))
                  )
                )
                
                (setq fullHandle (strcat handle "#" uniquePart))
                
                ;; Формируем XML
                (setq xmlContent 
                  (strcat xmlContent 
                    "  <Equipment uid=\"36\" idCat=\"5\" cvt_article=\"RBZ-337473\" type=\"equipment\" isInSpec=\"true\" positType=\"address\" schmEqGroupName=\"ИП212-64-R3\" ugoName=\"RC-UGO-IDTA-01\" addressCnt=\"1\" replGrpId=\"5\" curConsumpAlsStandby=\"0.00032\" curConsumpAlsAlarm=\"0.00032\" blckOptGrpId=\"Group_36\" rsType=\"R3\" minMountHeight=\"0.1\" handle=\"" fullHandle "\" positText=\"BTH\" IsPosNameFix=\"False\" positName=\"" pstnValue "\" mountingHeight=\"2.4\" cablingHeight=\"2.4\" FrAlrmZoneNum=\"" zoneNumber "\" isFireZoneExtended=\"False\" additionalInfo=\"\" dwgId=\"" dwgId "\">\n"
                    "    <ports count=\"1\">\n"
                    "      <port name=\"АЛС\" supportedLsType=\"address\" lsName=\"АЛС1.22\" />\n"
                    "    </ports>\n"
                    "  </Equipment>\n"
                  )
                )
              )
              (if pstnValue
                (princ (strcat "\nПропущен блок " handle " - не соответствует XDATA"))
                (princ (strcat "\nПропущен блок " handle " - отсутствует атрибут PSTN"))
              )
            )
          )
          
          ;; Завершаем XML
          (setq xmlContent (strcat xmlContent ""))
          (write-line xmlContent file)
          (close file)
          (setq file nil)
          
          (if found
            (princ (strcat "\nФайл создан: " filePath))
            (princ (strcat "\nБлоки с атрибутом PSTN не найдены на слое '" targetLayer "'!"))
          )
        )
        (progn
          (close file)
          (setq file nil)
          (princ (strcat "\nБлоки с указанным XDATA на слое '" targetLayer "' не найдены!"))
        )
      )
    )
    (princ "\nОшибка создания файла!")
  )
  (princ)


  (if (member 'c:YGO2 (atoms-family 1))
      (progn
        (princ "\nЗапуск команды YGO2")
        (c:YGO2)
      )
      (c:YGO2)
  )
  (princ) ; финальный princ
)

(princ "\nКоманда outRubezh готова к использованию. Введите outRubezh для выполнения.")