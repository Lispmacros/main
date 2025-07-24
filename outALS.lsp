(defun c:outALS (/ ss ss2 ss3 ss4 i ent handle filePath file xmlContent uniquePart fullHandle dwgId entData xdata rubezhData newXdata dwgIdFile)
  (setq filePath (strcat (getvar "dwgprefix") "db_proj_ls.xml"))
  
  ;; Попытка прочитать dwgId из файла dwgid.txt
  (setq dwgIdFile (findfile (strcat (getvar "dwgprefix") "dwgid.txt")))
  (if dwgIdFile
    (progn
      (setq dwgIdFile (open dwgIdFile "r"))
      (setq dwgId (read-line dwgIdFile))
      (close dwgIdFile)
      (if (not dwgId)
        (setq dwgId (rtos (getvar "CDATE") 2 8)) ; Если файл пустой, используем резервный вариант
      )
    )
    (setq dwgId (rtos (getvar "CDATE") 2 8)) ; Если файл не найден, используем резервный вариант
  )
  
  ;; Функция генерации уникального номера на основе handle
  (defun generateUniquePart (handle / sum randomPart)
    (setq sum 0)
    (foreach ch (vl-string->list handle)
      (setq sum (+ sum ch))
    )
    (setq randomPart (rem (getvar "MILLISECS") 10000))
    (rem (+ (* sum 1000) randomPart) 10000000)
  )
  
  (if (setq file (open filePath "w"))
    (progn
      (setq xmlContent "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<CommunicLines>\n")
      
      ;; Обработка адресных линий
      (if (setq ss (ssget "_X" '((8 . "RC-Линии_связи-Адресные"))))
        (progn
          (setq xmlContent (strcat xmlContent "<CommunicLine lsName=\"АЛС1.22\" userLsName=\"АЛС1.22\" type=\"address\" handleMainEqip=\"\">\n"))
          
          (setq i (sslength ss))
          (while (> i 0)
            (setq i (1- i))
            (setq ent (ssname ss i))
            (setq handle (cdr (assoc 5 (entget ent))))
            
            ;; Получаем полные данные объекта
            (setq entData (entget ent '("*")))
            (setq xdata (assoc -3 entData))
            
            ;; Обработка XDATA
            (if xdata
              (progn
                ;; Ищем данные приложения "rubezh"
                (setq rubezhData (cdr (assoc "rubezh" (cdr xdata))))
                
                (if (and rubezhData (setq uniquePart (cdr (assoc 1071 rubezhData))))
                  (setq uniquePart (itoa uniquePart))
                  (progn
                    ;; Если данных нет, генерируем новый уникальный номер
                    (setq uniquePart (generateUniquePart handle))
                    
                    ;; Создаем или обновляем XDATA
                    (setq newXdataItem (list "rubezh" (cons 1000 "cable#address") (cons 1071 uniquePart)))
                    
                    (if rubezhData
                      ;; Обновляем существующие данные
                      (setq newXdata (subst (cons "rubezh" (cdr newXdataItem)) (assoc "rubezh" (cdr xdata)) (cdr xdata)))
                      ;; Добавляем новые данные
                      (setq newXdata (cons newXdataItem (cdr xdata)))
                    )
                    
                    ;; Обновляем объект
                    (entmod (subst (cons -3 newXdata) xdata entData))
                    (setq uniquePart (itoa uniquePart))
                  )
                )
              )
              ;; Если XDATA нет - создаем
              (progn
                (regapp "rubezh")
                (setq uniquePart (generateUniquePart handle))
                (setq newXdata (list -3 (list "rubezh" (cons 1000 "cable#address") (cons 1071 uniquePart))))
                (entmod (append (entget ent) (list newXdata)))
                (setq uniquePart (itoa uniquePart))
              )
            )
            
            ;; Формируем полный handle
            (setq fullHandle (strcat handle "#" uniquePart))
            (setq xmlContent (strcat xmlContent "  <CableSegment handle=\"" fullHandle "\" id=\"0\" length=\"0\" dwgId=\"" dwgId "\"/>\n"))
          )
          
          (setq xmlContent (strcat xmlContent "</CommunicLine>\n"))
        )
      )
      
      ;; Обработка интерфейсных линий R3-Link
      (if (setq ss2 (ssget "_X" '((8 . "RC-Линии_связи-Интерфейсные_R3-Link"))))
        (progn
          (setq xmlContent (strcat xmlContent "<CommunicLine lsName=\"RL1\" userLsName=\"RL1\" type=\"interface_r3link\" handleMainEqip=\"\">\n"))
          
          (setq i (sslength ss2))
          (while (> i 0)
            (setq i (1- i))
            (setq ent (ssname ss2 i))
            (setq handle (cdr (assoc 5 (entget ent))))
            
            ;; Получаем полные данные объекта
            (setq entData (entget ent '("*")))
            (setq xdata (assoc -3 entData))
            
            ;; Обработка XDATA
            (if xdata
              (progn
                ;; Ищем данные приложения "rubezh"
                (setq rubezhData (cdr (assoc "rubezh" (cdr xdata))))
                
                (if (and rubezhData (setq uniquePart (cdr (assoc 1071 rubezhData))))
                  (setq uniquePart (itoa uniquePart))
                  (progn
                    ;; Если данных нет, генерируем новый уникальный номер
                    (setq uniquePart (generateUniquePart handle))
                    
                    ;; Создаем или обновляем XDATA
                    (setq newXdataItem (list "rubezh" (cons 1000 "cable#interface_r3link") (cons 1071 uniquePart)))
                    
                    (if rubezhData
                      ;; Обновляем существующие данные
                      (setq newXdata (subst (cons "rubezh" (cdr newXdataItem)) (assoc "rubezh" (cdr xdata)) (cdr xdata)))
                      ;; Добавляем новые данные
                      (setq newXdata (cons newXdataItem (cdr xdata)))
                    )
                    
                    ;; Обновляем объект
                    (entmod (subst (cons -3 newXdata) xdata entData))
                    (setq uniquePart (itoa uniquePart))
                  )
                )
              )
              ;; Если XDATA нет - создаем
              (progn
                (regapp "rubezh")
                (setq uniquePart (generateUniquePart handle))
                (setq newXdata (list -3 (list "rubezh" (cons 1000 "cable#interface_r3link") (cons 1071 uniquePart))))
                (entmod (append (entget ent) (list newXdata)))
                (setq uniquePart (itoa uniquePart))
              )
            )
            
            ;; Формируем полный handle
            (setq fullHandle (strcat handle "#" uniquePart))
            (setq xmlContent (strcat xmlContent "  <CableSegment handle=\"" fullHandle "\" id=\"0\" length=\"0\" dwgId=\"" dwgId "\"/>\n"))
          )
          
          (setq xmlContent (strcat xmlContent "</CommunicLine>\n"))
        )
      )
      
      ;; Обработка линий питания 12-24В
      (if (setq ss3 (ssget "_X" '((8 . "RC-Линии_связи-Питание_12-24В"))))
        (progn
          (setq xmlContent (strcat xmlContent "<CommunicLine lsName=\"P1\" userLsName=\"P1\" type=\"power12\" handleMainEqip=\"\">\n"))
          
          (setq i (sslength ss3))
          (while (> i 0)
            (setq i (1- i))
            (setq ent (ssname ss3 i))
            (setq handle (cdr (assoc 5 (entget ent))))
            
            ;; Получаем полные данные объекта
            (setq entData (entget ent '("*")))
            (setq xdata (assoc -3 entData))
            
            ;; Обработка XDATA
            (if xdata
              (progn
                ;; Ищем данные приложения "rubezh"
                (setq rubezhData (cdr (assoc "rubezh" (cdr xdata))))
                
                (if (and rubezhData (setq uniquePart (cdr (assoc 1071 rubezhData))))
                  (setq uniquePart (itoa uniquePart))
                  (progn
                    ;; Если данных нет, генерируем новый уникальный номер
                    (setq uniquePart (generateUniquePart handle))
                    
                    ;; Создаем или обновляем XDATA
                    (setq newXdataItem (list "rubezh" (cons 1000 "cable#power12") (cons 1071 uniquePart)))
                    
                    (if rubezhData
                      ;; Обновляем существующие данные
                      (setq newXdata (subst (cons "rubezh" (cdr newXdataItem)) (assoc "rubezh" (cdr xdata)) (cdr xdata)))
                      ;; Добавляем новые данные
                      (setq newXdata (cons newXdataItem (cdr xdata)))
                    )
                    
                    ;; Обновляем объект
                    (entmod (subst (cons -3 newXdata) xdata entData))
                    (setq uniquePart (itoa uniquePart))
                  )
                )
              )
              ;; Если XDATA нет - создаем
              (progn
                (regapp "rubezh")
                (setq uniquePart (generateUniquePart handle))
                (setq newXdata (list -3 (list "rubezh" (cons 1000 "cable#power12") (cons 1071 uniquePart))))
                (entmod (append (entget ent) (list newXdata)))
                (setq uniquePart (itoa uniquePart))
              )
            )
            
            ;; Формируем полный handle
            (setq fullHandle (strcat handle "#" uniquePart))
            (setq xmlContent (strcat xmlContent "  <CableSegment handle=\"" fullHandle "\" id=\"0\" length=\"0\" dwgId=\"" dwgId "\"/>\n"))
          )
          
          (setq xmlContent (strcat xmlContent "</CommunicLine>\n"))
        )
      )
      
      ;; Обработка линий звукового оповещения
      (if (setq ss4 (ssget "_X" '((8 . "RC-Линии_связи-Оповещение_звуковое"))))
        (progn
          (setq xmlContent (strcat xmlContent "<CommunicLine lsName=\"S1\" userLsName=\"S1\" type=\"notif_sound\" handleMainEqip=\"\">\n"))
          
          (setq i (sslength ss4))
          (while (> i 0)
            (setq i (1- i))
            (setq ent (ssname ss4 i))
            (setq handle (cdr (assoc 5 (entget ent))))
            
            ;; Получаем полные данные объекта
            (setq entData (entget ent '("*")))
            (setq xdata (assoc -3 entData))
            
            ;; Обработка XDATA
            (if xdata
              (progn
                ;; Ищем данные приложения "rubezh"
                (setq rubezhData (cdr (assoc "rubezh" (cdr xdata))))
                
                (if (and rubezhData (setq uniquePart (cdr (assoc 1071 rubezhData))))
                  (setq uniquePart (itoa uniquePart))
                  (progn
                    ;; Если данных нет, генерируем новый уникальный номер
                    (setq uniquePart (generateUniquePart handle))
                    
                    ;; Создаем или обновляем XDATA
                    (setq newXdataItem (list "rubezh" (cons 1000 "cable#notif_sound") (cons 1071 uniquePart)))
                    
                    (if rubezhData
                      ;; Обновляем существующие данные
                      (setq newXdata (subst (cons "rubezh" (cdr newXdataItem)) (assoc "rubezh" (cdr xdata)) (cdr xdata)))
                      ;; Добавляем новые данные
                      (setq newXdata (cons newXdataItem (cdr xdata)))
                    )
                    
                    ;; Обновляем объект
                    (entmod (subst (cons -3 newXdata) xdata entData))
                    (setq uniquePart (itoa uniquePart))
                  )
                )
              )
              ;; Если XDATA нет - создаем
              (progn
                (regapp "rubezh")
                (setq uniquePart (generateUniquePart handle))
                (setq newXdata (list -3 (list "rubezh" (cons 1000 "cable#notif_sound") (cons 1071 uniquePart))))
                (entmod (append (entget ent) (list newXdata)))
                (setq uniquePart (itoa uniquePart))
              )
            )
            
            ;; Формируем полный handle
            (setq fullHandle (strcat handle "#" uniquePart))
            (setq xmlContent (strcat xmlContent "  <CableSegment handle=\"" fullHandle "\" id=\"0\" length=\"0\" dwgId=\"" dwgId "\"/>\n"))
          )
          
          (setq xmlContent (strcat xmlContent "</CommunicLine>\n"))
        )
      )
      
      (setq xmlContent (strcat xmlContent "</CommunicLines>"))
      (write-line xmlContent file)
      (close file)
      (princ (strcat "\nФайл создан: " filePath))
    )
    (princ "\nОшибка создания файла!")
  )
  (princ)
)

(princ "\nКоманда outALS загружена. Введите outALS для выполнения.")