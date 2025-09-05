(defun c:ALS (/ *error* oldLayer oldOsmode oldCmdEcho ptStart ptEnd)
    ;; Функция обработки ошибок
    (defun *error* (msg)
        (if oldLayer (setvar "CLAYER" oldLayer))
        (if oldOsmode (setvar "OSMODE" oldOsmode))
        (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
        (princ (strcat "\nОшибка: " msg))
    )

    ;; Сохраняем системные переменные
    (setq oldLayer (getvar "CLAYER"))
    (setq oldOsmode (getvar "OSMODE"))
    (setq oldCmdEcho (getvar "CMDECHO"))
    
    ;; Устанавливаем режимы
    (setvar "CMDECHO" 0) ;; Отключаем эхо команд
    
    ;; Проверяем/создаем слой "RC-Линии_связи-Адресные"
    (if (not (tblsearch "LAYER" "RC-Линии_связи-Адресные"))
        (command "_.LAYER" "_M" "RC-Линии_связи-Адресные" "_C" "1" "" "")
    )
    (setvar "CLAYER" "RC-Линии_связи-Адресные")
    
    ;; Настройка объектной привязки
    (setvar "OSMODE" 35) ;; Конец + Середина + Пересечение
    
    ;; Основной цикл построения отрезков
    (if (setq ptStart (getpoint "\nУкажите начальную точку: "))
        (progn
            (while (setq ptEnd (getpoint ptStart "\nУкажите следующую точку: "))
                (command "_.LINE" ptStart ptEnd "")
                (setq ptStart ptEnd)
            )
        )
    )
    
    ;; Восстановление настроек
    (setvar "CLAYER" oldLayer)
    (setvar "OSMODE" oldOsmode)
    (setvar "CMDECHO" oldCmdEcho)
    
    (princ)
)

(princ "\nКоманда ALS загружена. Введите ALS для построения отрезков в слое АЛС.")
(princ)