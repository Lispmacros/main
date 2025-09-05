# github-realtime-sync.ps1
$LocalRepo = "D:\SERVER"  # Ваша папка для синхронизации

# Проверяем доступность GitHub CLI
if (-not (Get-Command "gh" -ErrorAction SilentlyContinue)) {
    Write-Error "GitHub CLI не установлен. Установите с: https://cli.github.com"
    exit 1
}

# Функция для проверки авторизации GitHub
function Test-GitHubAuth {
    try {
        $authStatus = gh auth status 2>&1
        return ($LASTEXITCODE -eq 0 -and $authStatus -match "logged in")
    }
    catch {
        return $false
    }
}

# Функция для синхронизации с GitHub
function Sync-ToGitHub {
    param([string]$ChangeType, [string]$File)
    
    Write-Host "[$(Get-Date -Format 'HH:mm:ss')] Обнаружено: $ChangeType в $File" -ForegroundColor Cyan
    
    try {
        # Переходим в репозиторий
        Set-Location $LocalRepo
        
        # Проверяем статус репозитория
        if (-not (Test-Path ".git")) {
            Write-Host "❌ Папка не является Git репозиторием" -ForegroundColor Red
            return
        }
        
        # Получаем текущую ветку
        $branch = git branch --show-current
        if (-not $branch) {
            $branch = "main"
        }
        
        # Сначала pull чтобы получить актуальные изменения
        Write-Host "🔄 Получение изменений с GitHub..." -ForegroundColor Gray
        git pull origin $branch 2>$null
        
        # Получаем изменения
        $changes = git status --porcelain
        if ($changes) {
            Write-Host "📝 Обнаружены изменения:" -ForegroundColor Yellow
            $changes | ForEach-Object { Write-Host "  $_" -ForegroundColor Gray }
            
            # Добавляем все файлы
            git add .
            
            # Создаем коммит
            $commitMessage = "Auto-sync: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') - $ChangeType"
            if ($File) { $commitMessage += " - $File" }
            
            git commit -m $commitMessage
            
            # Пушим изменения
            Write-Host "🚀 Отправка изменений на GitHub..." -ForegroundColor Gray
            git push origin $branch
            
            if ($LASTEXITCODE -eq 0) {
                Write-Host "✅ Синхронизация завершена успешно" -ForegroundColor Green
            }
            else {
                Write-Host "⚠ Ошибка при push, пробуем через gh..." -ForegroundColor Yellow
                gh repo sync
            }
        }
        else {
            Write-Host "✅ Изменений для коммита нет" -ForegroundColor Gray
        }
    }
    catch {
        Write-Host "❌ Ошибка синхронизации: $($_.Exception.Message)" -ForegroundColor Red
    }
}

# Проверяем авторизацию
if (-not (Test-GitHubAuth)) {
    Write-Host "❌ Не авторизованы в GitHub CLI" -ForegroundColor Red
    Write-Host "Выполните: gh auth login" -ForegroundColor Yellow
    exit 1
}

# Проверяем что папка существует и это Git репозиторий
if (-not (Test-Path $LocalRepo)) {
    Write-Host "❌ Папка не существует: $LocalRepo" -ForegroundColor Red
    exit 1
}

if (-not (Test-Path "$LocalRepo\.git")) {
    Write-Host "❌ Папка не является Git репозиторием" -ForegroundColor Red
    Write-Host "Выполните вручную:"
    Write-Host "cd $LocalRepo"
    Write-Host "git init"
    Write-Host "git remote add origin https://github.com/Lispmacros/main.git"
    exit 1
}

# Создаем FileSystemWatcher
try {
    $watcher = New-Object System.IO.FileSystemWatcher
    $watcher.Path = $LocalRepo
    $watcher.IncludeSubdirectories = $true
    $watcher.NotifyFilter = [System.IO.NotifyFilters]::FileName, 
                           [System.IO.NotifyFilters]::DirectoryName,
                           [System.IO.NotifyFilters]::LastWrite,
                           [System.IO.NotifyFilters]::Size
    $watcher.EnableRaisingEvents = $true

    # Регистрируем события
    $action = {
        $changeType = $EventArgs.ChangeType
        $fileName = $EventArgs.Name
        Write-Host "[$(Get-Date -Format 'HH:mm:ss')] Событие: $changeType - $fileName" -ForegroundColor Magenta
        Start-Sleep -Seconds 2  # Задержка для завершения операций записи
        Sync-ToGitHub $changeType $fileName
    }

    Register-ObjectEvent $watcher "Created" -Action $action
    Register-ObjectEvent $watcher "Changed" -Action $action
    Register-ObjectEvent $watcher "Deleted" -Action $action
    Register-ObjectEvent $watcher "Renamed" -Action $action

    Write-Host "🚀 Мониторинг запущен: $LocalRepo" -ForegroundColor Green
    Write-Host "📊 Отслеживаются изменения файлов" -ForegroundColor Yellow
    Write-Host "⏹️  Для остановки нажмите Ctrl+C" -ForegroundColor Red
    
    # Получаем информацию о репозитории
    try {
        Set-Location $LocalRepo
        $branch = git branch --show-current
        if (-not $branch) { $branch = "main" }
        $repoUrl = gh repo view --json url -q '.url'
        Write-Host "🔗 Репозиторий: $repoUrl (ветка: $branch)" -ForegroundColor Cyan
    }
    catch {
        Write-Host "ℹ️ GitHub: $(gh repo view --json name -q '.name')" -ForegroundColor Cyan
    }

    # Бесконечный цикл
    while ($true) {
        Start-Sleep -Seconds 60
        # Периодическая проверка
        Sync-ToGitHub "PeriodicCheck" ""
    }
}
catch {
    Write-Host "❌ Ошибка инициализации мониторинга: $($_.Exception.Message)" -ForegroundColor Red
}
