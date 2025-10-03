# Root of your dotfiles repo
$repoRoot = "./"

Write-Host "Symlinking dotfiles from $repoRoot..."

function Backup {
    param (
        [string]$Source,
        [string]$Target
    )

    if (Test-Path $Target) {
        $backupName = "$Target.bak_$(Get-Date -Format yyyyMMddHHmmss)"
        Write-Host "Backing up $Target -> $backupName"
        Rename-Item -Path $Target -NewName $backupName
    }
}


function Backup-And-Link {
    param (
        [string]$Source,
        [string]$Target,
        [bool]$HardLink=$true
    )

    Backup -Source $Source -Target $Target

    Write-Host "Creating symlink: $Target -> $Source"
    if ($HardLink)
    {
        New-Item -ItemType HardLink -Path $Target -Target $Source | Out-Null
    }
    else 
    {
        $absoluteSource = (Resolve-Path $Source).Path
        New-Item -ItemType Junction -Path $Target -Target $absoluteSource | Out-Null
    }
}

# --- Emacs config ---
$emacsSource = Join-Path $repoRoot "emacs\.emacs.d"
$emacsTarget = Join-Path $HOME ".emacs.d"
Backup-And-Link -Source $emacsSource -Target $emacsTarget -HardLink $false

# --- Git configs ---
$gitWorkSource     = Join-Path $repoRoot "git\.gitconfig-work"
$gitPersonalSource = Join-Path $repoRoot "git\.gitconfig-personal"
$gitWindowsSource  = Join-Path $repoRoot "git\.gitconfig-windows"

$gitWorkTarget     = Join-Path $HOME ".gitconfig-work"
$gitPersonalTarget = Join-Path $HOME ".gitconfig-personal"
$gitTarget         = Join-Path $HOME ".gitconfig"  # symlink to windows version

Backup-And-Link -Source $gitWorkSource -Target $gitWorkTarget
Backup-And-Link -Source $gitPersonalSource -Target $gitPersonalTarget
Backup-And-Link -Source $gitWindowsSource -Target $gitTarget

Write-Host "Dotfiles symlinking complete."
