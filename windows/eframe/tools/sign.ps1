# tools/sign.ps1 — one-time self-signing helper for eframe-jack-in.exe.
#
# Why this exists:
#   Corporate EDR (CrowdStrike Falcon / Defender ML) treats unsigned tray+hotkey
#   binaries as high-risk by default and silently denies execution. Signing the
#   exe with a locally-trusted certificate typically drops the heuristic score
#   below the block threshold. See CLAUDE.md → "EDR / execution policy" for
#   the full diagnosis story.
#
# What this does (idempotent — safe to re-run):
#   1. Finds or creates a self-signed code-signing cert in Cert:\CurrentUser\My
#      with subject "CN=eframe-jack-in local dev".
#   2. If a fresh cert was created, installs the public cert into
#      LocalMachine\Root (so the chain validates) and LocalMachine\TrustedPublisher
#      (so AppLocker publisher rules / SmartScreen / Falcon can recognise it).
#   3. Authenticode-signs the exe with SHA256 + a Digicert timestamp so the
#      signature keeps validating after the cert expires.
#
# Requirements: elevated PowerShell (steps 2 and 3 need admin on the LocalMachine store).
#
# Usage:
#   powershell -ExecutionPolicy Bypass -File tools\sign.ps1
#   powershell -ExecutionPolicy Bypass -File tools\sign.ps1 -ExePath other\eframe-jack-in.exe
#   powershell -ExecutionPolicy Bypass -File tools\sign.ps1 -Subject "CN=eframe-jack-in local dev, O=your-name"

[CmdletBinding()]
param(
    [string]$ExePath = "target\release\eframe-jack-in.exe",
    [string]$Subject = "CN=eframe-jack-in local dev",
    [string]$TimestampServer = "http://timestamp.digicert.com"
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path $ExePath)) {
    Write-Error "Not found: $ExePath  (build first with 'cargo build --release')"
    exit 1
}

# Elevation check — the LocalMachine cert store writes require admin.
$isAdmin = ([Security.Principal.WindowsPrincipal] `
    [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole(
        [Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
    Write-Warning "Not elevated. First-run cert-install steps will fail; signing will still work if a cert is already installed."
}

# 1. Find or create the code-signing cert.
$cert = Get-ChildItem Cert:\CurrentUser\My |
    Where-Object { $_.Subject -eq $Subject -and $_.NotAfter -gt (Get-Date).AddDays(30) } |
    Sort-Object NotAfter -Descending |
    Select-Object -First 1

if (-not $cert) {
    Write-Host "Creating new code-signing certificate: $Subject"
    $cert = New-SelfSignedCertificate `
        -Type CodeSigningCert `
        -Subject $Subject `
        -CertStoreLocation Cert:\CurrentUser\My `
        -KeyUsage DigitalSignature `
        -KeyExportPolicy Exportable `
        -NotAfter (Get-Date).AddYears(5) `
        -HashAlgorithm SHA256

    # Publish the public cert to the LocalMachine trust stores so third-party
    # policy layers (AppLocker publisher rules, Falcon publisher-trust, etc.)
    # recognise the chain. Requires admin.
    $tmp = New-TemporaryFile
    try {
        [IO.File]::WriteAllBytes($tmp, $cert.Export("Cert"))
        Import-Certificate -FilePath $tmp -CertStoreLocation Cert:\LocalMachine\Root             | Out-Null
        Import-Certificate -FilePath $tmp -CertStoreLocation Cert:\LocalMachine\TrustedPublisher | Out-Null
        Write-Host "Cert installed to LocalMachine\Root and LocalMachine\TrustedPublisher."
    } finally {
        Remove-Item $tmp -ErrorAction SilentlyContinue
    }
} else {
    Write-Host "Reusing existing cert: thumbprint $($cert.Thumbprint), expires $($cert.NotAfter)"
}

# 2. Sign the exe.
Write-Host "Signing $ExePath ..."
$sig = Set-AuthenticodeSignature -FilePath $ExePath -Certificate $cert `
    -TimestampServer $TimestampServer -HashAlgorithm SHA256

if ($sig.Status -ne "Valid") {
    Write-Error "Signing failed: $($sig.Status) - $($sig.StatusMessage)"
    exit 2
}

Write-Host ""
Write-Host "OK. Signature: $($sig.Status)"
Write-Host "  SignerCertificate: $($sig.SignerCertificate.Subject)"
Write-Host "  Timestamp:         $($sig.TimeStamperCertificate.Subject)"

# 3. Print SHA256 so it can be pasted into an IT ticket if this still doesn't
#    lift the Falcon block (see CLAUDE.md → "EDR / execution policy").
$hash = (Get-FileHash $ExePath -Algorithm SHA256).Hash
Write-Host ""
Write-Host "SHA256($ExePath) = $hash"
Write-Host "(paste this into an IT ticket if execution is still denied.)"
