wsl --shutdown
Start-Process powershell -Verb runAs -ArgumentList 'wsl --manage Ubuntu -s false; Optimize-VHD -Path $env:USERPROFILE\AppData\Local\Packages\CanonicalGroupLimited.Ubuntu_*\LocalState\ext4.vhdx -Mode full; sleep 5'
