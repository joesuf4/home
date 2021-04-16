@echo off
powershell wsl zsh "-c" "'. ~/.zshenv && exec gopls $@'" -- %*
