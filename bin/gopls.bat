@echo off
powershell wsl zsh "-c" "'. ~/.zshenv && gopls $@'" -- %*
