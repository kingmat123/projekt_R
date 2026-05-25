# 1. Zapisz token (wklej go w konsoli, gdy program o to poprosi)
gitcreds::gitcreds_set()

# 2. Zainicjuj Gita w folderze (jeśli RStudio zapyta o restart, kliknij YES i uruchom krok 3)
usethis::use_git()

# 3. Utwórz repozytorium na GitHubie i wyślij pliki
usethis::use_github()
