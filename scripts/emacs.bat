@rem Start VcXsrv and Emacs server

@echo off
wsl -d Ubuntu-20.04 bash -c "export DISPLAY=:0.0 && export LIBGL_ALWAYS_INDIRECT=1 && setsid emacs"