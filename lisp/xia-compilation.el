(defun xia-compile-microdxp ()
  "Compile the Handel/Xerxes libraries with support only for the microDXP"
  (interactive)
  (setq compile-command "make SERIAL=true CAMAC=false EPP=false ARCNET=false PLX=false USB=false BUILD_VBA=true all")
  (compile compile-command))

(defun xia-compile-dxp2x ()
  "Compile the Handel/Xerxes libraries with support only for the 2X"
  (interactive)
  (setq compile-command "make SERIAL=false CAMAC=true EPP=false ARCNET=false PLX=false USB=false BUILD_VBA=true DXP4C=false all")
  (compile compile-command))

(defun xia-compile-xmap ()
  "Compile the Handel/Xerxes libraries with support only for the xMAP"
  (interactive)
  (setq compile-command "make SERIAL=false CAMAC=false EPP=false ARCNET=false PLX=true USB=false all")
  (compile compile-command))

(defun xia-compile-xmap-profile ()
  "Compile the Handel/Xerxes libraries with support only for the xMAP (Profiling enabled)"
  (interactive)
  (setq compile-command "make SERIAL=false CAMAC=false EPP=false ARCNET=false PLX=true USB=false PROFILE=true all")
  (compile compile-command))

(defun xia-compile-all-mem ()
  "Compile the Handel/Xerxes libraries with the custom memory manager"
  (interactive)
  (setq compile-command "make CUSTOM_MEM_MANAGER=true all")
  (compile compile-command))

(defun xia-compile-all-param-debug ()
  "Compile Handel with XIA_PARAM_DEBUG turned on."
  (interactive)
  (setq compile-command "make PARAM_DEBUG=true all")
  (compile compile-command))

(defun xia-compile-all-plx-write-debug ()
  "Compile Handel with XIA_PLX_WRITE_DEBUG turned on."
  (interactive)
  (setq compile-command "make PLX_WRITE_DEBUG=true all")
  (compile compile-command))
