
pikZoomPlus <- resizeTclImage(file.path(imgdir, 'ZoomIn128.gif', fsep = .Platform$file.sep), factor = 4, zoom = FALSE)
pikZoomMinus <- resizeTclImage(file.path(imgdir, 'ZoomOut128.gif', fsep = .Platform$file.sep), factor = 4, zoom = FALSE)
pikZoomRect <- resizeTclImage(file.path(imgdir, 'ZoomRect128.gif', fsep = .Platform$file.sep), factor = 4, zoom = FALSE)
pikCentre <- tkimage.create('photo', file = file.path(imgdir, 'imgCentre24.gif', fsep = .Platform$file.sep))
pikRedraw <- resizeTclImage(file.path(imgdir, 'redraw128.gif', fsep = .Platform$file.sep), factor = 4, zoom = FALSE)
pikPanImg <- tkimage.create('photo', file = file.path(imgdir, 'PanImage32.gif', fsep = .Platform$file.sep))
pikReset <- resizeTclImage(file.path(imgdir, 'reset128.gif', fsep = .Platform$file.sep), factor = 4, zoom = FALSE)
#pikSelRect <- resizeTclImage(file.path(imgdir, 'SelectRect129.gif', fsep = .Platform$file.sep), factor = 4, zoom = FALSE)


