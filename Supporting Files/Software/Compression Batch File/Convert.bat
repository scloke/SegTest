set avidemux="C:\Program Files (x86)\Movie Tools\avidemux_64\avidemux_cli.exe"
set videocodec=Mjpeg
for %%f in (*.avi) do %avidemux% --video-codec %videocodec% --load "%%f" --save "Compressed\%%f_Compressed.avi" --quit