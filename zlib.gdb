set breakpoint pending on
set pagination off
set case-sensitive off
b zlib.adb:551 
command
      bt
      print Filter.Strm.all
      print In_Data_Last
      print AIn
      c
end
r
q
