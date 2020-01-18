
with Ada.Unchecked_Conversion;
with Interfaces;
with System; use System;

with FITS; -- Byte-type needed

--with Ada.Text_IO; use Ada.Text_IO;

with Ada.Streams.Stream_IO;


package body Generic_Data_Integer is


-- Value conversions

function Physical
        (BZERO : in TF; 
        BSCALE : in TF; 
        BLANK : in T; 
        Data  : in T) return TF
is
  D : TF := TF(Data);
begin
--  if (Data = BLANK) return NaN; end if;
--  FIXME See IEEE 748 standard what bit-pattern is NaN and explicitely use that
  return BZERO + BSCALE * D;  
end Physical;


end Generic_Data_Integer;
