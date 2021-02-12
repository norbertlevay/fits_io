
with Ada.Streams;
--with Mandatory; use Mandatory; -- NAXIS_Array needed

with FITS; use FITS;
--with FITS_IO; use FITS_IO;

generic
package Numeric_Type.Data_IO is

--package SIO renames FITS_IO;
--package SIO renames Ada.Streams.Stream_IO;



procedure Read_Buffered
   (F : SIO.File_Type;
   Arr : out Numeric_Arr;
   Length : out Ada.Streams.Stream_Element_Offset);


procedure Read (F : SIO.File_Type; A : out Float_Arr);
procedure Write(F : SIO.File_Type; A : in  Float_Arr);



-- Data Unit access


generic
  with procedure Elem(E : in Float);
procedure Read_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Array);



generic
  with procedure Elem(E : out Float);
procedure Write_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Array);


end Numeric_Type.Data_IO;

