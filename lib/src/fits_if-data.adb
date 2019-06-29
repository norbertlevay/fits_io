
with Ada.IO_Exceptions;
with Ada.Direct_IO;
--use Ada.Direct_IO;

with FITS; use FITS;

package body FITS_IF.Data is

--    generic
--     type Data_Type is private;
--     type Scan_Arr  is array (Positive_Count range <>) of Data_Type;
   procedure Read_Data
     (File      : File_Type;
      ScanLine  : out Scan_Arr;
      From      : Coord_Arr) 
   is
   begin
	   null;
   end Read_Data;

--  generic
--     type Data_Type is private;
--     type Scan_Arr  is array (Positive_Count range <>) of Data_Type;
   procedure Write_Data
     (File      : File_Type;
      ScanLine  : Scan_Arr;
      To        : Coord_Arr)
   is
   begin
	   null;
   end Write_Data;


end FITS_IF.Data;
