
with Ada.IO_Exceptions;
with Ada.Direct_IO;
--use Ada.Direct_IO;

with FITS; use FITS;

package FITS_IF.Data is

   	-- Random access by ScanLine 
	-- (by position in the NCube + Length, where Length <= Length of 1st axis) 
   
   type Coord_Arr is array (Positive_Count range <>) of Positive_Count;

    generic
     type Data_Type is private;
     type Scan_Arr  is array (Positive_Count range <>) of Data_Type;
   procedure Read_Data
     (File      : File_Type;
      ScanLine  : out Scan_Arr;
      From      : Coord_Arr);

  generic
     type Data_Type is private;
     type Scan_Arr  is array (Positive_Count range <>) of Data_Type;
   procedure Write_Data
     (File      : File_Type;
      ScanLine  : Scan_Arr;
      To        : Coord_Arr);


   -- and other acces: many rows with one read/write -> length must be full NCube length (NAXIS1)

end FITS_IF.Data;
