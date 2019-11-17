
-- FIXME use the same generic as Parser - to _separate_
-- this code from FITS_IO.File (no need for Ada.Stream_IO here)
-- Possible??

--with FITS.Data; -- Data_Arr needed
with FITS; use FITS;
with Ada.Streams.Stream_IO;


-- NOTE: this was NCube.ads originally

package FITS_IO.Data is

-- -------------------------------------------------
 -- Solution 1:
 -- write all data in one step - "small" data compared to computer memory

 -- implements [FITS Sect ??] ordering N-dimensional data by columns
 -- into an 1-dimensional array

 type Coord_Type is array (FPositive range <> ) of FPositive;
-- type Int_Arr is array (FPositive range <> ) of Integer;

 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  Coord_Type;
                      Coords    : out Coord_Type);
 generic
  -- type Item is (<>); -- this allows only discete types like Integer
  type Item is private; -- this allows also Float
  type Coll is array(FPositive range <>) of Item;
  with function Value ( Coord : in Coord_Type ) return Item;
 procedure Fill_In (DataVector : in out Coll; MaxCoords : in Coord_Type);-- is null;
 -- FIXME:
 -- Rename Value() to:               Set(coord,Item)
 --   and Fill_In() to is Set array: Set_All(array,...)
 -- Add generic get func:            Get(array, coord) return Item

 -- cutout: generate next coord from previous
 -- for "cutout": copying V-sized portion of NCube starting from Offset
 function Next_Coord(Coord  : in out Coord_Type;
                     Offset : in Coord_Type;
                     Vol    : in Coord_Type)
   return Coord_Type;

 -- using Next_Coord, read from file subcube of volume V
 -- assume Set_Index has positioned file-pointer to DU start
 -- Offset is measured from begining of DataUnit.
 -- Data is stored in 1-dim array Arr: use funcs FITS.Data.ELement()
 -- to reach data element by N-dimensional coordinate
 procedure Read_Volume(File     : in Ada.Streams.Stream_IO.File_Type;
                       Offset   : in Coord_Type;
                       MaxCoord : in Coord_Type;
                       Vol      : in Coord_Type;
                       Arr      : out UInt8_Arr);



 -- Solution 2:
 -- For "big" data: write by group of N-rows
 -- ...

end FITS_IO.Data;

-- NOTES:
-- below compiles but see FIXME
-- generic
--  type Index is (<>);
--  type Item is (<>);-- OR: private; OR limited private;
--  with function DValue ( Coord : in Coord_Type ) return Item;
-- procedure DFill_In (Data_Acc :  access FITS.Data.Data_Arr);-- is null;
 -- FIXME above commented line works BUT
 --       constraint not allowed after .Data_Arr above: However we know that ValueType chould be DType
 --       use directly array !? instead of Data_Arr?


