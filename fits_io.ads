

package FITS_IO is

 -- HDU info

 MaxAxes : constant Positive := 999; -- NAXIS<=999 [FITS, Sect 4.3.2]
 type Data_Type is (Float64,Float32,Int16,Int32,Int64);-- encoded in BITPIX as: (-64,-32, 16,32,64)
 type Dim_Type is array (1..MaxAxes) of Natural;
 type HDU_Info is record
  CardsCnt : Positive;  -- number of cards in this Header
  Data     : Data_Type; -- data type as given by BITPIX
  DimSizes : Dim_Type;  -- data dimensions, 0 means dimension not in use
 end record;
 type All_HDU_Info is array (Positive range <>) of HDU_Info;

 -- access

 type File_Type is limited private;

 function FitsFile_Info (FitsFile : File_Type) return All_HDU_Info;

 procedure Copy_Blocks (FromFile : File_type; FirstBlock : Positive; LastBlock : Positive;
                        ToFile   : File_type);


 procedure Copy_HDU (FromFile : File_type; FirstHDU : Positive; LastHDU : Positive;
                     ToFile   : File_type);


private

 type File_Data_Array;
 type File_Type is access File_Data_Array;

end FITS_IO;
