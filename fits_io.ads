

package FITS_IO is

 type File_Type is limited private;
 type Mode_Type is (In_File, Out_File, Inout_File, Append_File);

 procedure Create ( Fits : in out File_Type;
                    Mode : in Mode_Type;
                    Name : in String;
                    Form : in String    := "");

 procedure Open ( Fits : in out File_Type;
                  Mode : in Mode_Type;
                  Name : in String;
                  Form : in String   := "");

 procedure Close ( Fits : in out File_Type );


 -- HDU info

 MaxAxes : constant Positive := 999; -- NAXIS <= 999 [FITS, Sect 4.4.1]
 type Data_Type is (Float64,Float32,Int16,Int32,Int64);-- encoded in BITPIX as: (-64,-32, 16,32,64)
 type Dim_Type is array (1..MaxAxes) of Natural;
 type HDU_Info is record
  CardsCnt : Positive;  -- number of cards in this Header
  Data     : Data_Type; -- data type as given by BITPIX
  DimSizes : Dim_Type;  -- data dimensions, 0 means dimension not in use
 end record;
 Null_HDU_Info : constant HDU_Info := (1,Int32,(others=>0));
 type All_HDU_Info is array (Positive range <>) of HDU_Info;

 function FitsFile_Info ( Fits : File_Type ) return All_HDU_Info;
 -- FIXME consider returns array of preformated strings; handles unknown dimensions
 -- one line per HDU like: "NoOfCards DataType ( nnn x mmm x kkk )"

 procedure Copy_HDU (FromFile : File_type; FirstHDU : Positive; LastHDU : Positive;
                     ToFile   : File_type);


private

 type File_Data_Array;
 type File_Type is access File_Data_Array;

 procedure Copy_Blocks (FromFile : File_type; FirstBlock : Positive; LastBlock : Positive;
                        ToFile   : File_type);

end FITS_IO;
