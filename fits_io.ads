




package FITS_IO is

 type File_Type is limited private;
 type Mode_Type is (In_File, Inout_File, Append_File);

-- valid combinations:
-- Open   : In Inout Append
-- Create : Append

 procedure Create ( Fits : in out File_Type;
                    Mode : in Mode_Type;
                    Name : in String;
                    Form : in String := "");

 procedure Open ( Fits : in out File_Type;
                  Mode : in Mode_Type;
                  Name : in String;
                  Form : in String := "");

 procedure Mode ( Fits : in out File_Type;
                  Mode : out Mode_Type ) is null;

 procedure Set_Mode ( Fits : in out File_Type;
                      Mode : in Mode_Type ) is null;

 procedure Close ( Fits : in out File_Type );

 -- FITS-file level fucntionalities may change size of HDU's
 -- and so HDU's need to be shifted in file

 -- with existing Header

 type Header_Type is null record;-- TBD take the one from HDU package

 procedure Read ( File    : in  File_Type;  -- with Mode In

                  HDU_Num : in  Positive;
                  Header  : out Header_Type ) is null;

 procedure Modify ( File   : in File_Type;  -- with Mode Inout
                    Header : in Header_Type ) is null;

 procedure Delete ( File    : in File_Type; -- with Mode Inout
                    HDU_Num : in  Positive ) is null;

 -- with new Header

 procedure Insert ( File    : in File_Type; -- with Mode = Inout
                    HDU_Num : in Positive;
                    Header  : in Header_Type ) is null;

 procedure Append ( File    : in File_Type; -- with Mode Append
                    Header  : in Header_Type ) is null;

 -- utilities

 procedure Copy_HDU ( FromFile : in File_type;
                      FirstHDU : in Positive;
                      LastHDU  : in Positive;
                      ToFile   : in File_type);


 -- HDU access

 -- API TB Defined :
 -- To fill-in the DataUnit of a HDU, first gain access to HDU (TBD)
 -- and operate on that HDU (size cannot be changed anymore)
 -- Only File-level functions can modify size of an HDU.


 -- HDU info

 -- FIXME consider to return only one preformatted string per HDU
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

 function  FitsFile_Info ( Fits : File_Type ) return All_HDU_Info;

private

 type File_Data;
 type File_Type is access File_Data;

 -- FITS standard size definitions

 CardSize        : constant Positive := 80;
 BlockSize       : constant Positive := 2880; -- [FITS, Sect xxx]
 CardsCntInBlock : constant Positive := BlockSize / CardSize; -- 36

 subtype Card_Type is String(1..CardSize); -- makes sure index start with 1
 ENDCard  : Card_Type := "END                                                                             ";

 subtype Block_Type is String (1 .. BlockSize );
 type BlockArray_Type is array ( Positive range <> ) of Block_Type;

 -- HDU Records
 -- It is linked list of HDU info about
 -- positions and sizes of HDU's in FITS File.
 -- Open and Write will insert HDU Records into the list
 -- Create initializes an empty list
 -- Close destroys the list

 -- low-level file access by Blocks

 -- file positioning by Blocks, Index: 1,2,...

 function  Index ( File  : in File_Type ) return Positive;
 -- current Index to Block

 procedure Set_Index ( File  : in File_Type;
                       Index : in Positive );
 -- set Index to Block


 procedure Write ( File    : in File_Type;
                   Blocks  : in BlockArray_Type;
                   NBlocks : in Positive := 1);

 function  Read ( File    : in  File_Type;
                  NBlocks : in  Positive := 1)
  return BlockArray_Type;

 procedure Copy_Blocks (FromFile   : in File_Type;
                        FirstBlock : in Positive; -- Index of First to copy
                        LastBlock  : in Positive; -- Index of Last to copy
                        ToFile     : in File_Type) is null;
 -- copy FromFile( FirstBlock .. LastBlock ) --> ToFile

end FITS_IO;
