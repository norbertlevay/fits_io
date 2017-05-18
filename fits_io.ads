

with Ada.Strings.Bounded;
 -- for Header definition

package FITS_IO is

 type File_Type is limited private;
 type File_Mode is (In_File, Out_File, Inout_File, Append_File);

-- valid combinations:
-- Open   : In Inout Append
-- Create : Append

 procedure Create ( Fits : in out File_Type;
                    Mode : in File_Mode;
                    Name : in String;
                    Form : in String := "shared=no");--[GNAT 9.2 FORM strings]

 procedure Open ( Fits : in out File_Type;
                  Mode : in File_Mode;
                  Name : in String;
                  Form : in String := "shared=no");--[GNAT 9.2 FORM strings]

 function  Mode ( File : in  File_Type ) return File_Mode;
 procedure Set_Mode ( File : in out File_Type;
                      Mode : in File_Mode );

 procedure Close ( Fits : in out File_Type );

 -- FITS-file level fucntionalities may change size of HDU's
 -- and so HDU's need to be shifted in file

 -- with existing Header

 -- Header definition
 CardSize : constant Positive := 80;
 package SB is new Ada.Strings.Bounded.Generic_Bounded_Length(CardSize);
 type Header_Type is array (Positive range <>) of SB.Bounded_String;
 -- Header is array of lines, each line max 80 chars long

 function Read  ( File    : in  File_Type;
                  HDU_Num : in  Positive )
  return Header_Type;

 HDU_Last : constant := Positive'Last;-- FIXME this id tight to Positive range definition of HDU_Arr
 -- FIXME is this goos idea at all ?
 --   Better use separate Append(File,Header) besides Write(File,Header,HDU_Num)
 procedure Write ( File    : in  File_Type;
                   Header  : in  Header_Type;
                   HDU_Num : in  Positive := HDU_Last ); -- default: Append
 -- Open   + Out_File   -> Write(...,HDU_Num) truncates FITS-File and appends Header to the truncated end
 -- Open   + Inout_File -> Write(...,HDU_Num) overwrites HDU if sizes match (sizes counted in Blocks = 2880bytes)
 -- Open/Create + Append -> Write() (call without HDU_Num ) appends to the end

 -- FITS-file structure (HDU's)

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

 BlockSize       : constant Positive := 2880; -- [FITS, Sect xxx]
 CardsCntInBlock : constant Positive := BlockSize / CardSize; -- 36 cards per block

 subtype Card_Type is String(1..CardSize); -- makes sure index start with 1
 ENDCard  : Card_Type := "END                                                                             ";

 subtype Block_Type is String (1 .. BlockSize );
 type BlockArray_Type is array ( Positive range <> ) of Block_Type;

 type HeaderBlock_Type  is array (1 .. CardsCntInBlock) of String(1..CardSize);
 EmptyCard  : constant String(1..CardSize) := (others => ' ');
 EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);
 type HeaderBlocks_Type is array (Positive range <> ) of HeaderBlock_Type;
   -- Header format inside file

 function To_HeaderBlocks( Header : Header_Type )
  return HeaderBlocks_Type;

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
