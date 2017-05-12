
with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;
-- File_Type needed in HDU_Type FIXME

package HDU is

 type HDU_Type is limited private;
 type HDU_Mode is (In_HDU, Out_HDU, Inout_HDU, Append_HDU);

 procedure Create ( HDU  : in out HDU_Type;
                    Mode : in HDU_Mode;
                    Name : in String;
                    HDU_Num : Positive  := 1;-- default is Primary HDU
                    Form : in String    := "");

 procedure Open ( HDU  : in out HDU_Type;
                  Mode : in HDU_Mode;
                  Name : in String;
                  HDU_Num : Positive := 1;-- default is Primary HDU
                  Form : in String   := "");

 procedure Close ( HDU : in out HDU_Type );


 -- Header definition

 CardSize : constant Positive := 80;
 package SB is new Ada.Strings.Bounded.Generic_Bounded_Length(CardSize);
 type Header_Type is array (Positive range <>) of SB.Bounded_String;
 -- Header is array of lines, each line max 80 chars long

 -- Header access

 function  Read ( HDU : in HDU_Type ) return Header_Type;

 procedure Write ( HDU    : in out HDU_Type;
                   Header : in Header_Type );

 function Size( Header : Header_Type ) return Natural;
 -- returns Header size in Blocks


 -- positioning, FIXME these are FITS-file operations
 -- should be re-implemented when FITS_File_Type added
 -- now they serve as a workaround for new-Header write

 -- FITS file consists of blocks 2880-bytes (aka BlockSize)
 -- smallest addressable unit for file operatrions is Block
 -- all indexes, sizes are given in count of Blocks

 function Header_Index( HDU : HDU_Type ) return Positive;
 function Data_Index  ( HDU : HDU_Type ) return Positive;
 -- return index from start of the FITS file where Header and DataUnit start
 function Header_Size( HDU : HDU_Type ) return Natural;
 function Data_Size  ( HDU : HDU_Type ) return Natural;
 -- return Header and DataUnit sizes in bytes (StreamElement which in GNAT is Byte)

 function  Size(HDU : HDU_TYPE) return Positive;
 -- file size in blocks

 function  Index(HDU : HDU_TYPE) return Positive;
 procedure Set_Index(HDU : HDU_TYPE; Index : Positive );
 -- get/set Index in bytes (StreamElement which in GNAT is Byte)

 procedure Copy_Blocks(FromFile  : HDU_Type;
                       FromBlock : Positive; ToBlock : Natural;
                       ToFile  : HDU_Type );

 -- internal info - temporarly visible for FITS_IO
   BlockSize       : constant Positive := 2880;
   CardsCntInBlock : constant Positive := BlockSize / CardSize; -- 36 cards per block
   type HeaderBlock_Type  is array (1 .. CardsCntInBlock) of String(1..CardSize);
   EmptyCard  : constant String(1..CardSize) := (others => ' ');
   EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);
   type HeaderBlocks_Type is array (Positive range <> ) of HeaderBlock_Type;
   -- Header format inside file
   function To_HeaderBlocks( Header : Header_Type )
    return HeaderBlocks_Type;


private

 type HDU_Position_Type is record
  Header_Index : Positive;
  Header_Size  : Natural;
  Data_Index   : Positive;
  Data_Size    : Natural;
 end record;

 HDU_Null : constant HDU_Position_Type := (1,0,1,0);

 type HDU_Type is
  record
   FitsFile  : Ada.Streams.Stream_IO.File_Type;
   Positions : HDU_Position_Type;
  end record;

end HDU;

