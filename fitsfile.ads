
with Ada.Streams.Stream_IO;
-- File_Type needed in HDU_Type FIXME

package FitsFile is

 CardSize        : constant Positive := 80;
 CardsCntInBlock : constant Positive := 36;
 BlockSize       : constant Positive := CardSize*CardsCntInBlock;

 type HDU_Type is limited private;
 type HDU_Mode is (In_HDU, Out_HDU, Inout_HDU, Append_HDU);

 procedure Create ( HDU  : in out HDU_Type;
                    Mode : in HDU_Mode;
                    Name : in String;
                    HDU_Num : Positive  := 1;-- default is Primary HDU
                    Form : in String    := "");
 -- if Mode Out_File   : creates first HDU of a new file
 -- if Mode Append_Mode: creates new HDU of an existing file
 -- FIXME check Create + Append behaviour: 2nd case should map to OpenFile in Append_Mode ?

 procedure Open ( HDU  : in out HDU_Type;
                  Mode : in HDU_Mode;
                  Name : in String;
                  HDU_Num : Positive := 1;-- default is Primary HDU
                  Form : in String   := "");

 procedure Close ( HDU : in out HDU_Type );


 -- access Header

 type HeaderBlock_Type  is array (1 .. CardsCntInBlock) of String(1..CardSize);
 EmptyCard  : constant String(1..CardSize) := (others => ' ');
 EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);
 type HeaderBlocks_Type is array (Positive range <> ) of HeaderBlock_Type;

 function  Read ( HDU : in HDU_Type ) return HeaderBlocks_Type;

 procedure Write ( HDU          : in out HDU_Type;
                 HeaderBlocks : in HeaderBlocks_Type );


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

 function  Index(HDU : HDU_TYPE) return Positive;
 procedure Set_Index(HDU : HDU_TYPE; Index : Positive );
 -- get/set Index in bytes (StreamElement which in GNAT is Byte)

 procedure Copy_Blocks(FromFile  : HDU_Type;
                       FromBlock : Positive; ToBlock : Natural;
                       ToFile  : HDU_Type );

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

end FitsFile;

