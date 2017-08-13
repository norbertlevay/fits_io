
with Ada.Streams.Stream_IO;
--with Ada.Unchecked_Conversion;

 ---------------------------------------------------
 -- Low level FITS-Headers access by HeaderBlocks --
 ---------------------------------------------------

package FITS_IO.Block_IO is

 BlockSize       : constant Positive_Count := 2880; -- [FITS, Sect 3.1]
 CardsCntInBlock : constant Positive_Count := BlockSize / Positive_Count(CardSize);
 -- [FITS 3.3.1 Primary Header] 36 cards per block        -- conv ok, CardSize 80 const

 -- Card

 subtype Card_Type is String(1..CardSize);
 -- makes sure index start with 1

 ENDCard    : constant Card_Type := "END                                                                             ";
 EmptyCard  : constant Card_Type := (others => ' ');

 -- Header Block

 type HeaderBlock_Type is array (1 .. CardsCntInBlock) of Card_Type;
 type HeaderBlockArray_Type is array (Positive_Count range <>) of HeaderBlock_Type;
 -- Header format inside FITS-file (also arrays/records must be packed)

 EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);

 ----------------------
 -- File managemennt --
 ----------------------

 -- we use renames and subtypes to hide Block_IO implementation,
 -- so it can be replaced by Direct_IO or C-Streams implementation
 -- without modification of FITS_IO. E.g. all calls/references to
 -- Block_IO package must be BIO.xxxx prefixed (no reference to SIO).

 package SIO renames Ada.Streams.Stream_IO;

 subtype File_Type is SIO.File_Type;
 subtype Stream_Access is SIO.Stream_Access;
 -- renames types

 type File_Mode is (In_File, Inout_File, Out_File, Append_File);

 procedure Create
   (File : in out File_Type;
    Mode : File_Mode := Out_File;
    Name : String := "";
    Form : String := "");

 procedure Open
   (File : in out File_Type;
    Mode : File_Mode;
    Name : String;
    Form : String := "");

 procedure Close  (File : in out File_Type) renames SIO.Close;

 function  End_Of_File (File : File_Type) return Boolean renames SIO.End_Of_File;

 function  Stream (File : File_Type) return Stream_Access renames SIO.Stream;

 procedure Set_Mode (File : in out File_Type; Mode : File_Mode);

 -- positioning in file

 function  BlockIndex ( File  : in File_Type ) return Positive_Count;

 procedure Set_BlockIndex ( File  : in File_Type;
                            Index : in Positive_Count ); -- Block Index

 -------------------------
 -- Read / Write Header --
 -------------------------

 procedure Write(File    : in File_Type;
                 Blocks  : in HeaderBlockArray_Type);

 function  Read (File    : in File_Type;
                 NBlocks : in Positive_Count := 1) return HeaderBlockArray_Type;

end FITS_IO.Block_IO;
