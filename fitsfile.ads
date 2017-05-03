
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;

package FitsFile is

 CardSize        : constant Positive := 80;
 CardsCntInBlock : constant Positive := 36;
 BlockSize       : constant Positive := CardSize*CardsCntInBlock;

 type HDU_Type is limited private;

 procedure Create ( HDU : in out HDU_Type;
                    Mode : in File_Mode := Out_File;
                    Name : in String    := "";
                    HDU_Num : Positive  := 1;-- Primary HDU FIXME decide count from 0 or 1
                    Form : in String    := "");
 -- creates first HDU of a new file when in Out_Mode, or
 -- last HDU of an existing file when in Append_Mode

 procedure Open ( HDU : in out HDU_Type;
                  Mode : in File_Mode;
                  Name : in String;
                  HDU_Num : Positive := 1;-- Primary HDU FIXME decide count from 0 or 1
                  Form : in String   := "");

 procedure Close ( HDU : in out HDU_Type );


 -- access Header

 type HeaderBlock_Type  is array (1 .. CardsCntInBlock) of String(1..CardSize);
 EmptyCard  : constant String(1..CardSize) := (others => ' ');
 EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);
 type HeaderBlocks_Type is array (Positive range <> ) of HeaderBlock_Type;

 function  Get ( HDU : in HDU_Type ) return HeaderBlocks_Type;

 procedure Put ( HDU          : in out HDU_Type;
                 HeaderBlocks : in HeaderBlocks_Type );


 -- positioning

 function Index(HDU : HDU_TYPE) return Positive_Count;
 procedure Set_Index(HDU : HDU_TYPE; Index : Positive_Count );
 -- get/set Index in bytes (StreamElement which in GNAT is Byte)

 function Header_Index( HDU : HDU_Type ) return Positive_Count;
 function Data_Index  ( HDU : HDU_Type ) return Positive_Count;
 -- return index from start of the FITS file where Header and DataUnit start
 function Header_Size( HDU : HDU_Type ) return Positive_Count;
 function Data_Size  ( HDU : HDU_Type ) return Positive_Count;
 -- return Header and DataUnit sizes in bytes (StreamElement which in GNAT is Byte)

 function Size(HDU : HDU_TYPE) return Positive_Count;
 -- FIXME returns File-Size which is FILE-level operation not HDU-level: handle differently
 procedure Copy_Blocks(FromFile  : HDU_Type; ToFile : File_Type;
                       FromBlock : Positive; ToBlock : Natural );
 -- FIXME assymetric FromFile->HDUType ToFile FileType: handle differently

private

 type HDU_Position_Type is record
  Header_Index : Positive_Count;
  Header_Size  : Positive_Count;
  Data_Index   : Positive_Count; -- FIXME there can be HDU without DataUnit e.g. Size = 0
  Data_Size    : Positive_Count; -- but Positive_Count is >0 !! Count is >=0 Ada.Stream.Stream_IO
 end record;

 HDU_Null : constant HDU_Position_Type := (1,1,1,1);
 -- FIXME Positive_Count does not allow 0, use Natural for size Primary HDU can be without DataUnit

 type HDU_Type is
  record
   FitsFile  : File_Type;
   Positions : HDU_Position_Type;
  end record;

end FitsFile;

