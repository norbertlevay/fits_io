
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;

package FitsFile is

 CardSize        : constant Positive := 80;
 CardsCntInBlock : constant Positive := 36;
 BlockSize       : constant Positive := CardSize*CardsCntInBlock;

 type HDU_Position_Type is record
  Header_Index : Positive_Count;
  Header_Size  : Positive_Count;
  Data_Index   : Positive_Count; -- FIXME there can be (primary?) HDU without DataUnit
  Data_Size    : Positive_Count; -- FIXME Positive_Count is >0 !! Count is >=0 Ada.Stream.Stream_IO
 end record;

 HDU_Null : constant HDU_Position_Type := (1,1,1,1);
 -- FIXME Positive_Count does not allow 0, use Natural for size

 function Parse_HDU_Positions ( FitsFile : in File_Type; -- opened FitsFile
                                HDU_Num  : in Positive ) -- which HDU: 1,2,3,...FIXME decide count from 0 or 1
  return HDU_Position_Type;
  -- get positions and sizes of HDU's in FitsFile


 -- HDU_Type based calls

 type HDU_Type is
  record
   FitsFile  : File_Type;
   Positions : HDU_Position_Type;
  end record;

 procedure Create ( HDU : in out HDU_Type;
                    Mode : in File_Mode := Out_File;
                    Name : in String    := "";
                    HDU_Num : Natural   := 0;-- Primary HDU FIXME decide count from 0 or 1
                    Form : in String    := "");
 -- creates first HDU of a new file when in Out_Mode, or
 -- last HDU of an existing file when in Append_Mode

 procedure Open ( HDU : in out HDU_Type;
                  Mode : in File_Mode;
                  Name : in String;
                  HDU_Num : Natural   := 0;-- Primary HDU FIXME decide count from 0 or 1
                  Form : in String := "");

 procedure Close ( HDU : in out HDU_Type );


 -- access Header

-- type Header_Type is array (Positive range <> ) of String(1..CardSize);

 type HeaderBlock_Type  is array (1 .. CardsCntInBlock) of String(1..CardSize);
 type HeaderBlocks_Type is array (Positive range <> ) of HeaderBlock_Type;

 function  Get ( HDU : in HDU_Type ) return HeaderBlocks_Type;

 procedure Put ( HDU          : in out HDU_Type;
                 HeaderBlocks : in HeaderBlocks_Type );

end FitsFile;

