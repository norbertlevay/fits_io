
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
                                HDU_Num  : in Positive ) -- which HDU: 1,2,3,...
  return HDU_Position_Type;
  -- get positions and sizes of HDU's in FitsFile


 -- Header access

 -- read / write Header as one string

 function Read_Header ( InFitsFile : in File_Type;          -- opened FitsFile
                        HDU        : in HDU_Position_Type ) -- from this HDU
  return String;
  -- read Header as one long string
  -- of size Cards*CardSize rounded up to BlockSize
  -- (includes eventual empty cards after END-card)


 procedure Write_Header ( OutFitsFile : in out File_Type;
                          HDU         : in HDU_Position_Type;
                          Header      : in String );
 -- writes Header to position given by HDU
 -- only if Header sizes (counted in Blocks) match

 -- read / write Header as array of cards

 type Header_Type is array (Positive range <> ) of String(1..CardSize);

 function Get_Header ( InFitsFile : in File_Type;
                       HDU        : in HDU_Position_Type )
  return Header_Type;

 procedure Put_Header ( OutFitsFile : in out File_Type;
                        HDU         : in HDU_Position_Type;
                        Header      : in Header_Type );
 -- writes Header to position given by HDU
 -- only if Header sizes (counted in Blocks) match



 -- HDU_Type based calls

 type HDU_Type is
  record
   FitsFile  : File_Type;
   Positions : HDU_Position_Type;
  end record;

 procedure Create ( HDU : in out HDU_Type;
                    Mode : in File_Mode := Out_File;
                    Name : in String    := "";
                    HDU_Num : Natural   := 0;-- Primary HDU
                    Form : in String    := "");
 -- can be first HDU when Out_Mode, or
 -- last HDU of existing files when Append_Mode

 procedure Open ( HDU : in out HDU_Type;
                  Mode : in File_Mode;
                  Name : in String;
                  HDU_Num : Natural   := 0;-- Primary HDU
                  Form : in String := "");

 procedure Close  (HDU : in out HDU_Type);

 function  Get ( HDU : in HDU_Type ) return Header_Type;

 procedure Put ( HDU    : in out HDU_Type;
                 Header : in Header_Type );

end FitsFile;

