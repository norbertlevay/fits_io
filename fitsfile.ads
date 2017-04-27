
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

 function Parse_HDU_Positions ( FitsFile : in File_Type; -- opened FitsFile
                                HDU_Num  : in Positive ) -- which HDU: 1,2,3,...
  return HDU_Position_Type;
  -- get positions and sizes of HDU's in FitsFile


 -- Header access

 function Read_Header ( InFitsFile : in File_Type;          -- opened FitsFile
                        HDU        : in HDU_Position_Type ) -- from this HDU
  return String;
  -- read Header as one long string
  -- of size Cards*CardSize rounded up to BlockSize
  -- (includes eventual empty cards after END-card)


 procedure Write_Header ( OutFitsFile : in out File_Type;
                          HDU         : in HDU_Position_Type;
                          Header      : in String );
 -- writes Header to position given by HDU_Pos
 -- only if Header sizes (counted in Blocks) match

 procedure Write_Header_To_New_FitsFile ( InFitsFile : in File_Type;
                                          HDU        : in HDU_Position_Type;
                                          Header     : in String );
 -- writes Header to position given by HDU_Pos
 -- this func always creates new Fits File assuming
 -- that size of Headers do not match and so Blocks
 -- need to be shifted

end FitsFile;

