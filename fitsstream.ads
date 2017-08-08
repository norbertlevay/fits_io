--
-- This package serializes/deserializes FITS-Header
-- according to FITS standard, so it is ready for File-streams (Ada.Streams.Stream_IO)
-- or other stream-media.
--
-- Users of this package should do stream-media management:
-- see Stream_IO for files, or some other package for network media.
-- Once Stream is successfully acquired, this package can be used to
-- serialize and place FITS-Header into the stream and/or
-- to get it ("deserialize") from the stream.
--
-- Set_Index allows positioning in the stream if the media allows it
-- (for files yes, for network maybe?).

with Ada.Streams;

package FITSStream is

-- type Header_Rec is new Ada.Streams.Stream_Type ?? needs to derive from Stream - probably not

-- function List_Content(Stream) return HDU_Info_Array;
   -- list HDU properties (Cards, Data Type and dimensionality)

-- procedure Set_Index(Stream,HDU_Num);
   -- set file-index to begining of the HDU

-- procedure Read_Header (Stream,Header_Type);
-- procedure Write_Header(Stream,Header_Type);

-- procedure Read_Data (Stream,Data_Array,Offset);
-- procedure Write_Data(Stream,Data_Array,Offset);
   -- Offset counted in Data_Type relative to start of Data_Unit (after the Header)

-- for Header_Type'Read  use Read_Header;
-- for Header_Type'Write use Write_Header;

-- for Data_Type'Read  use Read_Data;
-- for Data_Type'Write use Write_Data;

end FITSStream;

---------------
-- Notes:

-- function Index(Stream) return Positive_Count; <- maybe not needed??
   -- returns HDU_Num within which the Stream_IO.Index currently resides
