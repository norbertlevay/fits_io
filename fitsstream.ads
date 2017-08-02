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
-- Set_Index allows positioning in the stream if the meda allows it
-- (for files yes, for network maybe?).

with Ada.Streams;

package FITSStream is

-- type Header_Rec is new Ada.Streams.Stream_Type

-- procedure Set_Index(Stream,HDU_Num);

-- procedure Read (Stream,Header_Rec);
-- procedure Write(Stream,Header_Rec);

-- use Read  as 'Read  for Header_Rec;
-- use Write as 'Write for Header_Rec;

end FITSStream;

---------------
-- Notes:

-- function Index(Stream) return Positive_Count; <- maybe not needed??
   -- returns HDU_Num within which the Stream_IO.Index currently resides
