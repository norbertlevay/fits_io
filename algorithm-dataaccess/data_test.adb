
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;

with Interfaces;
with Data_Types; use Data_Types;

procedure data_test is

package TIO renames Ada.Text_IO;
package SIO renames Ada.Streams.Stream_IO;


--BlockUInt8 : UInt8.Block := (others => 0);
BlockInt16 : Int16.Block := (others => 0);
BlockInt32 : Int32.Block := (others => 0);
BlockInt64 : Int64.Block := (others => 0);
BlockFloat32 : F32.Block := (others => 0.0);
BlockFloat64 : F64.Block := (others => 0.0);

File : SIO.File_Type;

begin

--Put("UInt8  : "& Positive'Image(BlockUInt8'First));
--Put_Line(" - " & Positive'Image(BlockUInt8'Last));

Put("Int16  : "& Positive'Image(BlockInt16'First));
Put_Line(" - " & Positive'Image(BlockInt16'Last));

Put("Int32  : "& Positive'Image(BlockInt32'First));
Put_Line(" - " & Positive'Image(BlockInt32'Last));

Put("Int64  : "& Positive'Image(BlockInt64'First));
Put_Line(" - " & Positive'Image(BlockInt64'Last));

Put("Float32: "& Positive'Image(BlockFloat32'First));
Put_Line(" - " & Positive'Image(BlockFloat32'Last));

Put("Float64: "& Positive'Image(BlockFloat64'First));
Put_Line(" - " & Positive'Image(BlockFloat64'Last));


--Data_UInt8.Block'Write(Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output), BlockUInt8);


-- to file

-- fill in data
for I in BlockInt32'Range
loop
 BlockInt32(I) := Data_Types.Integer_32(I);
end loop;



-- write file
SIO.Create(File, SIO.Out_File, "BlockInt32.dat");
Int32.Block'Write(SIO.Stream(File), BlockInt32);
SIO.Close(File);

-- reset values
BlockInt32 := (others => -1);

-- read file
SIO.Open(File, SIO.In_File, "BlockInt32.dat");
Int32.Block'Read(SIO.Stream(File), BlockInt32);
SIO.Close(File);


-- print to screen
for I in BlockInt32'Range
loop
 Put(Data_Types.Integer_32'Image(BlockInt32(I)));
end loop;
New_Line;


end data_test;

