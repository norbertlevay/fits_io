
with Ada.Streams;
with Numeric_Type;

generic
 with package Raw      is new Numeric_Type(<>);
 with package Physical is new Numeric_Type(<>);
package Array_IO is

   procedure Raw_To_Phys
    (Raw_Arr : in Raw.Numeric_Arr;
    A,B : in Float;
    Phys_Arr : out Physical.Numeric_Arr);

   procedure Phys_To_Raw
    (Raw_Arr : out Raw.Numeric_Arr;
    A,B : in Float;
    Phys_Arr : in Physical.Numeric_Arr);

   procedure To_Sream_Elements_Array
      (Raw_Arr : in Raw.Numeric_Arr;
      SE_Arr : out Ada.Streams.Stream_Element_Array) is null;

   procedure To_Raw_Numeric_Array
      (Raw_Arr : out Raw.Numeric_Arr;
      SE_Arr : in Ada.Streams.Stream_Element_Array) is null;


      -- NOTE not really Read/Write but these could
      -- substitute TT_AIO.Read/Write in Fits_IO.HDU_Write/Read:
      -- has no Stream, but Stream compatible data(pointers!!)
      -- (SEA is array of alised elements)
procedure Read_From_SE_Array
    (SE_Arr : in Ada.Streams.Stream_Element_Array;
    A,B : in Float;
    Phys_Arr : out Physical.Numeric_Arr) is null;

procedure Write_To_SE_Array
    (SE_Arr : out Ada.Streams.Stream_Element_Array;
    A,B : in Float;
    Phys_Arr : in Physical.Numeric_Arr)is null;





   -- orig API

procedure Read
    (S : not null access Ada.Streams.Root_Stream_Type'Class; 
    A,B : in Float;
    Phys_Arr : out Physical.Numeric_Arr);

procedure Write
    (S : not null access Ada.Streams.Root_Stream_Type'Class;
    A,B : in Float;
    Phys_Arr : in Physical.Numeric_Arr);

end Array_IO;

