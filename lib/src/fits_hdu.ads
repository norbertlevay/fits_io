
package HDU is

   type HDU_Type is limited  private;
   -- HDU_Type plays the role of File_Type in functional-API
   --e.g. represents the stream
   -- it will hold SIO.File_Type and Indexes: HDU_Start ENDCard_Pos, DU_First,DU_Last
   -- and should be capable of 'Read 'Write (like cin << blabla; )


   -- Header serves as attributes -> access with Get/Set funcs by category as in Standard
   -- FIXME instead of Key_Array/Card_Array is in Ada Map (container of Key-Value pair) ?

   function  Get(HDU : HDU_Type; Keys  : Key_Array) return String80_Array;
   procedure Set(HDU : HDU_Type; Cards : String80_Array);

   -- where

   Biblio      : constant Key_Array;
   Observation : constant Key_Array;
   -- etc...


   function Stream return Data_Stream_Access;
   -- return underlying Stream to write/read data:
   -- T_Arr'Read(Data_Stream, Data)
   -- T_Arr'Write(Data_Stream, Data)

   private

   type HDU_Control_Block is -- extends Stream_IO.Stream/File_Type 
           record
         DU_Stream : Ada.Stream_Access;
         Header : Header_Unit;
         Data   : Data_Unit;
      end record;
   -- Is like File_Control_Block e.g. controls access to one DU within FitsFile


   -- MORE NOTES detalining the above:

   function Stream(File : FITS_IO.File_Type; HDUNum : Count) returns Stream_Access;

   -- OR / AND
   function FITS_Streams(File : FITS_IO.File_Type) returns HDU_Stream_Access_Array;
   -- returns record: {Prim; Ext[n]} each of type Stream_Access


   -- HOW TO ACCESS STREAM ATTRIBITES -->>

   -- NOTE RM 13.13.1 The Package Streams :

   -- "The abstract type Root_Stream_Type is the root type of the class of stream types.
   -- The types in this class represent different kinds of streams.
   -- A new stream type is defined by extending the root type (or some other stream type),
   -- overriding the Read and Write operations, 
   -- and optionally defining additional primitive subprograms, according to the
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- requirements of the particular kind of stream." 

   -- e.g. Add to HDU_Control_Block primitive Get/Set funcs for Undef_Value and Scaling_AB
   --

   function  Get_Undef(HDU : HDU_Type) return Float;
   procedure Set_Undef(HDU : HDU_Type; Undef_Value : Float);

   function  Get_Scaling_A(HDU : HDU_Type) return Float;
   function  Get_Scaling_B(HDU : HDU_Type) return Float;
   procedure Set_Scaling_A(HDU : HDU_Type; A : Float);
   procedure Set_Scaling_B(HDU : HDU_Type; B : Float);

  -- HDU is return from Stream(FitsFIle) return Stream_Access
   -- where HDU_Type is derived from Root stream access (via Stream_IO)

end HDU;

