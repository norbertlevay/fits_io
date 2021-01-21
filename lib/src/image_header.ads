


package Image_Header is

   subtype NAXIS_Index is Integer range 1 .. 999;
   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

   -- Image metadata

   type DU_Type is
      (Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

   type Image_Rec(NAXIS : NAXIS_Index; Card_Count : Count) is
      record
         Data_Type   : DU_Type;
         NAXISn      : NAXIS_Array(1 .. NAXIS);
         Image_Cards : String_80_Array(1 .. Card_Count);
      end record;


   function  Read_Header   -- Parse_Image_Header
      (File : in out File_Type;
      Keys   : BS_8_Array)
      return Image_Rec;
   -- Header.Read_Mandatory(F,...) -- both run parser, feeding one Card at a time:
   -- Header.Read_Optional(F,...)    --> Read_Card(F,...)
                                     --  buffer read by Blocks: Card_Block'Read(S,...)

   function  Read_Cards -- Parse_Cards
      (File : in out File_Type;
      Keys   : BS_8_Array)
      return  String_80_Array;
   -- Header.Read_Optional(F,...)  --> Read_Card(F,...) --> Card_Block'Read(S,..) 


   procedure Write_Header  -- Generate_Image_Header 
      (File : in out File_Type;
      Image : Image_Rec);

   procedure Write_Cards  -- Add_Cards
      (File       : in out File_Type;
      Cards : String_80_Array);


   -- SEPARATE algorithms FROM FILE/STREAM OPERATIONS
   -- this Image_Header shoudl have only Struct ans String80_Arr conversions
   -- e.g. the full logic of converting/composing ImageHeader
   -- e.g. all functionalities below marked '-- *'
   -- FINAL HEADER CAN BE COMPOSED IN STREAM/FILE ONLY BECAUSE MAY BE 'INFINTE' BIG
   -- similarly
   -- PARSING ANY SET OF CARDS CAN BE DONE FROM FILE/STREAM ONLY BECAUSE ARBITRARY CARD MAY BE IN
   -- ANY POSITION IN HEADER WHICH HAS 'INFINITE' LENGTH
   -- 'INFINITE' = cannot load to memory all header; Standard defines no limit on Header length


   -- Write_Cards(S,...):
   -- x overwrite ENDCard (= position at ENDCard)
   -- * append Cards               = String80_Arr              --> String80_Arr'Write(S,C)
   -- * Write ENDCard with padding -> convert to String80_Arr  --> String80_Arr'Write(S,C)


   -- Write Image Header(S,..):
   -- * Write 1st card         -> convert to String80_Arr --> String80_Arr'Write(S,C)
   -- * Write Image/(Bin)Table -> convert to String80_Arr --> String80_Arr'Write(S,C)
   -- * Write Cards            = is String80_Arr          --> String80_Arr'Write(S,C)
   -- * Write End with padding -> convert to String80_Arr --> String80_Arr'Write(S,C)

   --  no need for Stream/File ............................    needs Stream/File

end Image_Header;

