


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


  procedure Write_First_Image_Card(t_File : in out File_Type; Is_Primary : Boolean);
  -- * create First_SIMPLE Card
  -- * create First_XTENSION Card
  -- write one or other by Is_Primary

   procedure Write_Image -- alg
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array;
      Is_Primary  : Boolean);
   -- * create Header.Primary Rec
   -- * create Header.Extension Rec (PCOUNT/GCOUNT)
   -- * generate Prim Cards
   -- * generate Ext Cards
   -- write Prim or Ext Cards by Is_Primary
   -- calc and store DU_Length


   procedure Write_Cards  -- Add_Cards
      (File : in out File_Type;
      Cards : String_80_Array);
   -- set index to ENDCard
   -- write Cards
   -- write ENDCard and padding

   procedure Write_End(FFile : in out File_Type);
   -- store ENDCard position
   -- write ENDCard
   -- write padding
   -- store DU_First position


   -- ----------------------------------------

   function Create_Card_SIMPLE(V : Boolean) return String_80_Array;
   function Create_Card_XTENSION_IMAGE return String_80_Array;

   Image_Rec : BITPIX NAXISn
   function Generate_Cards_Primary  (Im : Image_Rec) return String_80_Array;
   function Generate_Cards_Extension(Im : Image_Rec) return String_80_Array;
   function Generate_Cards_Extension(Tab    : Table_Rec)    return String_80_Array;
   function Generate_Cards_Extension(BinTab : BinTable_Rec) return String_80_Array;

end Image_Header;

