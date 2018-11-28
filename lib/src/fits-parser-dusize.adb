

package body FITS.Parser.DUSize is


   -- in PType use List to collect NAXIS vector elements

   type PType is record
    dummy : Positive;
    -- l : List; -- list element instead of NAXIS_Arr vector
   end record;

   function NAXISList_Length(PData : in PType) return Natural
   is
   begin
    return 1;
   end NAXISList_Length;

   type UType is record
    dummy : Positive;
   end record;

   function Parse_Card_DUSize
                    (Card      : in     Card_Type;
                     PData     : in out PType;
                     UData     : in out UType)
                     return Boolean
   is
   begin
    return False;
   end Parse_Card_DUSize;


   -- first parse IndexedKeys into a List
   -- read length and provide it to DU_Size_Type(Length)
   -- convert List into NAXISArr and return definite DUSize
   procedure Parse_Header_For_DUSize is
         new Parse_Header(Parsed_Type => PType,
                          User_Type   => UType,
                          Parse_Card  => Parse_Card_DUSize);



   function Parse_To_List return Natural
   is
     PData  : PType;
     UData  : UType; -- FIXME put both to Heap
     Source : FITS.Parser.Source_Type;
   begin
    -- actual Parsing happens here
    Parse_Header_For_DUSize(Source, PData, UData);
    return NAXISList_Length(PData);
   end Parse_To_List;


   function Parse_DUSize(Source : in Source_Type)
     return DU_Size_Type
     is
      Len : Positive := Parse_To_List;
      DUSize : DU_Size_Type(Len);
     begin
      -- convert from List to array
      return DUSize;
     end Parse_DUSize;

end FITS.Parser.DUSize;
