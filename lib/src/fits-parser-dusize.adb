
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with FITS.Header; use FITS.Header;

package body FITS.Parser.DUSize is

   -- holds array of keywords which will be parsed
   DU_Size_Keys : constant array (Positive range <>) of Key := (
    (Max_8.To_Bounded_String("BITPIX"), False, 0,   1, False),
    (Max_8.To_Bounded_String("NAXIS"),  False, 0,   1, False),
    (Max_8.To_Bounded_String("NAXIS"),  True,  1, 999, False)
   );


   -- in PType use List to collect NAXIS vector elements

   type PType is record
    dummy : Positive;
    -- l : List; -- list element instead of NAXIS_Arr vector
   end record;

   type UType is record
    dummy : Positive;
   end record;

   PData  : PType;
   UData  : UType;
   -- FIXME put both to Heap or Global-mem

   -- internally uses DU_Size_Keys array
   function Parse_Card_DUSize
                    (Card      : in     Card_Type;
                     PData     : in out PType;
                     UData     : in out UType)
                     return Boolean
   is
    -- remove spaces
    CardKey : String := Ada.Strings.Fixed.Trim(Card(1..8),Ada.Strings.Both);
    CurKey  : Key;
   begin

    for I in DU_Size_Keys'Range
    loop
      CurKey := DU_Size_Keys(I);
      declare
       CurKey_Name : String := Max_8.To_String(CurKey.Name);
      begin

       -- parse out values, indexes into PData
       if (CurKey.Indexed) then
         if(CurKey_Name = CardKey) then
            -- convert Key Index and Card value and store
            -- them into PData dynamic array (List or Container.Vector)
            CurKey.Found := True;
            -- FIXME should be true only if all array found
         end if;
       else -- pure Key
         if(CurKey_Name = CardKey) then
            CurKey.Found := True;
            -- convert Card value to PData value
         end if;
       end if;

      end;
    end loop;

    return False;
   end Parse_Card_DUSize;

   function NAXISList_Length(PData : in PType) return Natural
   is
   begin
    return 1;
   end NAXISList_Length;

   -- first parse IndexedKeys into a List
   -- read length and provide it to DU_Size_Type(Length)
   -- convert List into NAXISArr and return definite DUSize
   procedure Parse_Header_For_DUSize is
         new Parse_Header(Parsed_Type => PType,
                          User_Type   => UType,
                          Parse_Card  => Parse_Card_DUSize);


   function Parse_To_List return Natural
   is
     Source : FITS.Parser.Source_Type;
   begin
    -- actual Parsing happens here
    Parse_Header_For_DUSize(Source, PData, UData);
    return NAXISList_Length(PData);
   end Parse_To_List;


   ---------------
   -- interface --
   ---------------

   function Parse_DUSize(Source : in Source_Type)
     return DU_Size_Type
     is
      Len : Positive := Parse_To_List;
      DUSize : DU_Size_Type(Len);
     begin
      -- convert from List to array and fill in DUSize
      return DUSize;
     end Parse_DUSize;

end FITS.Parser.DUSize;
