
--with Ada.Text_IO;
--with Ada.Tags; use Ada.Tags;
--with Ada.Streams.Stream_IO;
--with Ada.Strings.Fixed; use  Ada.Strings.Fixed;
--with Ada.Strings.Bounded; use  Ada.Strings.Bounded;
--with FITS.Header; use FITS.Header;
--child can see all includes of Parent??

with Ada.Containers.Doubly_Linked_Lists;

package body FITS.Parser.DUSize is


   function Naxis(ParsedKeys : in Out_Key_List.List) return Positive
   is
    KeyRec   : Key_Record_Type;
    CurPos   : Out_Key_List.Cursor;
   begin
    CurPos := Out_Key_List.First(ParsedKeys);
    while Out_Key_List.Has_Element(CurPos)
    loop
        KeyRec := Out_Key_List.Element(CurPos);
        exit when Max_8.To_String(KeyRec.Name) = "NAXIS";
        Out_Key_List.Next(CurPos);
    end loop;
    return Positive'Value(Max20.To_String(KeyRec.Value));
   end Naxis;


   function To_DU_Size_Type(ParsedKeys : in Out_Key_List.List)
     return DU_Size_Type
   is
     DUSize : DU_Size_Type(Naxis(ParsedKeys));
     KeyRec : Key_Record_Type;
     CurPos : Out_Key_List.Cursor;
     Idx : Natural;
   begin
    CurPos := Out_Key_List.First(ParsedKeys);
    while Out_Key_List.Has_Element(CurPos)
    loop

        KeyRec := Out_Key_List.Element(CurPos);

        declare
         Name : String := Max_8.To_String(KeyRec.Name);
        begin
         if(Name = "BITPIX") then
           DUSize.BITPIX := Integer'Value(Max20.To_String(KeyRec.Value));
         elsif ( Name(1..5) = "NAXIS" ) then

          if( Name'Length > 5 ) then
             Idx := Natural'Value( Name(6..Name'Last) );
             DUSize.NAXISArr(Idx) := Natural'Value(Max20.To_String(KeyRec.Value));
           end if;

         end if;
        end;

        Out_Key_List.Next(CurPos);
    end loop;
     return DUSize;
   end To_DU_Size_Type;


   function Init_List return In_Key_List.List
   is
    InKeys :  In_Key_List.List;
    simple_ptr : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("SIMPLE"));
    bitpix_ptr : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("BITPIX"));
    naxis_ptr  : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("NAXIS"));
    naxisarr_ptr  : Keyword_Ptr := new Indexed_Keyword_Type'(Name => Max_8.To_Bounded_String("NAXIS"),
                                                           Index_First =>  1,
                                                           Index_Last  =>999,
                                                           Index       =>0);
   begin
    InKeys.Append(simple_ptr);
    InKeys.Append(bitpix_ptr);
    InKeys.Append(naxis_ptr);
    InKeys.Append(naxisarr_ptr);
    return InKeys;
   end Init_List;


   function Parse_Header_For_DUSize(Source : in Source_Type)
     return DU_Size_Type
   is
    function PH is new Parse_Header(Source_Type => Source_Type,
                                    Next        => Next);
    PKeys : In_Key_List.List  := Init_List;
    FKeys : Out_Key_List.List := PH(Source,PKeys);
    DUSize : DU_Size_Type := To_DU_Size_Type(FKeys);
   begin
    return DUSize;
   end Parse_Header_For_DUSize;

end FITS.Parser.DUSize;
