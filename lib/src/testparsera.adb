
with Ada.Text_IO; use Ada.Text_IO;

with FITS; use FITS;
with FITS.ParserA; use FITS.ParserA;
with FITS.Keyword; use FITS.Keyword;
with FITS.Header; use FITS.Header;





procedure testparsera is

  Cards : array (Positive range <>) of Card_Type := (
"SIMPLE  =                    T / file does conform to FITS standard             ",
"BITPIX  =                  -32 / number of bits per data pixel                  ",
"NAXIS   =                    2 / number of data axes                            ",
"NAXIS2  =                  100 / length of data axis 2                          ",
"EXTEND  =                    T / FITS dataset may contain extensions            ",
"COMMENT   FITS (Flexible Image Transport System) format is defined in 'Astronomy",
"COMMENT   and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H ",
"ORIGIN  = 'NOAO-IRAF FITS Image Kernel December 2001' / FITS file originator    ",
"DATE    = '2003-07-24T18:28:58' / Date FITS file was generated                  ",
"NAXIS1  =                  100 / length of data axis 1                          ",
"IRAF-TLM= '10:28:58 (24/07/2003)' / Time of last modification                   ",
"OBJECT  = 'U5780205B[1/4]'     / Name of the object observed                    ",
"END                                                                             "
  );

  PKeys : In_Key_List.List;
  FKeys : Key_List.List;

--  simple : Keyword_Type := (Name => Max_8.To_Bounded_String("SIMPLE"));
  simple_ptr : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("SIMPLE"));
  bitpix_ptr : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("BITPIX"));
  naxis_ptr  : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("NAXIS"));
  naxisarr_ptr  : Keyword_Ptr := new Indexed_Keyword_Type'(Name => Max_8.To_Bounded_String("NAXIS"),
                                                           Index_First =>  1,
                                                           Index_Last  =>999,
                                                           Index       =>0);

   procedure PrintKey(Position : Key_List.Cursor)
   is
    CurKey : Key_Record_Type := Key_List.Element(Position);
   begin
      Put_Line(Max_8.To_String(CurKey.Name) &
      " " &
      Max20.To_String(CurKey.Value) &
      " " &
      Max48.To_String(CurKey.Comment));
   end PrintKey;

   procedure PrintInKey(Position : In_Key_List.Cursor)
   is
    CurKey : Keyword_Ptr := In_Key_List.Element(Position);
   begin
      Put_Line(Max_8.To_String(CurKey.all.Name));
   end PrintInKey;


begin
--   procedure Parse(Card          : in Card_Type;
--                   Keys_To_Parse : in out In_Key_List.List;
--                   Found_Keys    : in out Key_List.List);



-- PKeys.Append(Keyword_Type'(Max_8.To_Bounded_String("SIMPLE"))'Access);
 PKeys.Append(simple_ptr);
 PKeys.Append(bitpix_ptr);
 PKeys.Append(naxis_ptr);
 PKeys.Append(naxisarr_ptr);

 PKeys.Iterate(PrintInKey'access);
 Ada.Text_IO.New_Line;

 -- parse each incoming card

 for I in Cards'Range
 loop
   Ada.Text_IO.Put(Cards(I));
   Parse(Cards(I), PKeys, FKeys);
 end loop;

 -- print parsed

 Ada.Text_IO.New_Line;
 FKeys.Iterate(PrintKey'access);

 -- convert to Ada struct

 declare
    DUSize : DU_Size_Type := To_DU_Size_Type(FKeys);
 begin

    -- print the struct

    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line("BITPIX = " & Integer'Image(DUSize.BITPIX));

    for I in DUSize.NAXISArr'Range
    loop
     Ada.Text_IO.Put_Line("NAXIS" &
       Integer'Image(I) &
       " = " &
       Integer'Image(DUSize.NAXISArr(I)));
    end loop;

 end;

end testparsera;

