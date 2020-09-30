

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!

package Optional.Reserved is


-- Conversion with To_Bounded_String renamed:
-- 
--   function BS8(S : String; Drop :Ada.Strings.Truncation := Ada.Strings.Error) 
--        return Bounded_String_8.Bounded_String
--                renames Bounded_String_8.To_Bounded_String;
--   Descriptive_Keys : constant Optional.Bounded_String_8_Arr :=
--        (BS8("DATE"),BS8("REFERENC"),BS8("ORIGIN"),BS8("EXTEND"),BS8("BLOCKED"));


 use BS_8;
 BZERO   : constant BS_8.Bounded_String := 1 * "BZERO";
 BSCALE  : constant BS_8.Bounded_String := 1 * "BSCALE";
 BUNIT   : constant BS_8.Bounded_String := 1 * "BUNIT";
 BLANK   : constant BS_8.Bounded_String := 1 * "BLANK";
 DATAMIN : constant BS_8.Bounded_String := 1 * "DATAMIN";
 DATAMAX : constant BS_8.Bounded_String := 1 * "DATAMAX";
 -- run-time error raised if the literal string longer




   Descriptive_Keys : constant Optional.Bounded_String_8_Arr :=
        (1* "DATE", 1* "REFERENC", 1* "ORIGIN", 1* "EXTEND", 1* "BLOCKED");

   Observation_Keys : constant Optional.Bounded_String_8_Arr :=
        (1* "DATE-OBS", 1* "DATExxxx", 
         1* "TELESCOP", 1* "INSTRUME", 1* "OBSERVER", 1* "OBJECT");
        -- FIXME DATExxxx needs special handling!?

   -- NOTE bounded string operator * repeats the string (and performs the conversions??)
   -- so it can be (mis)used to initialize bounded_string with string like this:
   use Bounded_String_8;
   Biblio_Keys : constant Optional.Bounded_String_8_Arr :=  
                (1 * "AUTHOR", 1 * "REFERENC");

   Commentary_Keys : constant Optional.Bounded_String_8_Arr :=  
                (1 * "COMMENT", 1 * "HISTORY", 8 * " ");

   Array_Keys : constant Optional.Bounded_String_8_Arr :=  
                (BSCALE, BZERO, BUNIT, BLANK,
                 DATAMAX, DATAMIN);

-- WCS keys : ... see Section 8
   WCS_Keys : constant Optional.Bounded_String_8_Arr :=
		(1*"WCSAXES",
	   	 1*"CTYPE", 1*"CUNIT",1*"CRVAL",1*"CDELT",1*"CRPIX", --<-- FIXME nccc3 !
		 1*"CROTA", 1*"PC", 1*"CD", 1*"PV", 1*"PS",
		 1*"WCSNAME",1*"CNAME",1*"CRDER",1*"CSYER",
		 1*"LONPOLE", 1*"LATPOLE",1*"EQUINOX",1*"EPOCH",
		 1 * "MJD-OBS", 1 * "MJD-AVG", 1 * "DATE-OBS", 1 * "DATE-AVG",
		 1 * "RADESYS", 1 * "RADECSYS", 1 * "RESTFRQ", 1 * "RESTFREQ",
		 1 * "RESTWAV", 1 * "SPECSYS", 1 * "SSYSOBS", 1 * "SSYSSRC",
		 1 * "OBSGEO-X",1 * "OBSGEO-Y", 1 * "OBSGEO-Z", 1 * "VELOSYS",
		 1 * "ZSOURCE", 1 * "VELANGL");

   Extension_Keys : constant Optional.Bounded_String_8_Arr :=
        (1 * "EXTNAME", 1 * "EXTVER", 1 * "EXTLEVEL");

   Reserved_Keys : constant Optional.Bounded_String_8_Arr :=
        (Descriptive_Keys & Observation_Keys & Biblio_Keys & Array_Keys & WCS_Keys);


 type Reserved_Key_Arr is array(SIO.Positive_Count range <>) of Valued_Key_Record;

 function Parse_Reserved_Keys (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Reserved_Key_Arr;

 for Reserved_Key_Arr'Input use Parse_Reserved_Keys;


end Optional.Reserved;

