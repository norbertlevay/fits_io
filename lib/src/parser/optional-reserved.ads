

with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!

package Optional.Reserved is


   function BS8(S : String; Drop :Ada.Strings.Truncation := Ada.Strings.Error) 
        return Bounded_String_8.Bounded_String
                renames Bounded_String_8.To_Bounded_String;

   Descriptive_Keys : constant Optional.Bounded_String_8_Arr :=
        (BS8("DATE"),BS8("REFERENC"),BS8("ORIGIN"),BS8("EXTEND"),BS8("BLOCKED"));

   Observation_Keys : constant Optional.Bounded_String_8_Arr :=
        (BS8("DATE-OBS"),BS8("DATExxxx"),
         BS8("TELESCOP"),BS8("INSTRUME"),BS8("OBSERVER"),BS8("OBJECT"));
        -- FIXME DATExxxx needs special handling!?

   -- NOTE bounded string operator * repeats the string (and performs the conversions??)
   -- so it can be (mis)used to initialize bounded_string with string like this:
   use Bounded_String_8;
   Biblio_Keys : constant Optional.Bounded_String_8_Arr :=  
                (1 * "AUTHOR", 1 * "REFERENC");

   Commentary_Keys : constant Optional.Bounded_String_8_Arr :=  
                (1 * "COMMENT", 1 * "HISTORY", 8 * " ");

   Array_Keys : constant Optional.Bounded_String_8_Arr :=  
                (1 * "BSCALE", 1 * "BZERO", 1 * "BUNIT", 1 * "BLANK",
                 1 * "DATAMAX", 1 * "DATAMIN");

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





end Optional.Reserved;

