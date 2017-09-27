
with FITS_SIO;

package Commands is

 procedure List_HDUs_In_File (FitsFileName : in String);

 procedure Limits;
  -- list values limited by implementation and/or system

 procedure Print_Header( FileName : in String;
                         HDUNum   : in Positive := 1 );


end Commands;

