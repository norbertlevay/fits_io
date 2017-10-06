
with FITS;

package Commands is

 procedure List_HDUs_In_File (FitsFileName : in String);

 procedure Limits;
  -- list values limited by implementation and/or system

 procedure Print_Header( FileName : in String;
                         HDUNum   : in Positive := 1 );

 procedure Copy_File_And_Modify_HDU(InFitsName  : in String;
                                    OutFitsName : in String;
                                    HDUNum      : in Positive := 1);

end Commands;

