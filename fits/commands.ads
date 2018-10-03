
with FITS;

package Commands is

 procedure List_HDUs_In_File (FitsFileName : in String);

 procedure Limits;
  -- list values limited by implementation and/or system

 procedure Print_Header( FileName : in String;
                         HDUNum   : in Positive := 1 );

 type HDUCmd_Type is
       (cleanhead, removekey);

 procedure Copy_File_And_Modify_HDU(InFitsName  : in String;
                                    OutFitsName : in String;
                                    Command     : in HDUCmd_Type;
                                    InKey       : in String; -- FIXME use variant record when params for more commands needed
                                    HDUNum      : in Positive := 1);

 procedure FITS_To_PNG (FitsFileName : in String;
                        PngFileName  : in String;
                        HDUNum       : in Positive := 1;
                        PlaneNum     : in Positive := 1);


end Commands;

