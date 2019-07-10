--
-- Example list info on each HDU in FITS-file
--
-- FIXME temp: for testing of FITS_IO (DUSize calc)
--


with Ada.Text_IO;      --use Ada.Text_IO;
with Ada.Command_Line; --use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;  --use Ada.Streams.Stream_IO;

with Ada.Strings.Unbounded;

with FITS_IO.File;   --use FITS_IO.File;
with FITSlib.File;   use FITSlib.File;
with FITSlib.Header;   use FITSlib.Header; -- HDU_Variant needed

procedure list
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 package CLI renames Ada.Command_Line;
 package SU  renames Ada.Strings.Unbounded;
 package FIO renames FITS_IO.File;

 InFileName : SU.Unbounded_String; 
 InFile     : SIO.File_Type;
 HDUNum     : Positive := 1;
 DSize      : Natural := 0;
 DUSize     : Natural := 0;
 DURem      : Natural;
 HStart     : SIO.Positive_Count; 
 use SIO;

    
    procedure Print_DDims_Rec (DDims  : Data_Dimensions_Type)
    is
	    lNAXISn : NAXIS_Arr := DDims.NAXISn(1 .. DDims.NAXIS);
    begin
	    TIO.Put(
		    HDU_Variant'Image(DDims.HDUVar)
		    & " " &
		    Positive'Image(DDims.CardsCount)
		    & " " &
		    Integer'Image(DDims.BITPIX)
		    );
	    
	    TIO.Put(" ( ");
	    TIO.Put(Positive'Image(lNAXISn(1)));
	    for I in 2 .. lNAXISn'Last
		    loop
			    TIO.Put(" x " & Positive'Image(lNAXISn(I)));
		    end loop;
	    TIO.Put_Line(" )");

    end Print_DDims_Rec;



    procedure Read_Print_Data_Dimensions (Source : SIO.File_Type;
	    Var : HDU_Variant)
    is
	     DDims  : Data_Dimensions_Type;
    begin

                case Var is
                        when UNKNOWN =>
                                null;
                                -- raise exception if Index() is 1 
                                -- (if >1 it will be unspecified extension)
                                -- FIXME better solution here ? if not a fitsfile 
                                -- we should not even call this function

                        when PRIM_UNKNOWN =>
                                -- raise exception and exit
                                null;

                        when PRIM_NON_STANDARD =>
                                -- raise exception and exit
                                null;

                        when PRIM_NO_DATA =>
                                -- FIXME what to do ?? read until end of header?
                                -- or leave FileIndex after 1st block?
                                DDims.NAXIS := 0;

                        when PRIM_IMAGE =>
                                Read_Data_Dimensions (Source, DDims);
				Print_DDims_Rec(DDims);

                        when RAND_GROUPS =>
                                Read_Data_Dimensions (Source, DDims);
				Print_DDims_Rec(DDims);

                        when EXT_IMAGE .. EXT_BINTABLE =>
                                Read_Data_Dimensions (Source, DDims);
				Print_DDims_Rec(DDims);

                        when EXT_UNKNOWN =>
                                -- raise exception and exit
                                null;
                end case;

    end Read_Print_Data_Dimensions;











 Exp : Exp_Type;

-- main begins ----------------------------------------------
begin

 if (CLI.Argument_Count /= 1) then
   TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name>");
   return;
 else
   InFileName := SU.To_Unbounded_String (CLI.Argument (1));
 end if;


   SIO.Open(InFile, SIO.In_File, SU.To_String(InFileName));
   
   TIO.Put_Line("New: ---------------");

   FITSlib.File.Read_Exp(InFile,Exp);


   TIO.Put_Line("______________________________________");
   TIO.Put_Line("TEST1: FITSlib.File.Read_DataSize_bits");
   
   FIO.Set_Index(InFile, 1);

   while not SIO.End_Of_File(InFile)
   loop

    -- Peek() resets origi FileIndex: no need for this: FIO.Set_Index(InFile, 1);
    -- size
    -- type
    TIO.Put_Line("HDU: " &
      HDU_Variant'Image( FITSlib.File.Peek(InFile)  )
    );

    DSize := FITSlib.File.Read_DataSize_bits(InFile);
    TIO.Put_Line("Data Size     [bytes] : " & Natural'Image(DSize/8));
    DURem  := (DSize/8) rem 2880;
    --TIO.Put_Line("DURem : " & Natural'Image(DURem));
    DUSize := ((DSize/8) / 2880 + 1) * 2880;
    if(DURem = 0) then
	   DUSize := DUSize - 2880;
    end if;
    TIO.Put_Line("DataUnit Size [bytes] : " & Natural'Image(DUSize));


    SIO.Set_Index(InFile, SIO.Index(InFile) + SIO.Positive_Count(DUSize) );

 end loop;

 


   TIO.Put_Line("______________________________________");
   TIO.Put_Line("TEST2: FITSlib.File.Read data dimensions");

   FIO.Set_Index(InFile, 1);
	 
   while not SIO.End_Of_File(InFile)
   loop

	   HStart := Index(InFile);

    Read_Print_Data_Dimensions( InFile, FITSlib.File.Peek(InFile)  );

	SIO.Set_Index(InFile, HStart);

    DSize  := FITSlib.File.Read_DataSize_bits(InFile);
    DUSize := ((DSize/8) / 2880 + 1) * 2880;

    SIO.Set_Index(InFile, SIO.Index(InFile) + SIO.Positive_Count(DUSize) );

  end loop;




   
   TIO.Put_Line("");
   TIO.Put_Line("________________________");
   TIO.Put_Line("TEST3: FITS_IO.Set_Index");

   FIO.Set_Index(InFile, 1);

   while not SIO.End_Of_File(InFile)
   loop
     declare
       -- FIXME tbd later: HDUInfo : HDU_Info := FITS_IO.File.List.Get(InFile);
     begin
       TIO.Put_Line(Positive'Image(HDUNum));
       HDUNum := HDUNum + 1;
       FIO.Set_Index(InFile, HDUNum);
     end;
   end loop;

   SIO.Close(InFile);



 exception

  when Except_ID : others =>
     declare
      Error : TIO.File_Type := TIO.Standard_Error;
     begin
      TIO.Put_Line(Error, Exception_Message( Except_ID ) );
      TIO.New_Line(Error);
      TIO.Put_Line(Error, Exception_Information( Except_ID ) );
      TIO.Put_Line(Error, "Call stack traceback symbols: addr2line -e ./list -a addr1 addr2 ...");

      -- TIO.Put_Line(Error, " > Trace-back of call stack: " );
      -- TIO.Put_Line(Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do the same manually, use:
      -- addr2line -e ./fits -a addr1 addr2 ...
     end;
end list;

   
   


