with
    Build_Date,
    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Direct_IO,
    Ada.Text_IO.Bounded_IO,
    Ada.Command_Line,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    GNAT.Traceback.Symbolic;


use
    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Command_Line;


procedure main is

 Version : String := "fits(ada) 0.1.0 Build: " & Build_Date.BuildDate ;

 procedure Print_Usage is
 begin
   Put_Line("Version: " & Version);New_Line;
   Put_Line("Usage:");New_Line;
   Put_Line(" fits info             <fitsfilename>                   prints HDU info");
   Put_Line(" fits header [options] <fitsfilename>                   prints header");
   Put_Line(" fits header [options] <fitsfilename> <headerfilename>  writes header into fits-file");
   New_Line;
   Put_Line("Options:");
   Put_Line(" --hdu N     HDU-number: 1,2,3,... Default is 1 = Primary Header.");
   New_Line;
   Put_Line("Notes:");
   Put_Line(" HDU info:        No of Cards, Data Type, ( axes dimensions ).");
   Put_Line(" <headerfilename> is text file with one fits-keyword record per line");
   Put_Line("                  last line must start with three letters: 'END'.");
   New_Line;
 end Print_Usage;

 package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);
 use SB;

 Cur_Argument     : SB.Bounded_String;
 Header_File_Path : SB.Bounded_String;
 Input_File_Path  : SB.Bounded_String;
 Output_File_Path : SB.Bounded_String;

 i : Positive := 1;

 NoOptions : Natural := 0;
 HDU_Num : Positive := 1;

 package IO  renames Ada.Text_IO;
 package CLI renames Ada.Command_Line;

 begin

 if (CLI.Argument_Count <= 1)
 then
  Print_Usage;
  Return;
 end if;

 -- Debug:
 -- IO.Put_Line (Item => "Argument Count:" & CLI.Argument_Count'Img);
 -- For all the given arguments
 -- for i in 1 .. CLI.Argument_Count loop
 --    IO.Put_Line (Item => CLI.Argument (Number => i));
 -- end loop;

 while i < Argument_Count loop

     Cur_Argument := SB.To_Bounded_String(Argument(i));

     -- Put_Line(To_String(Cur_Argument));

     if Cur_Argument = "info" then

        Input_File_Path := SB.To_Bounded_String(Argument(i+1));
        Print_Struct (To_String(Input_File_Path));

     elsif Cur_Argument = "header"
     then

        if Argument(i+1) = "--hdu" then
          HDU_Num := Integer'Value(Argument(i+2));
          NoOptions := 2;-- there were 2 more args
          i := i + NoOptions;
        end if;

        if CLI.Argument_Count = (2 + NoOptions)
        then
        	Input_File_Path := SB.To_Bounded_String(Argument(i+1));
	        i := i + 1;
		-- Print_Header( To_String(Input_File_Path), HDU_Num );
		FITSIO_Print_Header( To_String(Input_File_Path), HDU_Num );
        else
        	Input_File_Path  := SB.To_Bounded_String(Argument(i+1));
	        i := i + 1;
        	Header_File_Path := SB.To_Bounded_String(Argument(i+1));
	        i := i + 1;
                Write_Header(To_String(Input_File_Path),
                             To_String(Header_File_Path), HDU_Num);
        end if;

     else
        Put_Line("Wrong arguments.");
        New_line;
        Print_Usage;
     end if;

     i := i + 1;

 end loop;

-- Exception handling:
--
 -- declare exception:
 -- Queue_Error : exception; <- behaves like "type" (not "variable")
 --
 -- to rasie exception:
 -- raise Queue_Error with "Buffer Full"; <-- "with msg-string" is optional
 -- msg-string can be retrieved by Exception_Meassage( e_id )
 --
 -- When cathing exception instance identifier does not need
 -- to be declared expicitely:
 -- Except_ID : Ada.Exceptions.EXCEPTION_OCCURRENCE;
 -- It is clear from syntax "when <e_instance> : <e_type> => [code]"
 -- Except_ID will be filled in when a particular exception actually happens
 -- and is an index which can be used to retrieve more info about
 -- that particular exception

 exception
 when Except_ID : Name_Error =>
     Put_Line( "Name> " & Exception_Name( Except_ID ) );
     Put_Line( "Mesg> " & Exception_Message( Except_ID ) );
     -- Ada RefMan: Mesg should be short (one line)
     Put_Line( "Info: ");
     Put_Line( Exception_Information( Except_ID ) );
     -- Ada RefMan: Info can be long
  when Except_ID : others =>
     declare
      Error : File_Type := Standard_Error;
     begin
      Put_Line(Error, "Program error, send a bug-report:");
      Put_Line(Error, "provide program's arguments and copy the following information: ");
      New_Line(Error);
      Put_Line(Error, "Exception_Name: " & Exception_Name( Except_ID ) );
      Put_Line(Error, "Exception_Message: " & Exception_Message( Except_ID ) );
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      --Put_Line(" > Trace-back of call stack: " );
      -- Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do teh same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end main;

