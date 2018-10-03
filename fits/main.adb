
with Build_Date;

with Options; 		use Options;
with Commands; 		use Commands;
with Ada.Exceptions;    use Ada.Exceptions;

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;

-- for debug:
--with GNAT.Traceback;          use GNAT.Traceback;
--with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;


procedure main is

 Version : String := "fits 0.3.0 Build: " & Build_Date.BuildDate ;

 type FitsOptions is (v,h,hdu,plane);
 type FitsOption_Array is array (FitsOptions) of Option_Record;
 procedure Parse_FitsOptions is new Parse_Options
                                      (All_Options  => FitsOptions,
                                       Option_Array => FitsOption_Array);

 Known_Options : FitsOption_Array := (
  --      HasValue    Token       Description         State/Value
   v   => (False, tUS("-v"),    tUS("print version"),    False    ),
   h   => (False, tUS("-h"),    tUS("print help"),       False    ),
   hdu => (True,  tUS("--hdu"), tUS("HDU number 1.. (Default 1 = Primary Header)"), tUS("1") ),
   plane => (True,  tUS("--plane"), tUS("Plane number if more than 2D data (Default 1)"), tUS("1") )
 );

 Commands: array (Positive range <>) of Option_Record := (
  -- HasValue   Token           Description                               State
   (False, tUS("limits"),  tUS("list implementation/system limitations"), False ),
   (False, tUS("list"),    tUS("list HDU's in FITS-file"),                False ),
   (False, tUS("header"),  tUS("show header"),                            False ),
   (False, tUS("headerwrite"),  tUS("write header"),                      False ),
   (False, tUS("png"),       tUS("convert fits file to png image"),       False ),
   (False, tUS("removekey"), tUS("remove all cards starting with given string"), False ),
   (False, tUS("cleanhead"), tUS("guarantee header starts with cards as defined by FITS standard"), False )
 );

 procedure Print_Usage(WithVersion : Boolean) is
 begin
   Put_Line("Usage:");
   New_Line;
   Put_Line(" fits [-h -v] <cmd> [options] [params]");
   New_Line;
   Put_Line("Commands:");
    for ix in Commands'Range
    loop
      Put_Line(  "  "
               & To_String(Commands(ix).Token)
               & "    "
               & To_String(Commands(ix).Description));
    end loop;
   New_Line;
   Put_Line("Options:");
    for opt in Known_Options'Range
    loop
      Put_Line(  "  "
               & To_String(Known_Options(opt).Token)
               & "    "
               & To_String(Known_Options(opt).Description));
    end loop;
   New_Line;
   if WithVersion = True then
    Put_Line("Version: " & Version);
    New_Line;
   end if;
 end Print_Usage;


 package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);
 use SB;

 Cur_Argument     : SB.Bounded_String;
 Header_File_Path : SB.Bounded_String;
 Input_File_Path  : SB.Bounded_String;
 Output_File_Path : SB.Bounded_String;

 NoOptions : Natural := 0;
 HDUNum   : Positive := 1;

 KeyToRemove : SB.Bounded_String;

 package IO  renames Ada.Text_IO;
 package CLI renames Ada.Command_Line;

 Next      : Positive := 1;
 Cmd_Given : Boolean := False;
 Command   : Unbounded_String := Null_Unbounded_String;
 Param_Cnt : Natural;


 begin

  Parse_FitsOptions(Next,Known_Options);

--  Put_Line("DBG> Next / ArgCnt "
--           & Positive'Image(Next)
--           & Positive'Image(Argument_Count));

  Cmd_Given := not (Argument_Count < Next);

  if not Cmd_Given then
   Print_Usage(Known_Options(v).State);
   return;
  end if;

  Command := tUS(Argument(Next));
  Next    := Next + 1;

  Parse_FitsOptions(Next,Known_Options);

  Param_Cnt := Argument_Count - Next + 1;

--  Put_Line("DBG> Param_Cnt: " & Natural'Image(Param_Cnt));

 --
 -- run the command with Next Param_Cnt and Opts
 --

 if Command = Null_Unbounded_String then

   Print_Usage(Known_Options(v).State);

 elsif Command = "help" then

   Print_Usage(Known_Options(v).State);

 elsif Command = "limits" then

   Limits;

 elsif Command = "list" then

   while Next <= Argument_Count loop
    List_HDUs_In_File (Argument(Next));
    Next := Next + 1;
   end loop;

 elsif Command = "header" then

   Input_File_Path := SB.To_Bounded_String(Argument(Next));
   HDUNum          := Positive'Value(To_String(Known_Options(hdu).Value));
   Print_Header( To_String(Input_File_Path), HDUNum );

 elsif Command = "headerwrite" then

   Put_Line("Command not implemented.");

 elsif Command = "png" then

   HDUNum := Positive'Value(To_String(Known_Options(hdu).Value));

   while Next <= Argument_Count loop
    declare
      FitsFileName : String := Argument(Next);
      PlaneNum     : Positive := Positive'Value(
                                 To_String(Known_Options(plane).Value));
      PngFileName  : String := FitsFileName & "_P" &
                     To_String(Known_Options(plane).Value) & ".png";
    begin
      Ada.Text_IO.Put_Line(FitsFileName);
      FITS_To_PNG(FitsFileName, PngFileName, HDUNum, PlaneNum);
     -- PNGf.FITS_To_PNG(FitsFileName, PngFileName, HDUNum, PlaneNum);
      Next := Next + 1;
      Ada.Text_IO.New_Line;
    end;
   end loop;

 elsif Command = "cleanhead" then

   HDUNum := Positive'Value(To_String(Known_Options(hdu).Value));
   if HDUNum = 1 then
     Input_File_Path  := SB.To_Bounded_String(Argument(Next));
     Output_File_Path := SB.To_Bounded_String(Argument(Next+1));
     Copy_File_And_Modify_HDU(To_String(Input_File_Path),
                              To_String(Output_File_Path),
                              cleanhead,
                              "dummy",
                              HDUNum);
   else
     Put_Line("Implemented only for Primary HDU. Exit.");
   end if;


 elsif Command = "removekey" then

   HDUNum := Positive'Value(To_String(Known_Options(hdu).Value));
   KeyToRemove      := SB.To_Bounded_String(Argument(Next));
   Input_File_Path  := SB.To_Bounded_String(Argument(Next+1));
   Output_File_Path := SB.To_Bounded_String(Argument(Next+2));
   Copy_File_And_Modify_HDU(To_String(Input_File_Path),
                            To_String(Output_File_Path),
                            removekey,
                            To_String(KeyToRemove),
                            HDUNum);

 else

   Put_Line("Unknwon command");

 end if;


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
--     Put_Line( "Name> " & Exception_Name( Except_ID ) );
--     Put_Line( "Mesg> " & Exception_Message( Except_ID ) );
     -- Ada RefMan: Mesg should be short (one line)
     Put_Line( "Info: ");
     Put_Line( Exception_Information( Except_ID ) );
     -- Ada RefMan: Info can be long
  when Except_ID : others =>
     declare
      Error : File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Program error, send a bug-report:");
      Put_Line(Error, "provide program's arguments and copy the following information: ");
--      New_Line(Error);
--      Put_Line(Error, "Exception_Name: " & Exception_Name( Except_ID ) );
--      Put_Line(Error, "Exception_Message: " & Exception_Message( Except_ID ) );
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
--      Put_Line(" > Trace-back of call stack: " );
--      Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do teh same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end main;

