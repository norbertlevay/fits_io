
with Ada.Command_Line;

with Ada.Text_IO;
use  Ada.Text_IO;


package body Options is

  package CLI renames Ada.Command_Line;

-- fill in Opts table
-- update Next to first non-option arg
 procedure Parse_Options
           ( Next : in out Positive;
             Opts : in out Option_Array)
 is
 begin

  while Next <= CLI.Argument_Count
  loop

   declare
    Arg : String :=  CLI.Argument(Next);
   begin

    exit when Arg(1) /= '-';

    Put_Line( "DBG: " & Arg );

    for opt in Opts'Range
    loop

     if Arg = Opts(opt).Token
     then

      if Opts(opt).HasValue = False
      then
        Opts(opt).State := True;
        Put_Line("DBG: "
                 & All_Options'Image(opt)
                 & " WithValue: " & Boolean'Image(Opts(opt).HasValue)
                 & " State: " & Boolean'Image(Opts(opt).State)
                 & " > " & To_String(Opts(opt).Description)
                 );
      else
        Next := Next + 1;
        Opts(opt).Value := tUS(CLI.Argument(Next));
        Put_Line("DBG: "
                 & All_Options'Image(opt)
                 & " WithValue: " & Boolean'Image(Opts(opt).HasValue)
                 & " Value: " & To_String(Opts(opt).Value)
                 & " > " & To_String(Opts(opt).Description)
                 );
      end if;
     end if;

    end loop;
   end;-- declare

   Next := Next + 1;
  end loop;

  return;
 end Parse_Options;

end Options;
