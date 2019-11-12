with Ada.Text_IO;


separate(FA_Primary)
	procedure DBG_Print 
	is
	package TIO renames Ada.Text_IO;
	begin
	TIO.New_Line;
	if(State.SIMPLE.Read) then TIO.Put_Line("SIMPLE  "&State.SIMPLE.Value); end if;
	if(State.BITPIX.Read) then TIO.Put_Line("BITPITX "&State.BITPIX.Value); end if;
	if(State.NAXIS.Read)  then TIO.Put_Line("NAXIS   "&State.NAXIS.Value);  end if;
	for I in State.NAXISn'Range
	loop
	if(State.NAXISn(I).Read) then
	TIO.Put(Integer'Image(I) &":"& State.NAXISn(I).Value);
	end if;
	end loop;
	TIO.New_Line;
	if(State.PCOUNT.Read) then TIO.Put_Line("PCOUNT  "&State.PCOUNT.Value); end if;
	if(State.GCOUNT.Read) then TIO.Put_Line("GCOUNT  "&State.GCOUNT.Value); end if;
	if(State.GROUPS.Read) then TIO.Put_Line("GROUPS  "&State.GROUPS.Value); end if;
	TIO.Put(Boolean'Image(State.ENDCardSet) & " END ");
	TIO.Put_Line(Positive'Image(State.ENDCardPos));
	TIO.Put_Line(State_Name'Image(State.Name));
	end DBG_Print;



