with Ada.Text_IO;

separate(Strict)
	procedure DBG_Print
	is
	package TIO renames Ada.Text_IO;
	begin
	TIO.New_Line;

        if(State.SIMPLE.Read)   then TIO.Put_Line("SIMPLE  " & State.SIMPLE.Value);   end if;
	if(State.XTENSION.Read) then TIO.Put_Line("XTENSION "& State.XTENSION.Value); end if;

	if(State.BITPIX.Read)   then TIO.Put_Line("BITPITX  "& State.BITPIX.Value);   end if;
	if(State.NAXIS.Read)    then TIO.Put_Line("NAXIS    "& State.NAXIS.Value);    end if;

	TIO.Put("NAXIS: ");
	for I in State.NAXISn'Range
	loop
	if(State.NAXISn(I).Read)
	then 
		TIO.Put(Positive'Image(I) &":"& State.NAXISn(I).Value & " ");
	end if;
	end loop;
	TIO.New_Line;

        if(State.GROUPS.Read) then TIO.Put_Line("GROUPS  "&State.GROUPS.Value); end if;
	if(State.PCOUNT.Read) then TIO.Put_Line("PCOUNT  "&State.PCOUNT.Value); end if;
	if(State.GCOUNT.Read) then TIO.Put_Line("GCOUNT  "&State.GCOUNT.Value); end if;

	if(State.TFIELDS.Read)
	then 
		TIO.Put_Line("TFIELDS "&State.TFIELDS.Value);

		TIO.Put("TFORM: ");
		for I in State.TFORMn'Range
		loop
		if(State.TFORMn(I).Read) then 
			TIO.Put(Positive'Image(I) &":"& State.TFORMn(I).Value & " ");
		end if;
		end loop;
		TIO.New_Line;

		TIO.Put("TBCOL: ");
		for I in State.TBCOLn'Range
		loop
		if(State.TBCOLn(I).Read) then 
			TIO.Put(Positive'Image(I) &":"& State.TBCOLn(I).Value & " ");
		end if;
		end loop;
		TIO.New_Line;
 	end if;

	TIO.Put(Boolean'Image(State.ENDCardSet) & " END ");
        TIO.Put_Line(Positive'Image(State.ENDCardPos));
   
	TIO.Put_Line("State_Name: " & State_Name'Image(State.Name));
	TIO.Put_Line("State.XTENSION_Val : " & XT_Type'Image(State.XTENSION_Val));
	TIO.Put_Line("State.NAXIS_Val    : " & Integer'Image(State.NAXIS_Val));
	TIO.Put_Line("State.TFIELDS_Val  : " & Integer'Image(State.TFIELDS_Val));





	end DBG_Print;
