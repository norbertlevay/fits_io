with Ada.Text_IO;

separate(FA_Extension)
	procedure DBG_Print
	is

	package TIO renames Ada.Text_IO;

	procedure DBG_Print_Mandatory
	is
	begin
	TIO.New_Line;
	if(State.XTENSION.Read) then TIO.Put_Line("XTENSION "&State.XTENSION.Value); end if;
	if(State.BITPIX.Read)   then TIO.Put_Line("BITPITX  "&State.BITPIX.Value);   end if;
	if(State.NAXIS.Read)    then TIO.Put_Line("NAXIS    "&State.NAXIS.Value);    end if;
	TIO.Put("NAXIS: ");
	for I in State.NAXISn'Range
	loop
	if(State.NAXISn(I).Read) then Put(Positive'Image(I) &":"& State.NAXISn(I).Value & " "); end if;
	end loop;
	New_Line;
	if(State.PCOUNT.Read)  then TIO.Put_Line("PCOUNT  "&State.PCOUNT.Value);  end if;
	if(State.GCOUNT.Read)  then TIO.Put_Line("GCOUNT  "&State.GCOUNT.Value);  end if;
	if(State.TFIELDS.Read) then TIO.Put_Line("TFIELDS "&State.TFIELDS.Value); end if;
	TIO.Put("TFORM: ");
	for I in State.TFORMn'Range
	loop
	if(State.TFORMn(I).Read) then Put(Positive'Image(I) &":"& State.TFORMn(I).Value & " "); end if;end loop;
	New_Line;
	TIO.Put("TBCOL: ");
	for I in State.TBCOLn'Range
	loop
	if(State.TBCOLn(I).Read) then Put(Positive'Image(I) &":"& State.TBCOLn(I).Value & " "); end if;
	end loop;
	New_Line;
	Reserved.DBG_Print(State.Res);
	TIO.Put_Line(State_Name'Image(State.Name));
	end DBG_Print_Mandatory;







	procedure DBG_Print_Reserved
	is
	Res : Key_Rec_Arr := Get((EXTNAME,DATAMAX,DATAMIN,INSTRUME,TELESCOP));
	begin
	TIO.Put_Line("DBG_Print_reserved");
	for I in Res'Range
	loop
	TIO.Put("Get Res> "&Reserved_Key'Image(Res(I).Key));
	TIO.Put(" : "&Res(I).Value);
	TIO.New_Line;
	end loop;


	end DBG_Print_Reserved;


procedure DBG_Print(IdxKeys : IdxKey_Rec_Arr)
	is
	begin
	TIO.Put_Line("DBG_Print IdxKeys");
	for I in IdxKeys'Range
	loop
	TIO.Put("Get ResArr "& Reserved_Root'Image(IdxKeys(I).Root)&" ");
	for Idx in IdxKeys(I).Arr'Range
	loop
	TIO.Put(Integer'Image(Idx) &":"& IdxKeys(I).Arr(Idx).Value);
	end loop;
	TIO.New_Line;
	end loop;
	end DBG_Print;







	begin
	DBG_Print_Mandatory;
	DBG_Print_Reserved;
	DBG_Print(Get((TSCAL,TTYPE,TUNIT,TDISP)));
	end DBG_Print;
