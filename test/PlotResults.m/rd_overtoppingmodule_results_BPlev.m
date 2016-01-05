function [BPlev,z2L,qo] = rd_overtoppingmodule_results_BPlev(dirnm,filnm)

fidin = fopen([dirnm filnm],'r');
iL  = 0;
next = true;
while next
    linestring = fgetl(fidin);
    if ~ischar(linestring)
        next = false;
    elseif length(linestring) == 32
        linearray = str2num(linestring);
        iL = iL + 1;
        BPlev(iL) = linearray(1);
        z2L(iL)   = linearray(2);
        qo(iL)    = linearray(3);
    end
end
fclose(fidin);
