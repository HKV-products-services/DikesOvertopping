function [BPlev,z2L,qo] = rd_overtoppingmodule_results_BPlev(dirnm,filnm)

fidin = fopen([dirnm filnm],'r');
fgetl(fidin);
fgetl(fidin);
fgetl(fidin);

iL  = 0;
next = true;
while next
    linestring = fgetl(fidin);
    if ~ischar(linestring)
        next = false;
    elseif length(linestring) == 56
        linearray = str2num(linestring);
        iL = iL + 1;
        BPlev(iL) = linearray(1);
        z2L(iL)   = linearray(2);
        qo(iL)    = linearray(3);
        HBN_4(iL) = linearray(4);
        HBN_3(iL) = linearray(5);
        HBN_2(iL) = linearray(6);
    end
end
fclose(fidin);
