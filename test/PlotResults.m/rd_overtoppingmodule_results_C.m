function [cota,z2L,qo] = rd_overtoppingmodule_results_C(dirnm,filnm)

fidin = fopen([dirnm filnm],'r');
iL  = 0;
next = true;
while next
    linestring = fgetl(fidin);
    if ~ischar(linestring)
        next = false;
    elseif length(linestring) == 32 || length(linestring) == 34
        linearray = str2num(linestring(7:32));
        iL = iL + 1;
        cota(iL) = linearray(1);
        z2L(iL)  = linearray(2);
        qo(iL)   = linearray(3);
    end
end
fclose(fidin);
