function [h,Hs,So,Bt,Tm,z2L,qo,HBN_4,HBN_3,HBN_2] = rd_overtoppingmodule_results_L(dirnm,filnm)

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
    elseif length(linestring) == 87
        linearray = str2num(linestring);
        iL = iL + 1;
        h(iL)   = linearray(1);
        Hs(iL)  = linearray(2);
        So(iL)  = linearray(3);
        Bt(iL)  = linearray(4);
        Tm(iL)  = linearray(5);
        z2L(iL) = linearray(6);
        qo(iL)  = linearray(7);
        HBN_4(iL) = linearray(8);
        HBN_3(iL) = linearray(9);
        HBN_2(iL) = linearray(10);
    end
end
fclose(fidin);
