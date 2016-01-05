function [iS,wave_angle,iType,varlabel] = rd_overtoppingmodule_results_layout

%clear all

nkol = 14;

fidin1 = fopen('n:\My Documents\_0 SLA WVI\21 Dijken_Overslag\3 Deliveries\03 Test plan\Test plan 01.csv','r');
fgetl(fidin1);
fgetl(fidin1);

iL  = 0;
next = true;
while next
    linestring = fgetl(fidin1);
    if ~ischar(linestring)
        next = false;
    else
        iL = iL + 1;
        k = find(linestring == ';');
        ctxtry(1:nkol)= cellstr('-1');
        if k(1) > 1
            ctxtry(1)= cellstr(linestring(1:k(1)-1));
        end
        for i=2:nkol
            if k(i)-k(i-1) > 1
                ctxtry(i)= cellstr(linestring(k(i-1)+1:k(i)-1));
            end
        end
    end
    for iC=1:8
        string = char(ctxtry(iC)); iS(iL,iC) = str2num(string);
    end
    iC = 9;
    string = char(ctxtry(iC)); wave_angle(iL) = str2num(string);
    iC = 10;
    string = char(ctxtry(iC)); iType(iL) = str2num(string);
    varlabel(iL) = ctxtry(11);
end

fclose(fidin1);

%}