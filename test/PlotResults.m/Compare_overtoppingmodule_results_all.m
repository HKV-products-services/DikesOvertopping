% compare testresults overtopping module
%
clear all

%=== input begin ==========================================================
dirref = 'd:\3-Test\DikesOvertopping_23_1_1\Windows32bit\1-trendtests\2-results\';
dirnew = 'd:\3-Test\DikesOvertopping_23_1_1\Windows64bit\1-trendtests\2-results\';
dirou  = 'd:\3-Test\DikesOvertopping_23_1_1\';
filnmou= 'comparison_32_64-bit.txt';
%=== input end ============================================================

% prepare output file
fidou = fopen([dirou filnmou],'w');
fprintf(fidou,'%s\n'  ,'Comparison between the output*.txt files in directory');
fprintf(fidou,'%s\n'  ,['    ' dirref]);
fprintf(fidou,'%s\n'  ,'and the corresponding files in directory');
fprintf(fidou,'%s\n\n',['    ' dirnew]);

% generate a list of output file names
dirtab = dir([dirref 'output*.txt']);
nfil = size(dirtab,1);
nfildif = 0;

for ifil=1:nfil % do for each output file
    % open the ref file and skip the first 3 lines
    filnm = dirtab(ifil).name;
    fidinref = fopen([dirref filnm],'r');
    fgetl(fidinref);
    fgetl(fidinref);
    fgetl(fidinref);
    
    % open the new file and skip the first 3 lines
    fidinnew = fopen([dirnew filnm],'r');
    fgetl(fidinnew);
    fgetl(fidinnew);
    fgetl(fidinnew);

    founddiffinfile = 0;
    iLine  = 0;
    next = true;
    while next
        % read next line in both files to strings
        linestringref = fgetl(fidinref);
        linestringnew = fgetl(fidinnew);
        if ~ischar(linestringref)
            % end of file
            next = false;
        else
            iLine = iLine + 1;
            stringlen = length(linestringref);
            if length(linestringnew) >= stringlen
                % compare only the part of the string that is present in the ref file
                same = strcmp(linestringref,linestringnew(1:stringlen));
            else
                % the string in the new file is shorter i.e. different
                same = 0;
            end
            if ~same
                % display lines where differences were found
                fprintf(fidou,'%s\n',['=> Difference in file: ' filnm '; Line: ' num2str(iLine)]);
                fprintf(fidou,'%s\n',linestringref);
                fprintf(fidou,'%s\n',linestringnew);
                founddiffinfile = 1;
            end
        end
    end
    if founddiffinfile
        nfildif = nfildif + 1;
    else
        fprintf(fidou,'%s\n',['=> No difference in file: ' filnm '; number of compared lines: ' num2str(iLine)]);
    end
    fclose(fidinref);
    fclose(fidinnew);
end

% finalize output file
if nfildif==0
    fprintf(fidou,'\n%s\n','No differences were found.');
else
    fprintf(fidou,'\n%s %i %s\n','Differences were found in',nfildif,'files.');
end
fclose(fidou);
