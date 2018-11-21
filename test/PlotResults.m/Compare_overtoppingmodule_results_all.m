% compare testresults overtopping module
%
clear all

dirref = 'd:\svn_checkouts\DikesOvertopping\trunk\src\core\tests\DikesOvertoppingTests\OutputOvertopping\';
dirnew = 'd:\svn_checkouts\DikesOvertopping\trunk\src\core\tests\unitTests\';

% generate a list of output file names
dirtab = dir([dirref 'output*.txt']);
nfil = size(dirtab,1);

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
                disp(['=> Difference in file: ' filnm '; Line: ' num2str(iLine)]);
                disp(linestringref);
                disp(linestringnew);
                founddiffinfile = 1;
            end
        end
    end
    if ~founddiffinfile
        disp(['=> No difference in file: ' filnm '; number of compared lines: ' num2str(iLine)]);
    end
    fclose(fidinref);
    fclose(fidinnew);
end
