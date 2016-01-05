%function [cota,z2L,qo] = rd_overtoppingmodule_results_C(dirnm,filnm)
clear all

dirnm = 'd:\svn_checkouts\DikesOvertopping\trunk\src\core\tests\DikesOvertoppingTests\InputRTOovertopping\';

for iC = 1:8
    
    filnm = ['Cross_section' num2str(iC) '.txt'];
    fidin = fopen([dirnm filnm],'r');
    
    iL  = 0;
    next = true;
    while next
        linestring = fgetl(fidin);
        if ~ischar(linestring)
            next = false;
        elseif linestring(1) ~= '#'
            linearray = str2num(linestring);
            iL = iL + 1;
            x(iL) = linearray(1);
            z(iL) = linearray(2);
            f(iL) = linearray(3);
        end
    end
    % >>> keuze opmaak:
    figformat = 'doc';
    %
    % >>> Initialisatie opmaak:
    [ttxt,xtxt,ytxt,ltxt,lpos,Xtick,Ytick,fontsize,linewidth] = fig_opmaak_a(figformat);
    % linewidth =
    % fontsize  =
    %
    % >>> Definitie plot:
    plot(x,z,'ko-','Linewidth',linewidth)
    %
    % >>>Nadere invulling opmaak:
    ttxt  = ['Cross section nr ' num2str(iC)];
    % ttxt  =
    xtxt  = 'x (m)';
    ytxt  = 'y (m+NAP)';
    % ltxt  =
    % lpos  =
    Xtick = 0:5:50;
    Ytick = -3:1:7;
    %
    % >>>Toepassing opmaak:
    fig_opmaak_b(ttxt,xtxt,ytxt,ltxt,lpos,Xtick,Ytick,fontsize,linewidth)
    grid on
    fclose(fidin);
    pause
end
