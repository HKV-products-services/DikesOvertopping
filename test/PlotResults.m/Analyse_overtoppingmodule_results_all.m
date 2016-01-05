% plot testresults overtopping module
clear all

[iS,wave_angle,iType,varlabel] = rd_overtoppingmodule_results_layout;

[nT,nC] = size(iS);

iC = 1;
iL = 2;
chapternr    = 1;
reportlayout = 1;
figure2pdf   = 1;

pause_on     = 0;

Fig_pdf_dir  = 'd:\Overtopping_tests\';
Projectstr   = 'Wave overtopping kernel tests';
ProjectNrstr = '1220043.002';
dirnm = 'd:\svn_checkouts\DikesOvertopping\trunk\src\core\tests\DikesOvertoppingTests\OutputRTOovertopping\';
qomin = 10^-10;

for iC=1:8
    chapternr = iC;
    figurenr  = 0;
    for iT = 1:nT
        iL = iS(iT,iC);
        if iL > 0
            if iL < 100
                filnm = ['output_section' num2str(iC,'%1i') '_test' num2str(iL,'%02i') '.txt'];
            else
                iL  = iL - 100;
                filnm = ['output_section' num2str(iC,'%1i') '_test' num2str(iL,'%02i') 'berm.txt'];
            end
            
            figurenr = figurenr + 1;
            
            switch iType(iT)
                case {1 2 3 4}
                    [h,Hs,So,Bt,Tm,z2L,qo] = rd_overtoppingmodule_results_L(dirnm,filnm);
                case {5 6}
                    [cota,z2L,qo] = rd_overtoppingmodule_results_C(dirnm,filnm);
                    tana = 1./cota;
                    h = 3.0;
                case {7}
                    [BPlev,z2L,qo]  = rd_overtoppingmodule_results_B(dirnm,filnm);
                    h = 3.0;
                case {8}
                    [Bwidth,z2L,qo] = rd_overtoppingmodule_results_B(dirnm,filnm);
                    h = 3.0;
                case {9}
                    [RF,z2L,qo]     = rd_overtoppingmodule_results_B(dirnm,filnm);
                    h = 3.0;
            end
            
            switch iType(iT)
                case 1
                    var    = h;
                    Xtick  = -2:1:7;
                    xasstr = 'Water level (m+NAP)';
                case 2
                    var    = Hs;
                    Xtick  = 0:0.5:5;
                    xasstr = 'Wave height (m)';
                case 3
                    var    = So;
                    Xtick  = 0.00:0.01:0.07;
                    xasstr = 'Wave steepness (-)';
                case 4
                    var    = Bt;
                    Xtick  = 0:10:180;
                case 5
                    var    = tana;
                    Xtick  = 0:0.01:0.07;
                    xasstr = 'Slope tan(\alpha) (-)';
                case 6
                    var    = cota;
                    Xtick  = 1:1:8;
                    xasstr = 'Slope cot(\alpha) (-)';
                case 7
                    var    = BPlev;
                    Xtick  = []; %-2:1:4;
                    xasstr = 'Buckling point level (m+NAP)';
                case 8
                    var    = Bwidth;
                    Xtick  = 2:1:20;
                    xasstr = 'Berm width (m)';
                case 9
                    var    = RF;
                    Xtick  = 0.5:0.1:1.0;
                    xasstr = 'Roughness factor (-)';
            end
            %}
            if wave_angle(iT) == 0
                WaveAnglestr = 'Wave angle: 0 (\circ)';
            elseif wave_angle(iT) == 85
                WaveAnglestr = 'Wave angle: 85 (\circ)';
            else
                WaveAnglestr = ' ';
            end
            
            ReportTitle1 = ['Cross section nr ' num2str(iC,'%1i') '; series nr ' num2str(iL,'%2i') '; ' WaveAnglestr];
            ReportTitle2 = ['Varying ' char(varlabel(iT))];
            
            if reportlayout
                fontsize  = 12;
                linewidth =  1;
            else
                fontsize  = 18;
                linewidth =  2;
            end
            
            legpos   = 'SouthEast';
            
            % Define subplot 1 --------------------------------------------------------
            subplot(2,1,1);
            h = plot(var,z2L,'k.-');
            set(h,'LineWidth',linewidth);
            
            % Add title and x,y labels
            %title('<title 1>');
            xlabel(xasstr);
            ylabel('2% run-up level (m+NAP)');
            Ytick = [-2:2:14];
            Ytick = [];
            
            % Apply tick settings
            if ~isempty(Xtick)
                Xlim  = [min(Xtick) max(Xtick)];
                set(gca,'Xlim',Xlim,'Xtick',Xtick);
            end
            if ~isempty(Ytick)
                Ylim  = [min(Ytick) max(Ytick)];
                set(gca,'Ylim',Ylim,'Ytick',Ytick);
            end
            Ytick = [];
            
            grid on
            
            % Define subplot 2 --------------------------------------------------------
            subplot(2,1,2);
            k = find(qo==0.0);
            if ~isempty(k)
                h = semilogy(var,qo,'k.-',var(k),qomin,'r.-');
            else
                h = semilogy(var,qo,'k.-');
            end
            clear k
            set(h,'LineWidth',linewidth);
            
            % Add title and x,y labels
            %title('<title 2>');
            xlabel(xasstr);
            ylabel('Wave overtopping discharge (m^3/ms)');
            
            %Ytick = [10^-10:10^1];
            % Apply tick settings
            if ~isempty(Xtick)
                Xlim  = [min(Xtick) max(Xtick)];
                set(gca,'Xlim',Xlim,'Xtick',Xtick);
            end
            if ~isempty(Ytick)
                Ylim  = [min(Ytick) max(Ytick)];
                set(gca,'Ylim',Ylim,'Ytick',Ytick);
            end
            set(gca,'Ylim',[10^-10 10^2]);

            grid on
            
            % Apply overall settings for layout and printing --------------------------
            if reportlayout
                md_paper_deltares('a4p','deltares',{centre_title(ReportTitle1,ReportTitle2), ...
                    '',' ',Projectstr,ProjectNrstr,['Fig. ' num2str(chapternr) '.' num2str(figurenr)]})
                if figure2pdf
                    print(gcf,'-dpdf','-painters', ...
                        strcat([Fig_pdf_dir 'Fig' num2str(chapternr) '_' num2str(figurenr,'%02i') '.pdf']));
                end
            end
            
            if pause_on
                pause
            end
        end
    end
end
