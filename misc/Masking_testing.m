%% Load AZFP data
% Make sure you've run the initial processing and differencing code first!
% Otherwise this code won't work!

clc
clear variables
close all

addpath(genpath('/Users/dmossman/Box/2022 MSc Thesis Work/Code/AzfpMatlabToolbox_v18'))

dep_name = input("Enter the full name of the deployment: ","s");
mo = input('Enter the numerical month of the data: ','s');
da = input('Enter the numerical day of the data: ','s');

addpath(genpath(strcat('/Users/dmossman/Box/Glider Data/',dep_name)))
%% Processing with the ASL AZFP toolbox
Parameters.ProcDir = 1;
Parameters.Plot = 0;
Parameters.Value2Plot = 2;
Parameters.Time2Avg = 1;
Parameters.Bins2Avg = 1;
[Output,Par]=ProcessAZFP(Parameters);
%%
I = find(Output(1).Range(1,:) <= 2);

for i = 1:length(Output)
    Output(i).Sv(:,I) = [];
    Output(i).Range(:,I) = [];
end

%%
load(strcat('C:/Users/dmossman/Box/Glider Data/',dep_name,'/ru39-20230817T1520-profile-sci-delayed_aec6_4df7_1713.mat'));
gliderdata = ru39_20230817T1520_profile_sci_;
clear ru39_20230817T1520_profile_sci_

unix_epoch = datenum(1970,1,1,0,0,0);
gliderdata.mtime = gliderdata.time./86400 + unix_epoch;
nanindex = find(~isnan(gliderdata.depth));
gdepth = gliderdata.depth(nanindex);
gtime = gliderdata.mtime(nanindex);

for ii = 1:length(Output(1).Date)
    [~,timeindex(ii)] = min(abs(Output(1).Date(ii) - gtime));
end

Output(1).Depth = Output(1).Range(1,:) + gdepth(timeindex);
Output(2).Depth = Output(1).Depth(:,1:size(Output(2).Sv,2));
Output(3).Depth = Output(1).Depth(:,1:size(Output(3).Sv,2));
%%

StartDive = find([1;diff(Output(1).Depth(:,1))<0]);
% find indices where depth decreases
% these indices will be used as starting points for searching for actual
% dives

cc = 0; % number of distinct dives to go into the structure

for DD = 1:length(StartDive)-1
    if (StartDive(DD+1)-1 - StartDive(DD)) > 50
        % This if statement checks the "length" of each dive and only keeps
        % dives with > 50 entries
        % This avoids short dives and "false" dives where the glider begins
        % to rise again at the end of a dive
        % Depending on the depth of the area you are in, this value may
        % need to be changed in order to capture the true dives
        cc = cc+1;
        Dive(cc).Index = [StartDive(DD);StartDive(DD+1)-1];
    end
end

% get rid of data where the glider is ascending
for k = 1:length(Output())
for j = 1:length(Dive)
    if j == 1
        Output(k).Sv(1:(Dive(j).Index(1)-1),:) = NaN;
    else
        Output(k).Sv((Dive(j-1).Index(2)+1):(Dive(j).Index(1)-1),:) = NaN;
    end
end
end
%%

cr = [120,60,40];
for i = 1:length(Output) % for each frequency
    J = find(Output(i).Range(1,:) >= cr(i)); % find the frequency-specific
    % far field data
    Output(i).Sv(:,J) = []; % eliminate the Sv in the far field
    Output(i).Range(:,J) = []; % eliminate the far field ranges
    % Redo the depth calculation to account for the far field being cut off
    Output(i).Depth = Output(i).Range(1,:) + gdepth(timeindex);
end

%%

for k=1:length(Output) % for each frequency

    sv = Output(k).Sv';
    depth = Output(1).Depth';

    [bott_sv, bott_dep, bott_ind] = find_bottom(sv, depth);
    sv_nb = removeBottom(sv,bott_sv,bott_ind);
    Output(k).Sv = sv_nb';
end

% figure
% imagesc(Output(1).Sv')
%%

dbins = 5:100;

% At each depth (starting at 5 because of the trimming step), for each
% value in the Depth entry of the Output, first find the indices where
% depths are in a 1 m bin (defined as +/-0.5 each entry in dbins) and
% assign those to I

% Next, get the average Sv by raising 10 to the power of the 1m bin depths,
% dividing each of those values by 10, and taking the mean of that entire
% vector

% Convention is to average Sv in linear space; because Output gives Sv in
% decibel space, need to convert back to linear space
% Reduces influence of weak scatterers? Unsure of exact reasoning but it's
% just what's done.
% KD reply: arithmetic operations are always done in linear space and
% although its been explained to me before the explanation is odd.  It is
% convention in the discipline though.

for ii=1:length(Output) % for each frequency
    for dd = 1:length(dbins) % for each depth bin
        for pp = 1:size(Output(ii).Depth,1) % for each ping
            I = find(Output(ii).Depth(pp,:) > (dbins(dd)-0.5) & Output(ii).Depth(pp,:) <= (dbins(dd)+0.5) );
            P(ii).avg_sv(pp,dd) = nanmean(10.^(Output(ii).Sv(pp,I)./10)); % frequency change line
        end
    end
end
%%

for jj=1:length(Output) % Frequency index
    cc = 0;
    for DD = 1:length(StartDive) % for each index where a dive might start
        if DD == length(StartDive) % important for separation of dives/deal with fact that glider comes back up sometimes
            % we need to check if we are at the end of the StartDive
            % vector, because the code changes
            if (size(P(jj).avg_sv,1) - StartDive(DD)) > 50 % at the end of some dives the glider starts coming back up. Avoid that data. Also avoid very short dives
                cc = cc+1;  % Dive count
                % grab the avg_sv values corresponding to the dive
                Dive(cc).P(jj).sv = P(jj).avg_sv(StartDive(DD):end,:);
                % get the median of the dive avg_sv
                Dive(cc).P(jj).msv = nanmedian(Dive(cc).P(jj).sv,1);
                % use the median not the mean to decrease the influence of high scattering spikes (such as bubbles and fish)
            end
        else % when we are not at the end of the StartDive vector
            if (StartDive(DD+1)-1 - StartDive(DD)) > 50
                cc = cc+1; % Dive count
                % grab the avg_sv values corresponding to the dive
                Dive(cc).P(jj).sv = P(jj).avg_sv(StartDive(DD):StartDive(DD+1)-1,:);
                % get the median of the dive avg_sv
                Dive(cc).P(jj).msv = nanmedian(Dive(cc).P(jj).sv,1);
            end
        end
    end
end

%%

d_int = 10;
% depth interval to average over

for i = 1:length(Output) % for each frequency
    % preallocate enough space 
    M(i).AvgSv = nan * ones(size(Dive, 2), length(dbins));
    for f = 1:size(Dive, 2) % for each dive
        % take the mean of d_int Sv values at a time and put them in the M
        % structure
        % any means that include a NaN are set to NaN
        % (need to include the NaNs here for depth window calculations
        % later)
        temp = movmean(Dive(f).P(i).msv, d_int, 'includenan', 'Endpoints','discard');
        M(i).AvgSv(f,1:length(temp)) = temp;
    end
end

% Then find the minimum noise interval for each frequency
for i = 1:length(Output) % for each frequency
    % find the minimum Sv value in the moving average and its index, not
    % counting any NaN values
    [N, index] = min(M(i).AvgSv,[],'all','linear','omitnan');
   
    % raw minimum value
    NoiseFloor(i) = N;
    % dive number for each frequency where the minimum is located
    [D, J] = ind2sub(size(M(i).AvgSv),index);
    divenum(i) = D;
    
    while J >= 187
        J = J - 1;
    end
    
    % depth interval for each frequency where the minimum Sv is located
    DepthWindow(i,:) = dbins(J:J+d_int);
end

% remove the temporary structure
clear temp;

% %% Save daily noise floor
% Preserved for archival purposes
% filename = strcat("/Users/delphine/Documents/MATLAB/Sept_","26th","_Noise_Floor_Data_10m.mat");
% save(filename, 'NoiseFloor_26th', 'DepthWindow_26th', 'divenum_26th', 'frequency_26th');

% Subtract the noise floor from the avg_sv structures
for  i = 1:length(Output) % for each frequency
    % subtract the frequency-dependent noise floor from avg_sv
    P(i).avg_sv = P(i).avg_sv - NoiseFloor(i);
end

for j = 1:size(Dive, 2) % for each dive
    for k = 1:length(Output) % for each frequency
        % subtract the frequency-dependent noise floor
        Dive(j).P(k).sv = Dive(j).P(k).sv - NoiseFloor(i);
        % recalculate the median
        Dive(j).P(k).msv = nanmedian(Dive(j).P(k).sv,1);
    end
end

%%
biggest = find(max([size(Output(1).Sv, 2), size(Output(2).Sv, 2),...
    size(Output(3).Sv, 2)]));

% then make a pad for the smaller Sv matrices to bring them up to the same
% size as the largest one
for i = 1:length(Output)
    if i ~= biggest
        nanpad = nan * ones(size(Output(1).Sv,1), size(Output(biggest).Sv, 2) - size(Output(i).Sv, 2));
    else
        nanpad = [];
    end
    % create temporary structure from the Output data + the nanpad, if it
    % exists, in linear space
    temp(i).raw = [Output(i).Sv, nanpad];
end

% then make a structure of frequency differences
% some of these frequency differences are positive, some are negative
% how do I preserve this when converting from linear to log and back again?
% something to investigate down the line
for j = 1:2
    Diff(j).SvDiff = temp(j+1).raw - temp(j).raw;
end

% remove the temporary structure
% clear temp;

%% Average differences into 1 m depth bins
% Same averaging procedure as for the Output file
dbins = 5:100;

tic
for ii=1:(length(Output)-1)
    for dd = 1:length(dbins)
        for pp = 1:size(Output(biggest).Depth,1)
            I = find(Output(biggest).Depth(pp,:) > (dbins(dd)-0.5) & Output(biggest).Depth(pp,:) <= (dbins(dd)+0.5) );
            PDiff(ii).avg_sv(pp,dd) = nanmean(10.^(Diff(ii).SvDiff(pp,I)./10)); % averages are in the linear space
        end
    end
end

%%
filename = strcat("/Users/dmossman/Box/Glider Data/",dep_name,"/AZFP Data/2023",sprintf('%02s',mo),"/",sprintf('%02s',da),"/Processed_Data.mat");
save(filename, 'Output', 'P', 'dbins', 'cc', 'gliderdata', 'StartDive','Diff', 'PDiff');
clear filename;

%%
filename = strcat("/Users/dmossman/Box/Glider Data/",dep_name,"/AZFP Data/2023",sprintf('%02s',mo),"/",sprintf('%02s',da),"/Processed_Data.mat");
load(filename);
clear filename;
%% Create full matrix of masked Sv

% First trying just whether one single frequency is larger than the other and vice versa

for i = 1:(length(Output)-1) % for each pair of frequencies
    P(i).masking(1).mask = (10 * log10(abs(P(i).avg_sv)) > 10 * log10(abs(P(i+1).avg_sv)));
    P(i).masking(2).mask = (10 * log10(abs(P(i).avg_sv)) <= 10 * log10(abs(P(i+1).avg_sv)));
end

figure(2)
subplot(2,2,1)
imagesc([NaN * ones(5, size(P(1).masking(1).mask, 1)); P(1).masking(1).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('38kHz > 125kHz')

subplot(2,2,2)
imagesc([NaN * ones(5, size(P(1).masking(2).mask, 1)); P(1).masking(2).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('38kHz =< 125kHz')

subplot(2,2,3)
imagesc([NaN * ones(5, size(P(2).masking(1).mask, 1)); P(2).masking(1).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('125kHz > 200kHz')

subplot(2,2,4)
imagesc([NaN * ones(5, size(P(2).masking(2).mask, 1)); P(2).masking(2).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('125kHz =< 200kHz')

% filename = strcat("/Users/delphine/Documents/BoF2020_Cruise/Visuals/MATLAB Echosounder Figures/",date,"Sept/Simple_Mask_",date,"Sept.png");
% print(gcf,'-dpng',filename,'-r0')
% clear filename;

%%
% First get the dB difference window
% Values below are for copepods between 1.27 and 2.99 mm in length, from
% Joe's spreadsheet

% windows are likely too small; play with these values until the matching
% matrix looks like the patches in the echogram
% 130-200 make 0-7 dB and see if that helps
% ignore 769 kHz for now

% Controlled parameter tuning based on MultiNet data
% Do correlations with windows in 200-455 kHz of 1 dB, 5 dB, 10 dB
% dB_Diff_Lower = [7.4, 13.7, 7.8];
dB_Diff_Lower = [7.4, 16.1, 7.8];
% dB_Diff_Upper = [7.5, 14.2, 8.8];
dB_Diff_Upper = [7.5, 19.6, 8.8];

% is the 455 kHz data "real" or just noise? Calibration issues? Offset or
% dynamic range

% pick a transect, look at the bottom value, see what the values are as a
% pseudo calibration
% if the bottom depth values are off, we will need to do a calibration
% correction; bottom is flat, broad, frequency-independent
% histogram of 1 m above bottom to 2 m below bottom (and right at the bottom)
% for each frequency, see how similar the values are (or how different)
% gives us insight into the sensitivity
% if the dynamic range of the different frequencies is off, this becomes
% trickier

%% Then create the binary filter matrix

% frequency 1 > frequency 2 as a masking matrix, to start (and vice versa)

for i = 1:size(PDiff,2) % for each frequency difference
    % PDiff values are in linear space, dB_Diff values are in log space
    % convert PDiff to log space before creating mask
    PDiff(i).mask = (dB_Diff_Lower(i) < 10 * log10(abs(PDiff(i).avg_sv)) & 10 * log10(abs(PDiff(i).avg_sv)) < dB_Diff_Upper(i));
end

% Then multiply masking matrix by Sv to get masked observed Sv

P(1).masked = P(1).avg_sv;
% 130 kHz is not masked

for i = 1:size(PDiff,2) % for each frequency difference
    P(i+1).masked = P(i+1).avg_sv .* PDiff(i).mask;
    P(i+1).masked(P(i+1).masked == 0) = NaN;
end

%% Create by-dive matrices of masked Sv

for i = 1:size(Dive,2) % for each dive
    for j = 1:4 % for each frequency
        Dive(i).P(j).masked_sv = P(j).masked(Dive(i).Index(1):Dive(i).Index(2),:);
    end
end
%% Plot unmasked and masked 455 kHz only next to each other

figure(1)

% subplot(4,3,1)
% imagesc([NaN * ones(5, size(P(1).avg_sv, 1)); 10 * log10(abs(P(1).avg_sv'))])
% colormap('jet');
% caxis([-80 -40]);
% xlabel('Ping Number')
% ylabel('Depth (m)')
% title('130kHz')

subplot(2,2,1)
imagesc([NaN * ones(5, size(P(2).avg_sv, 1)); 10 * log10(abs(P(2).avg_sv'))])
colormap('jet');
caxis([-80 -40]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('200kHz unmasked')

subplot(2,2,3)
imagesc([NaN * ones(5, size(P(3).avg_sv, 1)); 10 * log10(abs(P(3).avg_sv'))])
colormap('jet');
caxis([-80 -40]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('455kHz unmasked')

ax1 = subplot(2,2,2);
imagesc([NaN * ones(5, size(PDiff(2).mask, 1)); PDiff(2).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('455kHz masking matrix')

subplot(2,2,4)
imagesc([NaN * ones(5, size(P(3).masked, 1)); 10 * log10(abs(P(3).masked'))])
colormap('jet');
caxis([-80 -40]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('455kHz masked')

colormap(ax1,gray);

h = colorbar;
set(get(h,'label'),'string','Sv (dB scattering per unit volume)');

h.Position(4) = 0.65;
h.Position(1) = .94-h.Position(3);
h.Position(2) = 0.5-h.Position(4)/2;

AddLetters2Plots(gcf,'VShift',-0.03,'Direction','TopDown')

sgtitle(strcat("Decibel Window Used: ",num2str(dB_Diff_Lower(2))," - ",num2str(dB_Diff_Upper(2))));

%% save figure
% filename = strcat("/Users/delphine/Documents/BoF2020_Cruise/Visuals/MATLAB Echosounder Figures/",date,"Sept/Unmasked_Masked_Comparison_",date,"Sept.png");
% filename = strcat("/Users/delphine/Documents/BoF2020_Cruise/Visuals/MATLAB Echosounder Figures/",...
%     date,...
%     "Sept/Unmasked_Masked_Comparison_10_dB_",...
%     date,...
%     "Sept.png");
% print(gcf,'-dpng',filename,'-r0')
% clear filename
close all
%% save files
filename = strcat("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/",da,"Sept_Masking_Data.mat");
save(filename, 'Output', 'P', 'Dive', 'Diff', 'DiveDiff', 'PDiff');