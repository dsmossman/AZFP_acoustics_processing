%% PURPOSE: convert Echoview integrated output to target species biomass per glider downcast depth bin
% This is specific to converting to fish abundance
% Author: Delphine Mossman
% Date Created: 14 Mar 2023
% Date Last Modified: 23 June 2025
% 1. Load echo integration CSV files
% 2. Remove rows without data in all three frequencies
% 3. Perform broad classification (swimbladder-bearing vs swimbladderless
% fish and weak vs strong echoes)
% 4. If applicable, perform species classification
% 5. Calculate obs and use that to calculate concentration/biomass
% 6. Export CSV file with abundance data

function Echoview_to_fish_biomass(yr, mo, da, input_dir, output_dir)

% preallocate data
data = table();

%Select csv file from directory
files = dir([input_dir '/*_',sprintf('%02s',da),'_*kHz.csv']);

%Import the selected csv file and add frequency column
for i =1:length(files)
    filnam = [input_dir '/' files(i).name];
    temp = readtable(filnam); clear filnam

    data = [data;temp]; clear temp
end

% Change to datetime
% data.Date_S = datetime(data.Date_S,'ConvertFrom','yyyymmdd');
% data.Date_E = datetime(data.Date_E,'ConvertFrom','yyyymmdd');
data.Date_M = datetime(data.Date_M,'ConvertFrom','yyyymmdd');

% Sort matrix
data = sortrows(data, {'Interval','Layer','Frequency'});

%% Get rid of empty rows and cells that don't have a response in all three frequencies

data(data.Sv_mean == -999,:) = [];
data(data.Sv_mean == 9999,:) = [];

data_38 = data(data.Frequency == 38,:);
data_120 = data(data.Frequency == 120,:);
data_200 = data(data.Frequency == 200,:);

% FOR RU39-20230817T1520, 200 kHz data looks weird after August 23rd, so
% exclude that from the analysis?

if (yr == "2023" && (mo == 8 && str2double(da) >= 23) || mo == 9)
    if height(data_38) ~= 0 && height(data_120) ~= 0

        data2_38 = data_38(ismember(data_38(:,{'Interval','Layer'}),...
            intersectm(data_38(:,{'Interval','Layer'}),data_120(:,{'Interval','Layer'}),'rows'),'rows'),:);

        data2_120 = data_120(ismember(data_120(:,{'Interval','Layer'}),...
            intersectm(data_38(:,{'Interval','Layer'}),data_120(:,{'Interval','Layer'}),'rows'),'rows'),:);

        data = [data2_38;data2_120];

    else
        headers = data.Properties.VariableNames;
        data = cell(0,33);
        data = cell2table(data);
        data.Properties.VariableNames = headers;
        clear headers
    end
else
    if height(data_38) ~= 0 && height(data_120) ~= 0 && height(data_200) ~= 0

        data2_38 = data_38(ismember(data_38(:,{'Interval','Layer'}),...
            intersectm(data_38(:,{'Interval','Layer'}),data_120(:,{'Interval','Layer'}),data_200(:,{'Interval','Layer'}),'rows'),'rows'),:);

        data2_120 = data_120(ismember(data_120(:,{'Interval','Layer'}),...
            intersectm(data_38(:,{'Interval','Layer'}),data_120(:,{'Interval','Layer'}),data_200(:,{'Interval','Layer'}),'rows'),'rows'),:);

        data2_200 = data_200(ismember(data_200(:,{'Interval','Layer'}),...
            intersectm(data_38(:,{'Interval','Layer'}),data_120(:,{'Interval','Layer'}),data_200(:,{'Interval','Layer'}),'rows'),'rows'),:);

        data = [data2_38;data2_120;data2_200];
    else
        headers = data.Properties.VariableNames;
        data = cell(0,33);
        data = cell2table(data);
        data.Properties.VariableNames = headers;
        clear headers
    end
end

% Data differencing

data.Difference = zeros(height(data),1) * NaN;

data.Difference(data.Frequency == 120) = data2_120.Sv_mean - data2_38.Sv_mean;
if exist('data2_200','var')
    data.Difference(data.Frequency == 200) = data2_200.Sv_mean - data2_120.Sv_mean;
end
%% Do some broad ID/separation

% Create a new table variable for species

data.Species = string(zeros(height(data),1));

% Start with barebones classification (i.e. no swimbladder, clupeoid, or
% physoclist) depending on frequency diff properties
% No swimbladder = like mackerel
% Clupeoid = depth dependence
% Physoclist = depth independence

% Rudstam et al., 2009 equation 1 for determining Sv threshold from TS?

if exist('data2_200','var')
    num_cells = height(data)/3;
else
    num_cells = height(data)/2;
end

%% Find swimbladderless vs swimbladder-having echoes

for i=1:num_cells % for each triplet/pair of pings
    % pull out the volume backscattering coefficient/strength values for each frequency
    Sv_38 = data.Sv_mean(i);
    Sv_120 = data.Sv_mean(i+num_cells);

    sv_38 = 10^(Sv_38/10);
    sv_120 = 10^(Sv_120/10);

    Diff_120_38 = data.Difference(i+num_cells);

    if exist('data2_200','var')
        Sv_200 = data.Sv_mean(i+2*num_cells);
        sv_200 = 10^(Sv_200/10);
        Diff_200_120 = data.Difference(i+2*num_cells);
    end

    % disp([sv_38, sv_120, sv_200])


    % possibly need to play with these values/conditions

    if exist('Sv_200','var')
        if Diff_120_38 > 0 && Diff_200_120 > 0
            if sv_200/sv_38 > 3 && sv_200/sv_38 < 6 && sv_120/sv_38 > 1.5 && sv_120/sv_38 < 4
                % swimbladderless
                data.Species([i i+num_cells i+2*num_cells]) = "Swimbladderless fish";
            end
        elseif sv_120/sv_38 < 1 && sv_200/sv_38 < 1 ...
                && Sv_38 > -60 && Sv_120 > -70 && Sv_200 > -70
            % swimbladder, most likely herring or alewife, maybe menhaden?
            % Gorska et al., 2004 and Lucca and Warren, 2019
            data.Species([i i+num_cells i+2*num_cells]) = "Swimbladder fish";
        else

            salp_depth = data.Depth_mean(i);

            if salp_depth <=10
                min_diff = -0.2;
                max_diff = 1.5;
            else
                min_diff = 4.2;
                max_diff = 5.1;
            end

            if Diff_120_38 > min_diff && Diff_120_38 < max_diff
                data.Species([i i+num_cells i+2*num_cells]) = "Gelatinous Zooplankton";
            end
        end
    else
        if Diff_120_38 > 0
            if sv_120/sv_38 > 1.5 && sv_120/sv_38 < 4
                % swimbladderless, weak echoes are likely sand lance,
                % strong echoes are... not that
                data.Species([i i+num_cells]) = "Swimbladderless fish";
            end
        elseif sv_120/sv_38 < 1 && ...
               Sv_38 > -60 && Sv_120 > -70
            % swimbladder, most likely menhaden
            % Gorska et al., 2004 and Lucca and Warren, 2019
            data.Species([i i+num_cells]) = "Swimbladder fish";
        else
            salp_depth = data.Depth_mean(i);

            if salp_depth <=10
                min_diff = -0.2;
                max_diff = 1.5;
            else
                min_diff = 4.2;
                max_diff = 5.1;
            end

            if Diff_120_38 > min_diff && Diff_120_38 < max_diff
                data.Species([i i+num_cells]) = "Gelatinous Zooplankton";
            end
        end
    end
end
% Label non-identified rows
data.Species(data.Species == "0") = "Unidentified";

%% Specific species assignments
% Anything I am confident about goes here

% data.Species(data.Species == "Swimbladderless fish (weak)") = "Sand lance";
% data.Species(data.Species == "Swimbladder fish (strong)") = "Menhaden";

%% Species-specific obs calculations

m = 20; % slope for most calculations

% ln(W) = ln(a) + b * ln(L)
% W = exp(ln(a) + b * ln(L))
% from Wigley 2003

squid_L = 6.2; % squid mean mantlelength in cm
squid_b = 79.90; % squid intercept at freq = 120kHz and slope = 20
squid_W = exp(-1.04605 + 2.05558 * log(squid_L)); % squid mean weight in g based on L

squid_TS = m * log10(squid_L) - squid_b;
squid_obs = 10^(squid_TS/10);

mack_L = 22.8; % mean length of mackerel in cm
mack_b = 53.58; % mackerel intercept at 200 kHz
mack_W = exp(-12.6713 + 3.3119 * log(mack_L)) * 1000; % mean weight of mackerel in g based on L

mack_TS = m * log10(mack_L) - mack_b;
mack_obs = 10^(mack_TS/10);

% herring is depth-dependent; the obs function is at the end of this script
herr_L = 19.7; % herring mean length in cm
herr_W = exp(-11.7972 + 3.0314 * log(herr_L)) * 1000; % herring mean weight in g based on L

bass_L = 58; % striped bass mean length in cm
bass_b = 56.26; % striped bass intercept at freq = 120 kHz
bass_m = 15.37; % striped bass slope at freq = 120 kHz
bass_W = exp(-11.7959 + 3.1383 * log(bass_L)) * 1000; % striped bass mean weight in g based on L

bass_TS = bass_m * log10(bass_L) - bass_b;
bass_obs = 10^(bass_TS/10);

ale_L = 18.5; % alewife mean length in cm
ale_m = 52.6; % alewife slope at 120 kHz
ale_b = 100.2; % alewife intercept at 120 kHz
ale_W = exp(-13.3875 + 3.6716 * log(ale_L)) * 1000; % alewife mean weight in g based on L

ale_TS = ale_m * log10(ale_L)- ale_b; % at 38 kHz, generalized clupeoid equation
ale_obs = 10^(ale_TS/10);

ale_herr_L = (ale_L + herr_L)/2; % length between alewife and herring in cm
ale_herr_W = exp(-12.5923 + 3.3515 * log(ale_herr_L)) * 1000; % averaged weight equation from herring/alewife

% all values/equations derived from Lucca and Warren 2019
menh_L = 25.5; % menhaden mean total length in cm
menh_W = exp(-11.396 +3.08 * log(menh_L)); % menhaden mean weight in g based on TL
menh_m = 20.4; % menhaden slope at 120 kHz
menh_b = 68.88; % menhaden intercept at 120 kHz

menh_TS = menh_m * log10(menh_L) - menh_b; % generalized menhaden TS equation
menh_obs = 10^(menh_TS/10);

bttr_L = 14.0; % butterfish mean fork length in cm; equation prefers total length so look for that2
bttr_W = exp(-10.6315 + 2.9225 * log(bttr_L)) * 1000; % butterfish mean weight in g from length

bttr_TS = m * log10(bttr_L) - mack_b; % using the equation for mackerel but with butterfish length
bttr_obs = 10^(bttr_TS/10);

blue_L = 42.0; % average bluefish fork length in cm
blue_W = exp(-11.4296 + 3.0458 * log(blue_L)) * 1000; % average bluefish weight in g from length
blue_b = 67.4; % bluefish intercept when slope = 20

blue_TS = m * log10(blue_L) - blue_b; % generalized bluefish TS equation
blue_obs = 10^(blue_TS/10);

% sand lance/sand eel stuff
sala_L = 17.8; % mean fork length; from Zhu et al., 2022
sala_W = 0.002 * sala_L^2.994; % mean weight from length; from Scott, 1972

sala_TS = 53.7 * log10(sala_L) - 124.3; % TS/FL relationship at 38 kHz; from Zhu et al., 2022
sala_obs = 10^(sala_TS/10);

%% "Generic" swimbladderless and swimbladder fish obs calculations
% No clue if this is correct methodology but I'm trying to get ballpark
% numbers here

swim_yes_L = mean([menh_L, herr_L, ale_L, blue_L]);
swim_yes_W = mean([menh_W, herr_W, ale_W, blue_W]);

swim_yes_TS = m * log10(swim_yes_L) - 69.65; % average of the Foote (1987) TS equations for 38 kHz
swim_yes_obs = 10^(swim_yes_TS/10);

swim_no_L = mean([squid_L, mack_L, bttr_L]);
swim_no_W = mean([squid_W, mack_W, bttr_W]);

swim_no_TS = m * log10(swim_no_L) - squid_b; % using the squid TS calc for now since it's valid for 120 kHz
swim_no_obs = 10^(swim_no_TS/10);

%% Calculate Abundance/Biomass

% This is where it will differ the most for fish; see Davison et al. 2015
% article for the process
% Area backscattering coefficient (sa) divided by backscattering cross
% section (obs) = abundance
% sa is units of m2/m2, obs is units of m2, so result is individuals/m2
% Abundance * mean weight = biomass

% sa is what we get from the measurements (written as ABC in the table)
% obs is different for each fish species and length distribution
% obs is also related to TS, which I have a bunch of equations for from my
% research

% TS = 10log10(obs)
% therefore, 10log10(obs) = whatever TS equation from length/frequency on
% a per-species basis

for k = 1:height(data)
    if data.Species(k) == "Squid" && (data.Frequency(k) == 120 || data.Frequency(k) == 125)
        data.Abundance(k) = data.ABC(k)/squid_obs;
        data.Biomass(k) = data.Abundance(k) * squid_W;
    elseif data.Species(k) == "Mackerel"
        if exist('data2_200','var') && data.Frequency(k) == 200
            data.Abundance(k) = data.ABC(k)/mack_obs;
        elseif data.Frequency(k) == 38
            mack_b = 90.05;
            mack_TS_38 = m * log10(mack_L) - mack_b;
            mack_obs_38 = 10^(mack_TS_38/10);

            data.Abundance(k) = data.ABC(k)/mack_obs_38;
        end
        data.Biomass(k) = data.Abundance(k) * mack_W;
    elseif data.Species(k) == "Alewife" && data.Frequency(k) == 38
        % using herring TS calculation for depth dependence
        data.Abundance(k) = data.ABC(k)/herr_obs(data.Depth_mean(k));
        data.Biomass(k) = data.Abundance(k) * ale_W;
    elseif data.Species(k) == "Butterfish"
        if exist('data2_200','var') && data.Frequency(k) == 200
            data.Abundance(k) = data.ABC(k)/bttr_obs;
        elseif data.Frequency(k) == 38
            mack_b = 90.05;
            bttr_TS_38 = m * log10(bttr_L) - mack_b;
            bttr_obs_38 = 10^(bttr_TS_38/10);

            data.Abundance(k) = data.ABC(k)/bttr_obs_38;
        end
        data.Biomass(k) = data.Abundance(k) * bttr_W;
    elseif data.Species(k) == "Bluefish" && data.Frequency(k) == 38
        data.Abundance(k) = data.ABC(k)/blue_obs;
        data.Biomass(k) = data.Abundance(k) * blue_W;
    elseif data.Species(k) == "Menhaden" && (data.Frequency(k) == 120 || data.Frequency(k) == 125)
        data.Abundance(k) = data.ABC(k)/menh_obs;
        data.Biomass(k) = data.Abundance(k) * menh_W;
    elseif data.Frequency(k) == 38 && data.Species(k) == "Sand lance"
        data.Abundance(k) = data.ABC(k)/sala_obs;
        data.Biomass(k) = data.Abundance(k) * sala_W;
    elseif data.Frequency(k) == 38 && (data.Species(k) == "Swimbladderless fish (strong)" || data.Species(k) == "Swimbladderless fish (weak)")
        data.Abundance(k) = data.ABC(k)/swim_no_obs;
        data.Biomass(k) = data.Abundance(k) * swim_no_W;
    elseif (data.Frequency(k) == 120 || data.Frequency(k) == 125) && (data.Species(k) == "Swimbladder fish (strong)" || data.Species(k) == "Swimbladder fish (weak)")
        data.Abundance(k) = data.ABC(k)/swim_yes_obs;
        data.Biomass(k) = data.Abundance(k) * swim_yes_W;
    else
        data.Abundance(k) = 0;
        data.Biomass(k) = 0;
    end
end

%% Export excel file with final data for that downcast

% Output file base name
BaseName = strcat('RMI',yr,'_',sprintf('%02d',mo),'_',sprintf('%02s',da),'_Biomass_Data.csv');
disp(strcat("Writing data for ",string(mo),"/",string(da)," to file."))
writetable(data,strcat(output_dir,BaseName))
end
%%
function herr_obs = herr_obs(herr_z)
herr_L = 18.5; % alewife mean length in cm
herr_b = 65.4; % herring intercept for 38 kHz and depth dependent function

herr_TS = 20 * log10(herr_L) - 2.3 * log10(1 + herr_z/10) - herr_b;
herr_obs = 10^(herr_TS/10);
end
