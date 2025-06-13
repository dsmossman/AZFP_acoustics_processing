function Echoview_to_zoop_biomass(yr, mo, da, input_dir, output_dir)
% PURPOSE: convert Echoview integrated output to target species biomass per
% glider downcast depth bin
% This is specific to converting to zooplankton biomass

% Delphine Mossman, delphine.mossman@marine.rutgers.edu
% Last updated June 2024

% Current parameters: 1m depth, 0.1 nmi GPS distance

%% Load echo integration .csv files of interest

% preallocate data
data = table();

%Select csv file from directory
files = dir([input_dir '/*_',sprintf('%02s',da),'_*kHz.csv']);

% List of frequencies

freqs = [120,200,455,769];

%Import the selected csv file and add frequency column
for i =1:length(files)
    filnam = [input_dir '/' files(i).name];
    temp = readtable(filnam); clear filnam
    temp{:,'Frequency'} = freqs(i);

    data = [data;temp]; clear temp
end

% Change to datetime
data.Date_M = datetime(data.Date_M,'ConvertFrom','yyyymmdd');

% Sort matrix
data = sortrows(data, {'Frequency','Interval'});

%% Get rid of empty rows, cells with too strong a response, and cells that don't have a response in all four frequencies

data.Sv_mean(data.Sv_mean == -999) = NaN;

data_120 = data((data.Frequency == 120) & (data.Sv_mean <= -60),:);
data_200 = data((data.Frequency == 200) & (data.Sv_mean <= -60),:);
data_455 = data((data.Frequency == 455) & (data.Sv_mean <= -60),:);
data_769 = data((data.Frequency == 769) & (data.Sv_mean <= -60),:);

if height(data_120) ~= 0 && height(data_200) ~= 0 && height(data_455) ~= 0 && height(data_769) ~= 0

data2_120 = data_120(ismember(data_120(:,{'Interval','Layer'}),...
    intersectm(data_120(:,{'Interval','Layer'}),data_200(:,{'Interval','Layer'}),data_455(:,{'Interval','Layer'}),data_769(:,{'Interval','Layer'}),'rows'),'rows'),:);

data2_200 = data_200(ismember(data_200(:,{'Interval','Layer'}),...
    intersectm(data_120(:,{'Interval','Layer'}),data_200(:,{'Interval','Layer'}),data_455(:,{'Interval','Layer'}),data_769(:,{'Interval','Layer'}),'rows'),'rows'),:);

data2_455 = data_455(ismember(data_455(:,{'Interval','Layer'}),...
    intersectm(data_120(:,{'Interval','Layer'}),data_200(:,{'Interval','Layer'}),data_455(:,{'Interval','Layer'}),data_769(:,{'Interval','Layer'}),'rows'),'rows'),:);

data2_769 = data_769(ismember(data_769(:,{'Interval','Layer'}),...
    intersectm(data_120(:,{'Interval','Layer'}),data_200(:,{'Interval','Layer'}),data_455(:,{'Interval','Layer'}),data_769(:,{'Interval','Layer'}),'rows'),'rows'),:);


data = [data2_120;data2_200;data2_455;data2_769];

% Data differencing

data.Difference = zeros(height(data),1) * NaN;

data.Difference(data.Frequency == 200) = data2_200.Sv_mean - data2_120.Sv_mean;
data.Difference(data.Frequency == 455) = data2_455.Sv_mean - data2_200.Sv_mean;
data.Difference(data.Frequency == 769) = data2_769.Sv_mean - data2_455.Sv_mean;

else
    headers = data.Properties.VariableNames;
    data = cell(0,size(data,2));
    data = cell2table(data);
    data.Properties.VariableNames = headers;
    clear headers
end

%% Do some broad ID/separation

% Create a new table variable for species

data.Species = string(zeros(height(data),1));

num_cells = height(data)/4;

% Find copepod echoes

for i=1:num_cells % for each triplet of pings
    % pull out the volume backscattering coefficient/strength values for
    % the important frequencies

    % backscatter differences based on previous tow data and acoustic
    % models (see spreadsheet)

    if ((data.Difference(i+num_cells*2)) >= -0.93) && ((data.Difference(i+num_cells*2)) <= 8.23)
        data.Species(i) = "Gelatinous Zooplankton";
        data.Species(i+num_cells) = "Gelatinous Zooplankton";
        data.Species(i+2*num_cells) = "Gelatinous Zooplankton";
        data.Species(i+3*num_cells) = "Gelatinous Zooplankton";
    elseif ((data.Difference(i+num_cells*2)) > 14.1) && ((data.Difference(i+num_cells*2)) < 14.6)
        data.Species(i) = "Large Copepod";
        data.Species(i+num_cells) = "Large Copepod";
        data.Species(i+2*num_cells) = "Large Copepod";
        data.Species(i+3*num_cells) = "Large Copepod";
    
    % elseif ((data.Difference(i+num_cells*2)) > 10.1) && ((data.Difference(i+num_cells*2)) < 13.8)
    %     data.Species(i) = "Small Copepod";
    %     data.Species(i+num_cells) = "Small Copepod";
    %     data.Species(i+2*num_cells) = "Small Copepod";
    %     data.Species(i+3*num_cells) = "Small Copepod";      
    end
end

% Label non-identified rows
data.Species(data.Species == "0") = "Empty Cell";

%% Calculate Abundance using the inverse problem
% [Z] = 10^((Sv - TS)/10)
% Then use IDW from P. parvus for biomass: 5.55 micrograms
% gives individuals or grams per meter cubed

for k = 1:height(data)
    if data.Species(k) == "Large Copepod" && data.Frequency(k) == 455
        data.Abundance(k) = 10.^((data.Sv_mean(k) - -108.3)/10); % from Brandyn's work
        data.Biomass(k) = data.Abundance(k) * 269.66e-6; % average Calanus sp. IDW
    % elseif data.Species(k) == "Small Copepod" && data.Frequency(k) == 455
    %     data.Abundance(k) = 10.^((data.Sv_mean(k) - -122.7)/10); % from my model
    %     data.Biomass(k) = data.Abundance(k) * 21.5e-6; % avg centropages/oithona sp. IDW
    else
        data.Abundance(k) = 0;
        data.Biomass(k) = 0;
    end
end

%% Export excel file with final data for that downcast

% Output file base name
BaseName = strcat('RMI',yr,'_',sprintf('%02d',mo),'_',sprintf('%02s',da),'_Biomass_Data.csv');
disp(strcat("Writing data for ",string(mo),"/",string(da),"/",string(yr)," to file."))
writetable(data,strcat(output_dir,BaseName))

end