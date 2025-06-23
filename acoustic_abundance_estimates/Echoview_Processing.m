%% Utilize zoop/fish processing functions to generate excel files that can be imported/graphed in R
% This is a separate file so that the function can be put into a loop and iterated over months/days
% Author: Delphine Mossman
% Date Created: 12 Oct 2023
% Date Last Modified: 23 June 2025
% 1. Get the directory of exported Echoview data
% 2. For each file, run either the fish or zooplankton function


clear variables; close all; clc;

% Add paths to the Matlab processing scripts
addpath(genpath('C:\Users\Delphine\Box\messy_acoustics_processing\'))

%% Initialization

% Path to exported Echoview data
dep_name = uigetdir("C:\Users\Delphine\Box\Glider Data\", "Select the glider deployment folder.");
addpath(genpath(dep_name));
dep_name = char(extractBetween(dep_name,35,strlength(dep_name)));

% Some formatting things
dep_year = char(extractBetween(dep_name,6,9));

dep_start = input("Enter the numerical month the deployment started: ");
dep_end = input("Enter the numerical month the deployment ended: ");

%% Function loop

for i = dep_start:dep_end
    yr=string(dep_year);
    % Directory with Echoview csv files
    input_dir = strcat('C:\Users\Delphine\Box\Glider Data\',dep_name,'\Echoview CSV Export Files',...
        '\',char(yr),sprintf('%02d',i),'\');
    % Directory to output formatted files
    output_dir = strcat('C:\Users\Delphine\Box\Glider Data\',dep_name,'\Derived Biomass Data\');

    days = dir([input_dir '*_200kHz.csv']);
    
    for j = 1:length(days)
        mo = i;
        da = days(j).name(12:13);
        
        % Needs to be manually changed depending on whether the AZFP is
        % zoop or fish configured
        Echoview_to_zoop_biomass(yr,mo,da,input_dir,output_dir)
        % Echoview_to_fish_biomass(yr,mo,da,input_dir,output_dir)

    end
end