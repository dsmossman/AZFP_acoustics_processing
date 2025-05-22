clear variables
close all
clc

% Give Matlab access to files
addpath(genpath('C:\Users\Delphine\Box\ACOUSTIC DATA PROCESSING PROTOCOLS\AZFP Processing\Glider file formatting\spt-master\'))
addpath(genpath('C:\Users\Delphine\Box\ACOUSTIC DATA PROCESSING PROTOCOLS\AZFP Processing\Glider file formatting\seawater_ver3_3.1\'))
addpath('C:\Users\Delphine\Box\ACOUSTIC DATA PROCESSING PROTOCOLS\AZFP Processing\Glider file formatting\')

dep_name = input("Enter the full name of the deployment: ","s");

addpath(genpath(strcat('C:\Users\Delphine\Box\Glider Data\',dep_name)))

cd(strcat('C:\Users\Delphine\Box\Glider Data\',dep_name,'\Echoview CSV Import Files\'))

% Formatting setup
carriage_return = 13;
line_feed = 10;
%% Import glider data CSV

data = readtable(strcat('C:\Users\Delphine\Box\Glider Data\',dep_name,'\',dep_name,'-profile-sci-delayed.csv'),...
    "PreserveVariableNames",true);
data = data(2:end,:);
%% Formatting GPS data
gps_time = string(cell2mat(data{:,"time"}));
gps_lat = num2str(data{:,"latitude"});
gps_lon = num2str(data{:,"longitude"});

gps_time_1 = datetime(extractBefore(gps_time,11),'Format','yyyy-MM-dd');
gps_time_2 = datetime(extractBetween(gps_time,12,19),'Format','HH:mm:SS');
gps_time_3 = zeros(length(gps_time),1);
% gps_time_3 = str2double(extractBetween(gps_time,21,22)) * 10;
%% Write to GPS csv file

fid = fopen('data.gps.csv', 'w+');
fprintf(fid, ['GPS_date,GPS_time,GPS_millseconds,Latitude,Longitude' char(carriage_return) char(line_feed)]);
fclose(fid);

writetable(table(gps_time_1,gps_time_2,gps_time_3,gps_lat,gps_lon),'data.gps.csv','WriteMode','Append')
fprintf("GPS data written to data.gps.csv\n"+...
    "Location: %s%s\n",pwd,'\data.gps.csv');
%% Formatting pitch data

indexes = ~isnan(data{:,"m_pitch"});

pitch = data{indexes,"m_pitch"} * 180/pi;

pitch_time_1 = gps_time_1(indexes);
pitch_time_2 = gps_time_2(indexes);
pitch_time_3 = gps_time_3(indexes);
%% Write to pitch csv file

fid = fopen('data.pitch.csv', 'w+');
fprintf(fid, ['Pitch_date,Pitch_time,Pitch_millseconds,Pitch_angle' char(carriage_return) char(line_feed)]);
fclose(fid);

writetable(table(pitch_time_1,pitch_time_2,pitch_time_3,pitch),'data.pitch.csv','WriteMode','Append')
fprintf("Pitch data written to data.pitch.csv\n"+...
    "Location: %s%s\n",pwd,'\data.pitch.csv');
%% Formatting roll data

indexes = ~isnan(data{:,"m_roll"});

roll = data{indexes,"m_roll"} * 180/pi;

roll_time_1 = gps_time_1(indexes);
roll_time_2 = gps_time_2(indexes);
roll_time_3 = gps_time_3(indexes);
%% Write to roll csv file

fid = fopen('data.roll.csv', 'w+');
fprintf(fid, ['Roll_date,Roll_time,Roll_milliseconds,Roll_angle' char(carriage_return) char(line_feed)]);
fclose(fid);

writetable(table(roll_time_1,roll_time_2,roll_time_3,roll),'data.roll.csv','WriteMode','Append')
fprintf("Roll data written to data.roll.csv\n"+...
    "Location: %s%s\n",pwd,'\data.roll.csv');
%% Formatting depth data

indexes = ~isnan(data{:,"depth"});

depth = data{indexes,"depth"};

dummy_column = ones(length(depth),1);

depth_time_1 = datetime(gps_time_1(indexes),"Format",'yyyyMMdd');
depth_time_2 = [char(datetime(gps_time_2(indexes),"Format",'HHmmSS'))...
    num2str(gps_time_3(indexes),'%03d')...
    char(48 * dummy_column)];
%% Extra formatting since it's not a csv file

EVL_file_header = 'EVBD 3 11.1.49';

depth_status = num2str(3 * dummy_column);

%% Write to depth evl file

fid = fopen('data.depth.evl', 'w+');
fprintf(fid, [EVL_file_header char(carriage_return) newline ...
             num2str(length(depth)) char(carriage_return) newline]);
fclose(fid);

writetable(table(depth_time_1, depth_time_2, depth, depth_status),...
        'data.depth.evl',...
        'Delimiter','space',...
        'WriteMode','Append','FileType','text');
fprintf("Depth data written to data.depth.evl\n"+...
    "Location: %s%s\n",pwd,'\data.depth.evl');