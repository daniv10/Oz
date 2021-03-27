function mem=movingAverage(s,num,shift) % movingAverage function (s=vector of values, num=number of moving average, shift=number of shifting days)
len=length(s); % define len as length of the vector s.
nana=nan(1,num-1+shift); % chaining number of NaN's into nana according to the demanded moving average.
v=[]; % reset vector of the movingAverage.
for i=num:len % loop1: i runs from num to len(because numbers before num are NaN's).
        res=0; % reset res.
        for j=i-num+1:i % loop2: j runs from 1 to i.
             res=res+s(j); % add s(j) in res.
        end
        res=res/num; % divide res wih num.
        v=[v,res]; % chaining res at the end of vector v.
end
mem=[nana,v]; % chaining nana at the beginning of vector v.

Date=datetime(Date,'InputFormat','dd/MM/yyyy');  % define date format.

highPrice=tbl.PX_HIGH; ; % highPrice=vector of values of column PX_HIGH from excel file.
lowPrice=tbl.PX_LOW;  % lowPrice=vector of values of column PX_LOW from excel file.
openPrice=tbl.PX_OPEN;  % openPrice=vector of values of column PX_OPEN from excel file.
closePrice=tbl.PX_LAST;  % closePrice=vector of values of column PX_LAST from excel file.
MedPrice = (highPrice + lowPrice) / 2 % Average between high price and low price

JAW = movingAverage(MedPrice,13,8)  % perform moving average 13 about the average price with shift of 8 days.

maxLoc=[];  % reset vector of index local maximum.
minLoc=[]; % reset vector of index local minimum.
len=length(highPrice); % define length of loop.

for i=1:len-2 
    if highPrice(i)<highPrice(i+1)&&highPrice(i+1)>highPrice(i+2) % finds local max of highPrice.
       maxLoc=[ maxLoc,i+1]; % chain the local max into vector.
    end
    if lowPrice(i)>lowPrice(i+1)&&lowPrice(i+1)<lowPrice(i+2)  % finds  local min of lowPrice.
       minLoc=[ minLoc,i+1];  % chaining them into vector.
    end
end
cla  % order that deletes the all graphs.
plot(1:length(JAW),JAW, 'b') % displays JAW graph in blue color.
hold on
TEETH = movingAverage(MedPrice,8,5) )  % perform moving average 8 about the average price with shift of 5 days.  
plot(1:length(TEETH),TEETH, 'r') % at the same axes displays TEETH graph in red color.
hold on
LIPS = movingAverage(MedPrice,5,3) % perform moving average 5 about the average price with shift of 3 days.
hold on
plot(1:length(LIPS),LIPS, 'g') % at the same axes displays LIPS graph in green color.
hold on
plot(minLoc,0.995*lowPrice(minLoc),'rd') % at the same axes displays red diamonds as local minimum 0.5% under the local minimum.
hold on
plot(maxLoc,1.005*highPrice(maxLoc),'gd') % at the same axes displays green diamonds as local maximum 0.5% above the local maximum.

candle(highPrice, lowPrice, closePrice, openPrice) % at the same axes displays highPrice, lowPrice, closePrice and openPrice vectors on graph in candle form.

ch = get(gca,'children');  % define ch for candle.
set(ch(1),'FaceColor','r')  % ch1 defines red candles.
set(ch(2),'FaceColor','g')  % ch2 defines green candles.

legend('Jaw','Teeth', 'Lips','Local min' , 'Local max'); % describes legend data of the graph.

res=dish(JAW,TEETH,LIPS) % the result from the shuffle function.
if res % checks if res=1 (it means there is a shuffle).
    [y,Fs] = audioread('dishdush.wma'); % load the audio file
    sound(y,Fs);  % play audio file.
end

function varargout = GUIproject(varargin)
% GUIPROJECT MATLAB code for GUIproject.fig
%      GUIPROJECT, by itself, creates a new GUIPROJECT or raises the existing
%      singleton*.
%
%      H = GUIPROJECT returns the handle to a new GUIPROJECT or the handle to
%      the existing singleton*.
%
%      GUIPROJECT('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in GUIPROJECT.M with the given input arguments.
%
%      GUIPROJECT('Property','Value',...) creates a new GUIPROJECT or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before GUIproject_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to GUIproject_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help GUIproject

% Last Modified by GUIDE v2.5 24-Sep-2018 19:38:05

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @GUIproject_OpeningFcn, ...
                   'gui_OutputFcn',  @GUIproject_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before GUIproject is made visible.
function GUIproject_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to GUIproject (see VARARGIN)

% Choose default command line output for GUIproject
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes GUIproject wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = GUIproject_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles) %'click here'  push button function.
% hObject    handle to pushbutton1 (see GCBO)               
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
set(handles.text2,'String', ''); % delete the string in text2 (if exists).
error=0; % reset error flag.
cla  % delete the graph (if exists).

Start_Date=get(handles.edit1, 'string'); % get dates that user writes in start date.
if isempty(Start_Date) % condition: if field is empty then...
    error=1; % set error flag. 
     set(handles.text6,'String', 'נא להכניס תאריך התחלה'); % write in text6 the string.
end

try  % checks if the string that was inserted as start date is in correct format. if not then jumps to catch.
   D = datenum(Start_Date,'dd/mm/yyyy');
catch
   error=1; % set error flag.
   set(handles.text6,'String', 'פורמט לא תקין נא להכניס תאריך בפורמט dd/mm/yyyy'); % write in text6 the string.
end

End_Date=get(handles.edit2, 'string'); % get dates that user writes in End date.
if isempty(End_Date) % condition: if field is empty then...
    error=1; % set error flag.
    set(handles.text7,'String', 'נא להכניס תאריך סיום'); % write in text7 the string.

end

try  % checks if the string that was inserted as end date is in correct format. if not then jumps to catch.
   D = datenum(End_Date,'dd/mm/yyyy')
catch
    error=1; % set error flag.
   set(handles.text7,'String', 'פורמט לא תקין נא להכניס תאריך בפורמט dd/mm/yyyy'); % write in text7 the string.

end

if isempty(get(handles.edit3,'String')) % condition: if the field is empty then...
        error=1; % set error flag.
        set(handles.text8,'String','נא להכניס קובץ'); % write in text8 the string.
end

    tbl = readtable(get(handles.edit3,'String')); % get the excel data in tbl from the edit3 string.

    tbl(tbl.Date<=datetime(Start_Date,'InputFormat','dd/MM/yyyy'),:)=[];  % Delete what's before start date.
    tbl(tbl.Date>=datetime(End_Date,'InputFormat','dd/MM/yyyy'),:)=[];  % Delete what's after end date.
    Date=tbl.Date;
    if isempty(Date) % this condition checks if start date is higher or equal than end date.
     error=1; % set error flag.
     set(handles.text7,'String', 'נא להכניס תאריך סיום גבוה מתאריך התחלה'); % write in text7 the string.
    end


if ~error  % checks the error flag.
    set(handles.text6,'String', ''); % delete the string in text6 (if exists).
    set(handles.text7,'String', ''); % delete the string in text7 (if exists).
    set(handles.text8,'String', ''); % delete the string in text8 (if exists).

    Main  % go to Main program

    if res % checks the result from the main program.              set(handles.text2,'String','יש דישדוש!', 'ForegroundColor' , 'g'); % writes in text2 the string in green color.
    else
       set(handles.text2,'String','אין דישדוש', 'ForegroundColor' , 'r'); % writes in text2 the string in red color.
    end
end




function edit1_Callback(hObject, eventdata, handles)
% hObject    handle to edit1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit1 as text
%        str2double(get(hObject,'String')) returns contents of edit1 as a double


% --- Executes during object creation, after setting all properties.
function edit1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit2_Callback(hObject, eventdata, handles)
% hObject    handle to edit2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit2 as text
%        str2double(get(hObject,'String')) returns contents of edit2 as a double


% --- Executes during object creation, after setting all properties.
function edit2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');



end


% --- If Enable == 'on', executes on mouse press in 5 pixel border.
% --- Otherwise, executes on mouse press in 5 pixel border or over text2.
function text2_ButtonDownFcn(hObject, eventdata, handles)
% hObject    handle to text2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



function edit3_Callback(hObject, eventdata, handles)
% hObject    handle to edit3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit3 as text
%        str2double(get(hObject,'String')) returns contents of edit3 as a double


% --- Executes during object creation, after setting all properties.
function edit3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton2.
function pushbutton2_Callback(hObject, eventdata, handles) % 'select file' push button function
% hObject    handle to pushbutton2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
[filename1,filepath1]=uigetfile({'*.xlsx;*.csv;*.txt;'},...
  'Select Data File'); % Open a window for select a file
  set(handles.edit3,'String',[filepath1,filename1] ); % insert the path and the name of the selected file
