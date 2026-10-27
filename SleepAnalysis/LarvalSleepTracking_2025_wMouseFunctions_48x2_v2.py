import sys
import serial
sys.path.append("C:\\opencv\\build\\python\\2.7")
import copy
import numpy as np
import cv2
import math
import time
from time import strftime
import os
from numpy import array
import xlwt
import xlrd
import datetime
import serial
import array as arr
from Test2_Sleep48x2 import Excel_Save
from pygame import mixer # Load the required library
import pygame
import gc
import matplotlib.pyplot as plt
import pandas as pd
import tkinter, tkinter.filedialog
from tkinter import *
from tkinter import filedialog



font = cv2.FONT_HERSHEY_SIMPLEX
connected = False
ser = serial.Serial("COM3", 9600, writeTimeout = 0)
while connected == False:
    print ('Initialising Arduino...')
    ser_in = ser.read()
    connected = True
ser.write(str.encode('4'))  # Turn on the white light
pygame.mixer.init()
effect = pygame.mixer.Sound('C:\\Users\\CWLab\\Desktop\\Frequency_30Hz_SineWave.wav')


def nothing(x):
    pass

temp9c = 0
gc.collect()
#Experiment parameters ================================
NumberOfHours = 144 # IF YOU CHANGE THIS PLS CHANGE IT BACK THANKS // 2 means 1 hour and 48 means 1 complete cycle of 24 hours.
set_frame_rate= 5 #Choose from the following 10, 20, 30
Constant_darkness = 0 # 1 means constant darkness  0 means normal light cycles
LightOnDay = 3 # If constant darkenss, specify after how many days the light should be turned on. Two (2) means the next next day.
FrameWidth = 3200 # 3200 for the new sleep rig
FrameHeight = 2048  # 2048 for the new sleep rig
TankWidth = 110 # in mm48
LightOffTime = '230000'
LightOnTime = '090000'
LightIndex = 0 # 0 = light On 1 = Light Off
FishThreshold = 36
SmallestSize = 25
LargestSize = 2500
GB1 = 27
tempQQ = 0  # Minute offset for testing sound
UpdateTime = 300 # Update the background every x number of frames
ShowPanels = 1
## Tank1 ======================================
## Details for saving file
FishName1 = 'yolk-pos'
DOB1 = '22-01-2026'
FishType1 = 'mpeg-gfp-lysc-dsred'
FishSex1 = 'TBD'
#=============================================
Well_ID = 1

## Tank2 ======================================
## Details for saving file
FishName2 = 'yolk-neg'
DOB2 = '22-01-2026'
FishType2 = 'mpeg-gfp-lysc-dsred'
FishSex2 = 'TBD'
#=============================================


# while not connected:
#    serin = ser.read()
#    connected = True

stamp = np.zeros((100,200,3))
NumberOfFish = input("Enter Number of Wells: ")
NumberOfFish = int(NumberOfFish)
GroupName = input("Enter Group Name: ")

CurrentDirectory = "D:\\Sleep_Rig\\Excel\\"
ROIFileName = filedialog.askopenfilename(initialdir=CurrentDirectory, title = "Select a setting CSV file",filetypes = (('CSV files',"*.csv"),("All files","*.*")))
if len(ROIFileName) > 0:
    ROIXY1=pd.read_csv(ROIFileName, index_col=None)
    ROIXY1 = ROIXY1.iloc[0:len(ROIXY1),1:5]
    for i in range(4):
        tempA = ROIXY1.iloc[:,i]
        NewTempA = pd.to_numeric(tempA, downcast='signed')
        if i == 0:
            ROIXY = NewTempA
        else:
            ROIXY = pd.concat([ROIXY, NewTempA], join = 'outer', axis = 1)
    ROIXY = ROIXY.values.tolist()
    ROIXY = np.array(ROIXY)
    ProtectionState = 1
    tempROI2 = 1
else:
    ROIXY = np.zeros((96, 4))
    tempROI = 0
    tempROI2 = 0
    ProtectionState = 0
save1=0
setupbox = 0
Center = []
tempSave = 0
tt801 = "D:\\Sleep_Rig\\Video\\" + GroupName + "\\"
tt802 = "D:\\Sleep_Rig\\Excel\\" + GroupName + "\\"
tt81 = "D:\\Sleep_rig\\Video\\" + GroupName
cv2.namedWindow('frame')
tt9652 = 0

# Camera Setting ====================================
# Capture image wirh specified size and frame rate
cap = cv2.VideoCapture(0)
cap.set(5,set_frame_rate)  # number 5 is frame rate
cap.set(3,FrameWidth)  # number 3 is frame width
cap.set(4,FrameHeight)  # nu    mber 4 is frame height

# Assign the fourcc words and initialize the video container
fourcc1 = cv2.VideoWriter_fourcc('M','J','P','G')  # MJPG, DIVX, XVID,
fourcc2 = cv2.VideoWriter_fourcc('D','I','V','X')  # MJPG, DIVX, XVID,
fourcc3 = cv2.VideoWriter_fourcc('X','V','I','D')  # MJPG, DIVX, XVID,
RGB = 0

#while ser.read() != '5':
#    ser.read
#    print ('Waiting')


#  Below is the mouse evemt

boxes=[]
boxes1=[]
VibrationStatus = 0
ValveIndex1 = 0
ValveIndex2 = 0

drawing = False
dummy= 0
preset=0
preset1=0
tt91=0
tt92=0
tempHour = 0
tempMinute = 0
tempIntensity = 0
tt9651 = 0 # toggle for vibration
tt9652 = 0 # toggle for light
tt9653 = 0 # toggle for vibration
tt9654 = 0 # toggle for saving data
tt9655 = 0 # Integer for saving data

data1=[];
data1Stamp=[];
data2=[];
data2Stamp=[];
data3=[];
data4=[];
data5=[];
data6=[];
data7=[];
data8=[];
data9=[];
data10=[];
data11=[];
data12=[];
data13=[];
data14=[];
data15=[];
data16=[];
data17=[];
data18=[];
data19=[];
data20=[];
data21=[];
data22=[];
data23=[];
data24=[];
data25=[];
data26=[];
data27=[];
data28=[];
data29=[];
data30=[];
data31=[];
data32=[];
data33=[];
data34=[];
data35=[];
data36=[];
data37=[];
data38=[];
data39=[];
data40=[];
data41=[];
data42=[];
data43=[];
data44=[];
data45=[];
data46=[];
data47=[];
data48=[];
data49=[];
data50=[];
data51=[];
data52=[];
data53=[];
data54=[];
data55=[];
data56=[];
data57=[];
data58=[];
data59=[];
data60=[];
data61=[];
data62=[];
data63=[];
data64=[];
data65=[];
data66=[];
data67=[];
data68=[];
data69=[];
data70=[];
data71=[];
data72=[];
data73=[];
data74=[];
data75=[];
data76=[];
data77=[];
data78=[];
data79=[];
data80=[];
data81=[];
data82=[];
data83=[];
data84=[];
data85=[];
data86=[];
data87=[];
data88=[];
data89=[];
data90=[];
data91=[];
data92=[];
data93=[];
data94=[];
data95=[];
data96=[];
tempAA3A = 0
i=0
mode = 0
StartDate = strftime("%y",) + strftime("%m",) + strftime("%d",)
StartDate1 = strftime("%d",)
if Constant_darkness == 1:
    LightOnDay = int(StartDate1) + LightOnDay

def on_change(value):
    pass
# def on_change1(value):
#     boxes[0][0] = cv2.getTrackbarPos('P1-X', 'frame')
#     boxes[0][1] = cv2.getTrackbarPos('P1-Y', 'frame')
#     for i in range(1, NumberOfFish + 1, 1):
#         cv2.rectangle(gray3, (boxes[0 + (i - 1) * 2][0], boxes[0 + (i - 1) * 2][1]),(boxes[1 + (i - 1) * 2][0], boxes[1 + (i - 1) * 2][1]), (0, 0, 255), 3)
#         cv2.circle(gray3, (Center[i - 1][0], Center[i - 1][1]), Radius1, (255, 0, 0), 2)
#         cv2.putText(gray2, str('%02d' % i), (boxes[0 + (i - 1) * 2][0] + 2, boxes[0 + (i - 1) * 2][1] + 25), font, 1,(0, 0, 0), 1, cv2.LINE_AA)

temp4 = 0
WellNumber = 0
def set_up_ROIs(event,x,y,flags,param):
    if len(boxes1) <= 4:
        global boxnumber, drawing, img, gray2, gray3, arr, arr1, dummy, mode, Radius1, Radius2, gray5, gray51a, gray51b, gray51aMin, gray51bMin, Column1ShiftX, Column1ShiftY, Column1ShiftR, tempROI, ROIXY
        if event == cv2.EVENT_LBUTTONDOWN:
            drawing = True
            sbox = [x, y]
            boxes1.append(sbox)
        elif event == cv2.EVENT_MOUSEMOVE:
            if drawing == True:
                if len(boxes1) == 1:
                    arr = array(gray2)
                    cv2.rectangle(arr,(boxes1[-1][0],boxes1[-1][-1]),(x, y),(0,0,255),3)
                    cv2.imshow('frame',arr)
                if len(boxes1) == 3:
                    arr = array(gray3)
                    cv2.rectangle(arr, (boxes1[-1][0], boxes1[-1][-1]), (x, y), (0, 0, 255), 3)
                    cv2.imshow('frame', arr)
        elif event == cv2.EVENT_LBUTTONUP:
            drawing = not drawing
            ebox = [x, y]
            boxes1.append(ebox)
            if len(boxes1) == 2 or len(boxes1) == 4:
                if len(boxes1) == 4:
                    cv2.rectangle(gray3, (boxes1[-2][-2], boxes1[-2][-1]), (boxes1[-1][-2], boxes1[-1][-1]),(0, 0, 255), 3)
                    cv2.imshow('frame', gray3)
                    mode = 1
                    dummy = 1
                    i = 0
                elif len(boxes1) == 2:
                    cv2.rectangle(gray2, (boxes1[-2][-2], boxes1[-2][-1]), (boxes1[-1][-2], boxes1[-1][-1]),(0, 0, 255), 3)
                    cv2.imshow('frame', gray2)
                    gray3 = copy.copy(frame)
                if not os.path.exists(tt801):
                    os.makedirs(tt81)
                if not os.path.exists(tt802):
                    os.makedirs(tt802)
                tempXa = boxes1[-1][-2]-boxes1[-2][-2]
                tempYa = boxes1[-1][-1]-boxes1[-2][-1]
                tempXb = tempXa - (tempXa % 6)
                tempYb = tempYa - (tempYa % 8)
                tempXc = tempXb / 6
                tempYc = tempYb / 8
                tempZ=0

                for bby in range(8):
                    for bbx in range(6):
                        sbox = [int(boxes1[-2][-2] + bbx * tempXc),int(boxes1[-2][-1] + bby * tempYc)]
                        boxes.append(sbox)
                        ebox = [int(boxes1[-2][-2] + (bbx+1) * tempXc-1),int(boxes1[-2][-1] + (bby+1) * tempYc-1)]
                        boxes.append(ebox)
                        tempZ = tempZ + 1
                        cv2.rectangle(gray3, (boxes[-2][-2], boxes[-2][-1]), (boxes[-1][-2], boxes[-1][-1]),(0, 0, 255), 3)
                if len(boxes) == 96:
                    for mm in range(0, len(boxes), 2):
                        Center.append([int((boxes[mm + 1][0] - boxes[mm][0]) / 2) + boxes[mm][0], int((boxes[mm + 1][1] - boxes[mm][1]) / 2) + boxes[mm][1]])
                        if tempROI2 == 0:
                            ROIXY[tempROI, 0] = int((boxes[mm + 1][0] - boxes[mm][0]) / 2) + boxes[mm][0]  # x
                            ROIXY[tempROI, 1] = int((boxes[mm + 1][1] - boxes[mm][1]) / 2) + boxes[mm][1]  # y
                            tempROI = tempROI + 1
                    if tempROI2 == 0:
                        for mm in range(48):
                            ROIXY[mm, 2] = 85 # r
                            ROIXY[mm, 3] = FishThreshold  # FishThreshold
                    Radius1 = 85
                    for mm in range(1, NumberOfFish + 1, 1):
                        cv2.circle(gray3, (Center[mm - 1][0], Center[mm - 1][1]), Radius1, (255, 0, 0), 2)
                        Column1ShiftX = 8
                        Column1ShiftR = 0
                        cv2.putText(gray3, str('%02d' % mm), (boxes[0 + (mm - 1) * 2][0] + 2, boxes[0 + (mm - 1) * 2][1] + 25), font, 1, (0, 0, 0), 1, cv2.LINE_AA)
                    gray51a = np.zeros((48, int(boxes[1][1] - boxes[0][1] + 1)-1, int(boxes[1][0] - boxes[0][0] + 1)-1))
                    gray51aMin = np.zeros(48)
                elif len(boxes) == 192:
                    for mm in range(96, len(boxes), 2):
                        Center.append([int((boxes[mm + 1][0] - boxes[mm][0]) / 2) + boxes[mm][0], int((boxes[mm + 1][1] - boxes[mm][1]) / 2) + boxes[mm][1]])
                        if tempROI2 == 0:
                            ROIXY[tempROI, 0] = int((boxes[mm + 1][0] - boxes[mm][0]) / 2) + boxes[mm][0]  # x
                            ROIXY[tempROI, 1] = int((boxes[mm + 1][1] - boxes[mm][1]) / 2) + boxes[mm][1]  # y
                            tempROI = tempROI + 1
                    if tempROI2 == 0:
                        for mm in range(48):
                            ROIXY[48+mm, 2] = 85 # r
                            ROIXY[48+mm, 3] = FishThreshold  # FishThreshold
                        # print(ROIXY)
                    Radius2 = 85
                    for mm in range(NumberOfFish+ 1, NumberOfFish*2 + 1, 1):
                        cv2.circle(gray3, (Center[mm - 1][0], Center[mm - 1][1]), Radius2, (255, 0, 0), 2)
                        cv2.putText(gray3, str('%02d' % mm), (boxes[0 + (mm - 1) * 2][0] + 2, boxes[0 + (mm - 1) * 2][1] + 25), font, 1, (0, 0, 0), 1, cv2.LINE_AA)
                    gray5a = cv2.cvtColor(gray5, cv2.COLOR_BGR2GRAY)
                    gray5a = np.float32(gray5a)
                    temp9 = 0
                    gray51b = np.zeros((48, int(boxes[97][1] - boxes[96][1] + 1)-1, int(boxes[97][0] - boxes[96][0] + 1)-1))
                    gray51bMin = np.zeros(48)
                    for i in range(1, NumberOfFish * 2+1, 1):
                        if i <= 48:
                            gray51a[i-1, :, :] = gray5a[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1],boxes[0+(i-1)*2][0]:boxes[1+(i-1)*2][0]]
                            gray51aMin[i-1] = np.ma.min(gray51a[i-1,:,:])
                        else:
                            gray51b[i-49, :, :] = gray5a[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1],boxes[0+(i-1)*2][0]:boxes[1+(i-1)*2][0]]
                            gray51bMin[i-49] = np.ma.min(gray51b[i-49, :, :])
                    gray5 = cv2.convertScaleAbs(gray5a)
                cv2.imshow('frame', gray3)
                temp9c = 1
    else:
        global temp4, ClickX, ClickY, WellNumber

        if event == cv2.EVENT_RBUTTONDOWN:  # Codes below are executed only when the mouse is moving on the designated window
            WellNumber = 0
        elif event == cv2.EVENT_LBUTTONDOWN:  # Codes below are executed only when the mouse is moving on the designated window
            temp9a, temp9b = gray2.shape
            if x <= int(temp9b/2):
                temp3a = 0
                for ii in range(6):
                    if ROIXY[ii, 0] - Radius1 < x:
                        if ROIXY[ii+1,0]-Radius1 > x:
                            temp3a = ii+1
                        elif ii == 5:
                            temp3a = 6
                temp3b = 0
                for ii in range(8):
                    if ROIXY[ii*6, 1] - Radius1 < y:
                        if ROIXY[(ii+1)*6 + 1, 1] - Radius1 > y:
                            temp3b = ii + 1
                        elif ii == 7:
                            temp3b = 8
                WellNumber = (temp3b - 1) * 6 + temp3a
            else:
                temp3a = 0
                for ii in range(6):
                    if ROIXY[ii+48, 0] - Radius1 < x:
                        if ROIXY[ii+48+1,0]-Radius1 > x:
                            temp3a = ii+1
                        elif ii == 5:
                            temp3a = 6
                temp3b = 0
                for ii in range(8):
                    if ROIXY[ii*6+48, 1] - Radius1 < y:
                        if ii == 7:
                            temp3b = 8
                        elif ROIXY[(ii+1)*6+48+1, 1] - Radius1 > y:
                            temp3b = ii + 1
                WellNumber = (temp3b-1)*6 + temp3a+48
            Well_ID = WellNumber
            cv2.setTrackbarPos("Protection", 'Setting', 1)
            cv2.setTrackbarPos("Well_ID", 'Setting', WellNumber)
            cv2.setTrackbarMin('X', 'Setting', int(ROIXY[Well_ID - 1, 0]) - 40)
            cv2.setTrackbarMax('X', 'Setting', int(ROIXY[Well_ID - 1, 0]) + 40)
            cv2.setTrackbarPos("X", 'Setting', int(ROIXY[Well_ID - 1, 0]))
            cv2.setTrackbarMin('Y', 'Setting', int(ROIXY[Well_ID - 1, 1]) - 40)
            cv2.setTrackbarMax('Y', 'Setting', int(ROIXY[Well_ID - 1, 1]) + 40)
            cv2.setTrackbarPos("Y", 'Setting', int(ROIXY[Well_ID - 1, 1]))
            cv2.setTrackbarMin("Radius", 'Setting', int(ROIXY[Well_ID-1, 2])-20)
            cv2.setTrackbarMax("Radius", 'Setting', int(ROIXY[Well_ID-1, 2])+20)
            cv2.setTrackbarPos("Radius", 'Setting', int(ROIXY[Well_ID-1, 2]))
            cv2.setTrackbarMin("Threshold", 'Setting', int(ROIXY[Well_ID-1, 3])-20)
            cv2.setTrackbarMax("Threshold", 'Setting', int(ROIXY[Well_ID-1, 3])+20)
            cv2.setTrackbarPos("Threshold", 'Setting', int(ROIXY[Well_ID-1, 3]))
            ClickX = x
            ClickY = y
            temp4 = 1
        elif event == cv2.EVENT_MOUSEMOVE:
            if temp4 == 1:
                ROIXY[WellNumber - 1][0] = ROIXY[WellNumber - 1][0] - (ClickX - x)
                ROIXY[WellNumber - 1][1] = ROIXY[WellNumber - 1][1] - (ClickY - y)
                ClickX = x
                ClickY = y
        elif event == cv2.EVENT_LBUTTONUP:
            temp4 = 0
        elif event == cv2.EVENT_MOUSEWHEEL:
            if flags > 0:
                ROIXY[WellNumber - 1][2] = ROIXY[WellNumber - 1][2] - 1
            else:
                ROIXY[WellNumber - 1][2] = ROIXY[WellNumber - 1][2] + 1
# ====================================================

kernel = np.ones((5, 5), np.uint8)
ret, gray4 = cap.read()
gray7 = copy.deepcopy(gray4)
gray4 = np.float32(gray4)
gray7 = np.float32(gray7)
BackUpdate = 0
j = 0
while(int(tt91)<NumberOfHours):  # check !
    # capture frame-by-frame
    CurrentTime1 = strftime("%H",) + strftime("%M",) + strftime("%S",)
    if CurrentTime1 == LightOffTime:
        if LightIndex == 0:
            ser.write(str.encode('3'))  # Turn off the white light
            LightIndex = 1
            tt9652 = 3
            print ('The light was turned off at ...' + CurrentTime1)
            BackUpdate = 290
    if CurrentTime1 == LightOnTime:
        if Constant_darkness == 1:
            if int(strftime("%d",)) == LightOnDay:
                if LightIndex == 1:
                    ser.write(str.encode('4'))  # Turn on the white light
                    LightIndex = 0
                    print ('The light was turned on at ...' + CurrentTime1)
            else:
                print ('The light was not turned on at ...' + CurrentTime1)
                print ('This is a constant darkness protocol')

        else:
            if LightIndex == 1:
                ser.write(str.encode('4'))  # Turn on the white light
                LightIndex = 0
                tt9652 = 0
                print ('The light was turned on at ...' + CurrentTime1)


    if LightIndex == 1:   # LightIndex == 1 means the following startle protocol will take place during midnight. LightIndex == 0 means the same protocol will take place during day time.
        # Remember to change it to LightIndex == 1 for midnight testing.
        tt224 = strftime("%H", )
        tt225 = strftime("%M", )
        tt226 = strftime("%S", )
        if (int(tt224) > 0 and int(tt224) <= 6):
            if int(tt224) % 2 == 0:  # Even Hour
                if int(tt225) == 0:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.0)  # Intensity level for this first minute
                            effect.play(maxtime=200)
                            print ('222 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 1:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.12)  # Intensity level for this 2nd minute
                            effect.play(maxtime=200)
                            print ('223 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 2:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.24)
                            effect.play(maxtime=200)
                            print ('224 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            # effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 3:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.36)
                            effect.play(maxtime=200)
                            print ('225 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 4:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.48)
                            effect.play(maxtime=200)
                            print ('226 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 5:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.60)
                            effect.play(maxtime=200)
                            print ('227 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 6:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.72)
                            effect.play(maxtime=200)
                            print ('228 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 7:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.84)
                            effect.play(maxtime=200)
                            print ('229 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 8:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(1.00)
                            effect.play(maxtime=200)
                            print ('230 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 9:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(1.00)
                            effect.play(maxtime=200)
                            print ('230 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 10:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.84)
                            effect.play(maxtime=200)
                            print ('229 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 11:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.72)
                            effect.play(maxtime=200)
                            print ('228 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 12:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.60)
                            effect.play(maxtime=200)
                            print ('227 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 13:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.48)
                            effect.play(maxtime=200)
                            print ('226 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 14:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.36)
                            effect.play(maxtime=200)
                            print ('225 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 15:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.24)
                            effect.play(maxtime=200)
                            print ('224 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 16:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.12)
                            effect.play(maxtime=200)
                            print ('223 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 17:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.00)
                            effect.play(maxtime=200)
                            print ('222 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
            else:  # Odd hour
                if int(tt225) == 0 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(1.0)
                            effect.play(maxtime=200)
                            print ('230 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 1 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.84)
                            effect.play(maxtime=200)
                            print ('229 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 2 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.72)
                            effect.play(maxtime=200)
                            print ('228 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 3 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.60)
                            effect.play(maxtime=200)
                            print ('227 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 4 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.48)
                            effect.play(maxtime=200)
                            print ('226 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 5 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.36)
                            effect.play(maxtime=200)
                            print ('225 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 6 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.24)
                            effect.play(maxtime=200)
                            print ('224 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 7 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.12)
                            effect.play(maxtime=200)
                            print ('223 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 8 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.00)
                            effect.play(maxtime=200)
                            print ('222 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 9 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.00)
                            effect.play(maxtime=200)
                            print ('222 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 10 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.12)
                            effect.play(maxtime=200)
                            print ('223 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 11 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.24)
                            effect.play(maxtime=200)
                            print ('224 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 12 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.36)
                            effect.play(maxtime=200)
                            print ('225 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 13 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.48)
                            effect.play(maxtime=200)
                            print ('226 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 14 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.60)
                            effect.play(maxtime=200)
                            print ('227 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 15 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.72)
                            effect.play(maxtime=200)
                            print ('228 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 16 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(0.84)
                            effect.play(maxtime=200)
                            print ('229 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0
                if int(tt225) == 17 + tempQQ:
                    if int(tt226) == 0:
                        if VibrationStatus == 0:
                            effect.set_volume(1.00)
                            effect.play(maxtime=200)
                            print ('230 at ' + CurrentTime1)
                            VibrationStatus = 1
                            tt9653 = 1
                    if int(tt226) == 1:
                        if VibrationStatus == 1:
                            effect.stop
                            tt9653 = 0
                            VibrationStatus = 0

    if (int(CurrentTime1) >= int(LightOnTime)):
        tt224 = strftime("%H", )
        tt225 = strftime("%M", )
        if int(tt224) > 16:
            if int(tt224) < 23:
                if int(tt225) >= 30:  # UV Array is ON at 20
                    if tt9652 == 0:
                        ser.write(str.encode('3'))  # Turn off the white light
                        if LightIndex == 0:
                            LightIndex = 1
                        tt9652 = 1
                        print('The light was turned off at ...' + CurrentTime1)
                        BackUpdate = 290
                else:  # UV Array is ON at 20:
                    if int(tt224) < 23:
                        if tt9652 == 1:
                            if LightIndex == 1:
                                tt9652 = 0
                                LightIndex = 0
                                print('Light is On at ' + CurrentTime1)
                                ser.write(str.encode('4'))

    ret, frame = cap.read()
    gray2 = frame[:]
    gray6 = copy.deepcopy(frame)
    gray9 = copy.deepcopy(frame)
    # gray10 = copy.deepcopy(frame)
    # gray7 = np.float32(gray6)
    # gray7 = copy.deepcopy(frame)

    if dummy == 1:
        if setupbox == 0:
            starttime = datetime.datetime.now()
            frametime = starttime
            setupbox = 1
            if RGB == 1:
                #Below is for RGB
                Tank1= cv2.VideoWriter(tt81 + '\\Tank1.avi',fourcc, 5.0, (boxes[1][0]-boxes[0][0],boxes[1][1]-boxes[0][1]))
            else:
                #Below is for B/W
                # print(int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/2)+1)
                # print(int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/2)+1)
                # print('+++++')
                # Tank1 = cv2.VideoWriter(tt81 + '\\Tank1_Files_001.avi',fourcc2, 5.0, (int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/2)+1,int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/2)+1),0)
                # Tank1a = cv2.VideoWriter(tt81 + '\\Tank1_Raw_Files_001.avi', fourcc2, 5.0, (int((boxes[NumberOfFish*2-1][0] - boxes[0][0]) / 2) + 1,int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) / 2) + 1), 0)
                # Tank2 = cv2.VideoWriter(tt81 + '\\Tank2_Files_001.avi',fourcc2, 5.0, (int((boxes[NumberOfFish*4-1][0]-boxes[NumberOfFish*2][0])/2)+1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1])/2)+1),0)
                # Tank2a = cv2.VideoWriter(tt81 + '\\Tank2_Raw_Files_001.avi', fourcc2, 5.0, (int((boxes[NumberOfFish*4-1][0] - boxes[NumberOfFish*2][0]) / 2) + 1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1]) / 2) + 1), 0)
                # Tank1 = cv2.VideoWriter(tt81 + '\\Tank1_Files_001.avi',fourcc2, 5.0, (int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/1)+1,int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/1)+1),0)
                Tank1a = cv2.VideoWriter(tt81 + '\\Tank1_Raw_Files_001.avi', fourcc1, 5.0, (int((boxes[NumberOfFish*2-1][0] - boxes[0][0]) /2) + 1, int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) /2) + 1), 0)
                Tank1b = cv2.VideoWriter(tt81 + '\\Tank1_Color_Files_001.avi', fourcc2, 5.0, (int((boxes[NumberOfFish*2-1][0] - boxes[0][0]) /3) + 1, int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) /3) + 1))
                # Tank2 = cv2.VideoWriter(tt81 + '\\Tank2_Files_001.avi',fourcc2, 5.0, (int((boxes[NumberOfFish*4-1][0]-boxes[NumberOfFish*2][0])/1)+1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1])/1)+1),0)
                Tank2a = cv2.VideoWriter(tt81 + '\\Tank2_Raw_Files_001.avi', fourcc1, 5.0, (int((boxes[NumberOfFish*4-1][0] - boxes[NumberOfFish*2][0]) /2) + 1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1]) /2) + 1), 0)
                Tank2b = cv2.VideoWriter(tt81 + '\\Tank2_Color_Files_001.avi', fourcc2, 5.0, (int((boxes[NumberOfFish*4-1][0] - boxes[NumberOfFish*2][0]) /3) + 1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1]) /3) + 1))



    if mode==1:
        if j == 1:
            StartTime = strftime("%H", ) + strftime("%M", ) + strftime("%S", )
            print('The experiment started from ...ymd & hms ____' + StartDate + ' & ' + StartTime)
        j = j + 1
        for i in range(1, NumberOfFish + 1, 1):
            cv2.rectangle(gray2,(boxes[0+(i-1)*2][0],boxes[0+(i-1)*2][1]),(boxes[1+(i-1)*2][0],boxes[1+(i-1)*2][1]), (0,0,255),3)
            if len(Center) > 0:
                # cv2.circle(gray2, (Center[i-1][0], Center[i-1][1]), Radius1, (255, 0, 0), 2)
                # cv2.circle(gray2, (int(ROIXY[i-1,0]), int(ROIXY[i-1,1])), int(ROIXY[i-1,2]), (255, 0, 0), 2)
                if i % 6 == 1:
                    Column1ShiftY = 2 + (i // 6) * 2
                    # cv2.circle(gray2, (Center[i-1][0]+Column1ShiftX, Center[i-1][1]-Column1ShiftY), Radius1-Column1ShiftR, (255, 0, 255), 2)
                    # cv2.circle(gray2, (Center[i - 1][0] + Column1ShiftX, Center[i - 1][1] - Column1ShiftY),1, (255, 0, 255), 2)
            cv2.putText(gray2, str('%02d'%i), (boxes[0+(i-1)*2][0]+2, boxes[0+(i-1)*2][1]+25), font, 1, (0, 0, 0), 1, cv2.LINE_AA)
        for i in range(NumberOfFish+1, NumberOfFish*2 + 1, 1):
            cv2.rectangle(gray2,(boxes[0+(i-1)*2][0],boxes[0+(i-1)*2][1]),(boxes[1+(i-1)*2][0],boxes[1+(i-1)*2][1]), (0,0,255),3)
            # if len(Center) > 0:
                # cv2.circle(gray2, (Center[i-1][0], Center[i-1][1]), Radius2, (255, 0, 0), 2)
                # cv2.circle(gray2, (int(ROIXY[i - 1, 0]), int(ROIXY[i - 1, 1])), int(ROIXY[i - 1, 2]), (255, 0, 0), 2)
            cv2.putText(gray2, str('%02d'%i), (boxes[0+(i-1)*2][0]+2, boxes[0+(i-1)*2][1]+25), font, 1, (0, 0, 0), 1, cv2.LINE_AA)
        tt224 = strftime("%H", )
        tt225 = strftime("%M", )
        tt226 = strftime("%S", )
        if int(tt225) == 25 or int(tt225) == 55:  # save the data twice in an hour
            if tt9654 == 0:
                tt9654 = 1
                tt9655 = tt9655+1
                CurrentTime = strftime("%H",) + strftime("%M",) + strftime("%S",)
                tt91 = '%03d' % int(tt9655)
                tt92 = '%03d' % int(tt9655+1)
                print ('Files_' + tt91 + ' were saved at ' + CurrentTime)
                tt99 = Excel_Save(tt91, GroupName, boxes, data1, data1Stamp, data2, data2Stamp, data3, data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24,data25,data26,data27,data28,data29,data30,data31,data32,data33,data34,data35,data36,data37,data38,data39,data40,data41,data42,data43,data44,data45,data46,data47,data48,data49,data50,data51,data52,data53,data54,data55,data56,data57,data58,data59,data60,data61,data62,data63,data64,data65,data66,data67,data68,data69,data70,data71,data72,data73,data74,data75,data76,data77,data78,data79,data80,data81,data82,data83,data84,data85,data86,data87,data88,data89,data90,data91,data92,data93,data94,data95,data96, Tank1a, Tank1b, Tank2a, Tank2b, FishName1,DOB1,FishType1,FishSex1,FishName2,DOB2,FishType2,FishSex2,FrameWidth,FrameHeight,TankWidth,save1)
                if int(tt91)<NumberOfHours:
                    # Tank1  = cv2.VideoWriter(tt81 + '\\Tank1' + '_Files_' + tt92 + '.avi',fourcc2, 5, (int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/2)+1,int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/2)+1), 0)
                    # Tank1a = cv2.VideoWriter(tt81 + '\\Tank1' + '_Raw_Files_' + tt92 + '.avi', fourcc2, 5, (int((boxes[NumberOfFish * 2 - 1][0] - boxes[0][0]) / 2) + 1,int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) / 2) + 1), 0)
                    # Tank2  = cv2.VideoWriter(tt81 + '\\Tank2' + '_Files_' + tt92 + '.avi',fourcc2, 5, (int((boxes[NumberOfFish*4-1][0]-boxes[NumberOfFish*2][0])/2)+1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1])/2)+1), 0)
                    # Tank2a = cv2.VideoWriter(tt81 + '\\Tank2' + '_Raw_Files_' + tt92 + '.avi', fourcc2, 5, (int((boxes[NumberOfFish * 4 - 1][0] - boxes[NumberOfFish*2][0]) / 2) + 1,int((boxes[NumberOfFish * 4 - 1][1] - boxes[NumberOfFish*2][1]) / 2) + 1), 0)
                    # Tank1  = cv2.VideoWriter(tt81 + '\\Tank1' + '_Files_' + tt92 + '.avi',fourcc2, 5, (int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/1)+1,int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/1)+1), 0)
                    Tank1a = cv2.VideoWriter(tt81 + '\\Tank1' + '_Raw_Files_' + tt92 + '.avi', fourcc1, 5, (int((boxes[NumberOfFish * 2 - 1][0] - boxes[0][0]) /2) + 1,int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) /2) + 1), 0)
                    Tank1b = cv2.VideoWriter(tt81 + '\\Tank1' + '_Color_Files_' + tt92 + '.avi', fourcc2, 5, (int((boxes[NumberOfFish * 2 - 1][0] - boxes[0][0]) /3) + 1,int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) /3) + 1), 1)
                    # Tank2  = cv2.VideoWriter(tt81 + '\\Tank2' + '_Files_' + tt92 + '.avi',fourcc2, 5, (int((boxes[NumberOfFish*4-1][0]-boxes[NumberOfFish*2][0])/1)+1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1])/1)+1), 0)
                    Tank2a = cv2.VideoWriter(tt81 + '\\Tank2' + '_Raw_Files_' + tt92 + '.avi', fourcc1, 5, (int((boxes[NumberOfFish * 4 - 1][0] - boxes[NumberOfFish*2][0]) /2) + 1,int((boxes[NumberOfFish * 4 - 1][1] - boxes[NumberOfFish*2][1]) /2) + 1), 0)
                    Tank2b = cv2.VideoWriter(tt81 + '\\Tank2' + '_Color_Files_' + tt92 + '.avi', fourcc2, 5, (int((boxes[NumberOfFish * 4 - 1][0] - boxes[NumberOfFish * 2][0]) /3) + 1,int((boxes[NumberOfFish * 4 - 1][1] - boxes[NumberOfFish * 2][1]) /3) + 1),1)
                    data1Stamp=[];
                    data2Stamp=[];
                    data1 = [];
                    data2 = [];
                    data3 = [];
                    data4 = [];
                    data5 = [];
                    data6 = [];
                    data7 = [];
                    data8 = [];
                    data9 = [];
                    data10 = [];
                    data11 = [];
                    data12 = [];
                    data13 = [];
                    data14 = [];
                    data15 = [];
                    data16 = [];
                    data17 = [];
                    data18 = [];
                    data19 = [];
                    data20 = [];
                    data21 = [];
                    data22 = [];
                    data23 = [];
                    data24 = [];
                    data25 = [];
                    data26 = [];
                    data27 = [];
                    data28 = [];
                    data29 = [];
                    data30 = [];
                    data31 = [];
                    data32 = [];
                    data33 = [];
                    data34 = [];
                    data35 = [];
                    data36 = [];
                    data37 = [];
                    data38 = [];
                    data39 = [];
                    data40 = [];
                    data41 = [];
                    data42 = [];
                    data43 = [];
                    data44 = [];
                    data45 = [];
                    data46 = [];
                    data47 = [];
                    data48 = [];
                    data49 = [];
                    data50 = [];
                    data51 = [];
                    data52 = [];
                    data53 = [];
                    data54 = [];
                    data55 = [];
                    data56 = [];
                    data57 = [];
                    data58 = [];
                    data59 = [];
                    data60 = [];
                    data61 = [];
                    data62 = [];
                    data63 = [];
                    data64 = [];
                    data65 = [];
                    data66 = [];
                    data67 = [];
                    data68 = [];
                    data69 = [];
                    data70 = [];
                    data71 = [];
                    data72 = [];
                    data73 = [];
                    data74 = [];
                    data75 = [];
                    data76 = [];
                    data77 = [];
                    data78 = [];
                    data79 = [];
                    data80 = [];
                    data81 = [];
                    data82 = [];
                    data83 = [];
                    data84 = [];
                    data85 = [];
                    data86 = [];
                    data87 = [];
                    data88 = [];
                    data89 = [];
                    data90 = [];
                    data91 = [];
                    data92 = [];
                    data93 = [];
                    data94 = [];
                    data95 = [];
                    data96 = [];

                    BackUpdate = 0
                    gray7 = np.float32(gray9)
                    # gray7 = np.float32(gray8)

                    gc.collect()
        else:
            tt9654 = 0
        if BackUpdate < UpdateTime:
            cv2.accumulateWeighted(gray9, gray7, 0.1)
            BackUpdate = BackUpdate + 1
        else:
            if BackUpdate == UpdateTime:
                gray8 = cv2.convertScaleAbs(gray7)
                gray8a = cv2.cvtColor(gray8, cv2.COLOR_BGR2GRAY)
                gray5 = copy.deepcopy(gray8a)

                gray51aMin2 = np.zeros(48)
                gray51a2 = np.zeros((48, int(boxes[1][1] - boxes[0][1] + 1) - 1, int(boxes[1][0] - boxes[0][0] + 1) - 1))
                gray51bMin2 = np.zeros(48)
                gray51b2 = np.zeros((48, int(boxes[97][1] - boxes[96][1] + 1) - 1, int(boxes[97][0] - boxes[96][0] + 1) - 1))
                temp9 = 0
                for i in range(1, NumberOfFish * 2 + 1, 1):
                    if i <= 48:
                        gray51a2[i-1, :, :] = gray5[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1], boxes[0+(i-1)*2][0]:boxes[1+(i-1)*2][0]]
                        gray51aMin2[i-1] = np.ma.min(gray51a2[i-1, :, :])
                        avg_img = np.sum((gray51a2[i-1, :, :], gray51a[i-1, :, :]*3), axis=0)
                        gray51a[i-1, :, :] = avg_img/4
                        gray51aMin[i-1] = np.ma.min(gray51a[i-1, :, :])
                    else:
                        gray51b2[i-49,:,:] = gray5[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1],boxes[0+(i-1)*2][0]:boxes[1+(i-1) * 2][0]]
                        gray51bMin2[i - 49] = np.ma.min(gray51b2[i - 49, :, :])
                        avg_img = np.sum((gray51b2[i-49, :, :], gray51b[i-49, :, :]*3), axis=0)
                        gray51b[i-49, :, :] = avg_img/4
                        gray51bMin[i-49] = np.ma.min(gray51b[i-49, :, :])
                BackUpdate = 0

                if LightIndex == 1:
                    if UpdateTime == 300:
                        UpdateTime = 50
                elif LightIndex == 0:
                    if UpdateTime == 50:
                        UpdateTime = 300

        imgraw = []
        # Below is for Tank 1 tracking =======================================================
        # gray5 - gray6 for tracking fish black eyes
        # gray6 - gray5 for tracking Nacre fish white body
        # print(type(gray5))
        # print(type(gray6))
        # print(gray5.shape)
        # print(gray6.shape)
        gray6 = cv2.cvtColor(gray6, cv2.COLOR_BGR2GRAY)
        for i in range(1,NumberOfFish*2+1,1):
            if i <= 48:
                gray52a = gray51a[i-1,:,:]
                gray52a1 = cv2.convertScaleAbs(gray52a)
                BG = copy.deepcopy(gray52a1)
                # img1 = cv2.subtract(gray5[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1],boxes[0+(i-1)*2][0]:boxes[1+(i-1)*2][0]],gray6[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1],boxes[0+(i-1)*2][0]:boxes[1+(i-1)*2][0]]) # global view
                img1 = cv2.subtract(gray52a1,gray6[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1],boxes[0+(i-1)*2][0]:boxes[1+(i-1)*2][0]]) # global view
                img11 = copy.deepcopy(img1)
                mask1a = cv2.circle(img11, (int((ROIXY[i - 1, 0] + 2) - boxes[(i - 1) * 2][0]), int((ROIXY[i - 1, 1] + 1)) - boxes[(i - 1) * 2][1]),int((ROIXY[i - 1, 2]) + 2), 255, -1)
                mask1b = cv2.subtract(mask1a, img1)
                tempz1, tempz2 = mask1b.shape
                mask1d = np.zeros((tempz1,tempz2))
                mask1d = cv2.convertScaleAbs(mask1d)
                mask1d = cv2.circle(mask1d, (int((ROIXY[i - 1, 0] + 2) - boxes[(i - 1) * 2][0]),int((ROIXY[i - 1, 1] + 1)) - boxes[(i - 1) * 2][1]),int((ROIXY[i - 1, 2]) + 2), 255, -1)
                mask1e = cv2.subtract(mask1d, mask1b)
                img1 = mask1e
            else:
                gray52b = gray51b[i-49,:,:]
                gray52b1 = cv2.convertScaleAbs(gray52b)
                BG = copy.deepcopy(gray52b1)
                img1 = cv2.subtract(gray52b1,gray6[boxes[0+(i-1)*2][1]:boxes[1+(i-1)*2][1],boxes[0+(i-1)*2][0]:boxes[1+(i-1)*2][0]]) # global view
                img11 = copy.deepcopy(img1)
                mask1a = cv2.circle(img11, (int((ROIXY[i - 1, 0] + 2) - boxes[(i - 1) * 2][0]), int((ROIXY[i - 1, 1] + 1)) - boxes[(i - 1) * 2][1]),int((ROIXY[i - 1, 2]) + 2), 255, -1)
                mask1b = cv2.subtract(mask1a, img1)
                tempz1, tempz2 = mask1b.shape
                mask1d = np.zeros((tempz1,tempz2))
                mask1d = cv2.convertScaleAbs(mask1d)
                mask1d = cv2.circle(mask1d, (int((ROIXY[i - 1, 0] + 2) - boxes[(i - 1) * 2][0]),int((ROIXY[i - 1, 1] + 1)) - boxes[(i - 1) * 2][1]),int((ROIXY[i - 1, 2]) + 2), 255, -1)
                mask1e = cv2.subtract(mask1d, mask1b)
                img1 = mask1e
            img1[img1 < 0] = 0
            ret, img1b = cv2.threshold(img1, int(ROIXY[i-1,3]), 255, 0)

            # img1c = cv2.GaussianBlur(img1b, (GB1, GB1), cv2.BORDER_REFLECT_101)  # better
            # img1c = cv2.erode(img1b, kernel, iterations=1)
            img1c = cv2.dilate(img1b, kernel, iterations=3)
            # img1e = cv2.GaussianBlur(img1d, (GB1, GB1), cv2.BORDER_REFLECT_101)  # better
            edged = cv2.Canny(img1c, 10, 100)
            if ShowPanels == 1:
                if i == Well_ID:
                    if Well_ID > 0:
                        CurrentImage = copy.deepcopy(gray2[boxes[0 + (i - 1) * 2][1]:boxes[1 + (i - 1) * 2][1],boxes[0 + (i - 1) * 2][0]:boxes[1 + (i - 1) * 2][0]])
                        CurrentImage1 = cv2.resize(CurrentImage, (0, 0), fx=2.0, fy=2.0,interpolation=cv2.INTER_LINEAR)
                        cv2.imshow('Raw', CurrentImage1)
                        cv2.moveWindow('Raw', 50, 150)
                        BG1 = cv2.resize(BG, (0, 0), fx=2.0, fy=2.0, interpolation=cv2.INTER_LINEAR)
                        cv2.imshow('Background', BG1)
                        cv2.moveWindow('Background', 50, 650)
                        img1b1 = cv2.resize(img1b, (0, 0), fx=2.0, fy=2.0, interpolation=cv2.INTER_LINEAR)
                        cv2.imshow('Thresholded', img1b1)
                        cv2.moveWindow('Thresholded', 50, 1150)
                        # cv2.imshow('Gaussian', img1c)
                        # cv2.moveWindow('Gaussian', 50, 900)
                        # cv2.imshow('Eroded', img1c)
                        # cv2.moveWindow('Eroded', 50, 900)
                        img1c1 = cv2.resize(img1c, (0, 0), fx=2.0, fy=2.0, interpolation=cv2.INTER_LINEAR)
                        cv2.imshow('Dilated', img1c1)
                        cv2.moveWindow('Dilated', 550, 650)
                        edged1 = cv2.resize(edged, (0, 0), fx=2.0, fy=2.0, interpolation=cv2.INTER_LINEAR)
                        cv2.imshow('Edged', edged1)
                        cv2.moveWindow('Edged', 550, 1150)

            (cnts, _) = cv2.findContours(edged.copy(), cv2.RETR_TREE, cv2.CHAIN_APPROX_NONE)
            cnts = sorted(cnts, key=cv2.contourArea, reverse = True)[:10]
            if len(cnts) == 0:
                x = 0
                y = 0
                y = 0
                w = 0
                h = 0
                if i == 1:
                    data1.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 2:
                    data2.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 3:
                    data3.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 4:
                    data4.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 5:
                    data5.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 6:
                    data6.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 7:
                    data7.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 8:
                    data8.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 9:
                    data9.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 10:
                    data10.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 11:
                    data11.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 12:
                    data12.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 13:
                    data13.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 14:
                    data14.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 15:
                    data15.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 16:
                    data16.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 17:
                    data17.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 18:
                    data18.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 19:
                    data19.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 20:
                    data20.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 21:
                    data21.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 22:
                    data22.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 23:
                    data23.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 24:
                    data24.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 25:
                    data25.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 26:
                    data26.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 27:
                    data27.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 28:
                    data28.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 29:
                    data29.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 30:
                    data30.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 31:
                    data31.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 32:
                    data32.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 33:
                    data33.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 34:
                    data34.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 35:
                    data35.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 36:
                    data36.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 37:
                    data37.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 38:
                    data38.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 39:
                    data39.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 40:
                    data40.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 41:
                    data41.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 42:
                    data42.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 43:
                    data43.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 44:
                    data44.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 45:
                    data45.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 46:
                    data46.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 47:
                    data47.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 48:
                    data48.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 49:
                    data49.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 50:
                    data50.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 51:
                    data51.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 52:
                    data52.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 53:
                    data53.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 54:
                    data54.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 55:
                    data55.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 56:
                    data56.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 57:
                    data57.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 58:
                    data58.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 59:
                    data59.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 60:
                    data60.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 61:
                    data61.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 62:
                    data62.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 63:
                    data63.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 64:
                    data64.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 65:
                    data65.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 66:
                    data66.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 67:
                    data67.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 68:
                    data68.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 69:
                    data69.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 70:
                    data70.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 71:
                    data71.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 72:
                    data72.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 73:
                    data73.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 74:
                    data74.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 75:
                    data75.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 76:
                    data76.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 77:
                    data77.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 78:
                    data78.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 79:
                    data79.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 80:
                    data80.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 81:
                    data81.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 82:
                    data82.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 83:
                    data83.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 84:
                    data84.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 85:
                    data85.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 86:
                    data86.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 87:
                    data87.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 88:
                    data88.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 89:
                    data89.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 90:
                    data90.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 91:
                    data91.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 92:
                    data92.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 93:
                    data93.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 94:
                    data94.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 95:
                    data95.append([0, 0, 0, 0, 0, 0, 0])
                elif i == 96:
                    data96.append([0, 0, 0, 0, 0, 0, 0])
            else:
                [x, y, w, h] = cv2.boundingRect(cnts[0])
                FishX = int(x+w/2)+boxes[0+(i-1)*2][0]
                FishY = int(y+h/2)+boxes[0+(i-1)*2][1]
                Distance = int(np.sqrt((FishX-ROIXY[i-1,0])*(FishX-ROIXY[i-1,0]) + (FishY-ROIXY[i-1,1])*(FishY-ROIXY[i-1,1])))
                Wall = 0
                if Distance > int(ROIXY[i - 1, 2] * 0.71):
                    Wall = 1
                if w*h > SmallestSize:
                    if h > 9:
                        if h < 60:
                            if w < 60:
                                if w * h < LargestSize:
                                    if i==1:
                                        data1.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==2:
                                        data2.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==3:
                                        data3.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==4:
                                        data4.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==5:
                                        data5.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==6:
                                        data6.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==7:
                                        data7.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==8:
                                        data8.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==9:
                                        data9.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==10:
                                        data10.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==11:
                                        data11.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==12:
                                        data12.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==13:
                                        data13.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==14:
                                        data14.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==15:
                                        data15.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==16:
                                        data16.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==17:
                                        data17.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==18:
                                        data18.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==19:
                                        data19.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==20:
                                        data20.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==21:
                                        data21.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==22:
                                        data22.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==23:
                                        data23.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==24:
                                        data24.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==25:
                                        data25.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==26:
                                        data26.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==27:
                                        data27.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==28:
                                        data28.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==29:
                                        data29.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==30:
                                        data30.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==31:
                                        data31.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==32:
                                        data32.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==33:
                                        data33.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==34:
                                        data34.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==35:
                                        data35.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==36:
                                        data36.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==37:
                                        data37.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==38:
                                        data38.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==39:
                                        data39.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==40:
                                        data40.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==41:
                                        data41.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==42:
                                        data42.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==43:
                                        data43.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==44:
                                        data44.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==45:
                                        data45.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==46:
                                        data46.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==47:
                                        data47.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==48:
                                        data48.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==49:
                                        data49.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==50:
                                        data50.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==51:
                                        data51.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==52:
                                        data52.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==53:
                                        data53.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==54:
                                        data54.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==55:
                                        data55.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==56:
                                        data56.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==57:
                                        data57.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==58:
                                        data58.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==59:
                                        data59.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==60:
                                        data60.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==61:
                                        data61.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==62:
                                        data62.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==63:
                                        data63.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==64:
                                        data64.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==65:
                                        data65.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==66:
                                        data66.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==67:
                                        data67.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==68:
                                        data68.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==69:
                                        data69.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==70:
                                        data70.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==71:
                                        data71.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==72:
                                        data72.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==73:
                                        data73.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==74:
                                        data74.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==75:
                                        data75.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==76:
                                        data76.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==77:
                                        data77.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==78:
                                        data78.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==79:
                                        data79.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==80:
                                        data80.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==81:
                                        data81.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==82:
                                        data82.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==83:
                                        data83.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==84:
                                        data84.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==85:
                                        data85.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==86:
                                        data86.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==87:
                                        data87.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==88:
                                        data88.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==89:
                                        data89.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==90:
                                        data90.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==91:
                                        data91.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==92:
                                        data92.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==93:
                                        data93.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==94:
                                        data94.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==95:
                                        data95.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    elif i==96:
                                        data96.append([int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1], x, y, w, h, Wall])
                                    cv2.rectangle(gray2,(x-2+boxes[0+(i-1)*2][0],y-2++boxes[0+(i-1)*2][1]),(x+w+2+boxes[0+(i-1)*2][0],y+h+2++boxes[0+(i-1)*2][1]),(0,255,0),2)
                                    cv2.circle(gray2, (int(x+w/2)+boxes[0+(i-1)*2][0], int(y+h/2)+boxes[0+(i-1)*2][1]), 2, (0,0,255),-1)
                                    if ShowPanels == 1:
                                        if i == Well_ID:
                                            if Well_ID > 0:
                                                CurrentImage = copy.deepcopy(gray2[boxes[0 + (i - 1) * 2][1]:boxes[1 + (i - 1) * 2][1],boxes[0 + (i - 1) * 2][0]:boxes[1 + (i - 1) * 2][0]])
                                                CurrentImage1 = cv2.resize(CurrentImage, (0, 0), fx=2.0, fy=2.0,interpolation=cv2.INTER_LINEAR)
                                                cv2.imshow('Raw', CurrentImage1)
                                                cv2.moveWindow('Raw', 50, 150)
                                else:
                                    if i == 1:
                                        data1.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 2:
                                        data2.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 3:
                                        data3.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 4:
                                        data4.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 5:
                                        data5.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 6:
                                        data6.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 7:
                                        data7.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 8:
                                        data8.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 9:
                                        data9.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 10:
                                        data10.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 11:
                                        data11.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 12:
                                        data12.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 13:
                                        data13.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 14:
                                        data14.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 15:
                                        data15.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 16:
                                        data16.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 17:
                                        data17.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 18:
                                        data18.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 19:
                                        data19.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 20:
                                        data20.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 21:
                                        data21.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 22:
                                        data22.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 23:
                                        data23.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 24:
                                        data24.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 25:
                                        data25.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 26:
                                        data26.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 27:
                                        data27.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 28:
                                        data28.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 29:
                                        data29.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 30:
                                        data30.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 31:
                                        data31.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 32:
                                        data32.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 33:
                                        data33.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 34:
                                        data34.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 35:
                                        data35.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 36:
                                        data36.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 37:
                                        data37.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 38:
                                        data38.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 39:
                                        data39.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 40:
                                        data40.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 41:
                                        data41.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 42:
                                        data42.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 43:
                                        data43.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 44:
                                        data44.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 45:
                                        data45.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 46:
                                        data46.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 47:
                                        data47.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 48:
                                        data48.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 49:
                                        data49.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 50:
                                        data50.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 51:
                                        data51.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 52:
                                        data52.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 53:
                                        data53.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 54:
                                        data54.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 55:
                                        data55.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 56:
                                        data56.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 57:
                                        data57.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 58:
                                        data58.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 59:
                                        data59.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 60:
                                        data60.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 61:
                                        data61.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 62:
                                        data62.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 63:
                                        data63.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 64:
                                        data64.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 65:
                                        data65.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 66:
                                        data66.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 67:
                                        data67.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 68:
                                        data68.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 69:
                                        data69.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 70:
                                        data70.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 71:
                                        data71.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 72:
                                        data72.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 73:
                                        data73.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 74:
                                        data74.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 75:
                                        data75.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 76:
                                        data76.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 77:
                                        data77.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 78:
                                        data78.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 79:
                                        data79.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 80:
                                        data80.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 81:
                                        data81.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 82:
                                        data82.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 83:
                                        data83.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 84:
                                        data84.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 85:
                                        data85.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 86:
                                        data86.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 87:
                                        data87.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 88:
                                        data88.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 89:
                                        data89.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 90:
                                        data90.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 91:
                                        data91.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 92:
                                        data92.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 93:
                                        data93.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 94:
                                        data94.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 95:
                                        data95.append([0, 0, 0, 0, 0, 0, 0])
                                    elif i == 96:
                                        data96.append([0, 0, 0, 0, 0, 0, 0])

                            else:
                                if i == 1:
                                    data1.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 2:
                                    data2.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 3:
                                    data3.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 4:
                                    data4.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 5:
                                    data5.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 6:
                                    data6.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 7:
                                    data7.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 8:
                                    data8.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 9:
                                    data9.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 10:
                                    data10.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 11:
                                    data11.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 12:
                                    data12.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 13:
                                    data13.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 14:
                                    data14.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 15:
                                    data15.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 16:
                                    data16.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 17:
                                    data17.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 18:
                                    data18.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 19:
                                    data19.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 20:
                                    data20.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 21:
                                    data21.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 22:
                                    data22.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 23:
                                    data23.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 24:
                                    data24.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 25:
                                    data25.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 26:
                                    data26.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 27:
                                    data27.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 28:
                                    data28.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 29:
                                    data29.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 30:
                                    data30.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 31:
                                    data31.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 32:
                                    data32.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 33:
                                    data33.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 34:
                                    data34.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 35:
                                    data35.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 36:
                                    data36.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 37:
                                    data37.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 38:
                                    data38.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 39:
                                    data39.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 40:
                                    data40.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 41:
                                    data41.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 42:
                                    data42.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 43:
                                    data43.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 44:
                                    data44.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 45:
                                    data45.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 46:
                                    data46.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 47:
                                    data47.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 48:
                                    data48.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 49:
                                    data49.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 50:
                                    data50.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 51:
                                    data51.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 52:
                                    data52.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 53:
                                    data53.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 54:
                                    data54.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 55:
                                    data55.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 56:
                                    data56.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 57:
                                    data57.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 58:
                                    data58.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 59:
                                    data59.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 60:
                                    data60.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 61:
                                    data61.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 62:
                                    data62.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 63:
                                    data63.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 64:
                                    data64.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 65:
                                    data65.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 66:
                                    data66.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 67:
                                    data67.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 68:
                                    data68.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 69:
                                    data69.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 70:
                                    data70.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 71:
                                    data71.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 72:
                                    data72.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 73:
                                    data73.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 74:
                                    data74.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 75:
                                    data75.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 76:
                                    data76.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 77:
                                    data77.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 78:
                                    data78.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 79:
                                    data79.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 80:
                                    data80.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 81:
                                    data81.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 82:
                                    data82.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 83:
                                    data83.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 84:
                                    data84.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 85:
                                    data85.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 86:
                                    data86.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 87:
                                    data87.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 88:
                                    data88.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 89:
                                    data89.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 90:
                                    data90.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 91:
                                    data91.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 92:
                                    data92.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 93:
                                    data93.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 94:
                                    data94.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 95:
                                    data95.append([0, 0, 0, 0, 0, 0, 0])
                                elif i == 96:
                                    data96.append([0, 0, 0, 0, 0, 0, 0])

                        else:
                            if i == 1:
                                data1.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 2:
                                data2.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 3:
                                data3.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 4:
                                data4.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 5:
                                data5.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 6:
                                data6.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 7:
                                data7.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 8:
                                data8.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 9:
                                data9.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 10:
                                data10.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 11:
                                data11.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 12:
                                data12.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 13:
                                data13.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 14:
                                data14.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 15:
                                data15.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 16:
                                data16.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 17:
                                data17.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 18:
                                data18.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 19:
                                data19.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 20:
                                data20.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 21:
                                data21.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 22:
                                data22.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 23:
                                data23.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 24:
                                data24.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 25:
                                data25.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 26:
                                data26.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 27:
                                data27.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 28:
                                data28.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 29:
                                data29.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 30:
                                data30.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 31:
                                data31.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 32:
                                data32.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 33:
                                data33.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 34:
                                data34.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 35:
                                data35.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 36:
                                data36.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 37:
                                data37.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 38:
                                data38.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 39:
                                data39.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 40:
                                data40.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 41:
                                data41.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 42:
                                data42.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 43:
                                data43.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 44:
                                data44.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 45:
                                data45.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 46:
                                data46.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 47:
                                data47.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 48:
                                data48.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 49:
                                data49.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 50:
                                data50.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 51:
                                data51.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 52:
                                data52.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 53:
                                data53.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 54:
                                data54.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 55:
                                data55.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 56:
                                data56.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 57:
                                data57.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 58:
                                data58.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 59:
                                data59.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 60:
                                data60.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 61:
                                data61.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 62:
                                data62.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 63:
                                data63.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 64:
                                data64.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 65:
                                data65.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 66:
                                data66.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 67:
                                data67.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 68:
                                data68.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 69:
                                data69.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 70:
                                data70.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 71:
                                data71.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 72:
                                data72.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 73:
                                data73.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 74:
                                data74.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 75:
                                data75.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 76:
                                data76.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 77:
                                data77.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 78:
                                data78.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 79:
                                data79.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 80:
                                data80.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 81:
                                data81.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 82:
                                data82.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 83:
                                data83.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 84:
                                data84.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 85:
                                data85.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 86:
                                data86.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 87:
                                data87.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 88:
                                data88.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 89:
                                data89.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 90:
                                data90.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 91:
                                data91.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 92:
                                data92.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 93:
                                data93.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 94:
                                data94.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 95:
                                data95.append([0, 0, 0, 0, 0, 0, 0])
                            elif i == 96:
                                data96.append([0, 0, 0, 0, 0, 0, 0])

                    else:
                        if i == 1:
                            data1.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 2:
                            data2.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 3:
                            data3.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 4:
                            data4.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 5:
                            data5.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 6:
                            data6.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 7:
                            data7.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 8:
                            data8.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 9:
                            data9.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 10:
                            data10.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 11:
                            data11.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 12:
                            data12.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 13:
                            data13.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 14:
                            data14.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 15:
                            data15.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 16:
                            data16.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 17:
                            data17.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 18:
                            data18.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 19:
                            data19.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 20:
                            data20.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 21:
                            data21.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 22:
                            data22.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 23:
                            data23.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 24:
                            data24.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 25:
                            data25.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 26:
                            data26.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 27:
                            data27.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 28:
                            data28.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 29:
                            data29.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 30:
                            data30.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 31:
                            data31.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 32:
                            data32.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 33:
                            data33.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 34:
                            data34.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 35:
                            data35.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 36:
                            data36.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 37:
                            data37.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 38:
                            data38.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 39:
                            data39.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 40:
                            data40.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 41:
                            data41.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 42:
                            data42.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 43:
                            data43.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 44:
                            data44.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 45:
                            data45.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 46:
                            data46.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 47:
                            data47.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 48:
                            data48.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 49:
                            data49.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 50:
                            data50.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 51:
                            data51.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 52:
                            data52.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 53:
                            data53.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 54:
                            data54.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 55:
                            data55.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 56:
                            data56.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 57:
                            data57.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 58:
                            data58.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 59:
                            data59.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 60:
                            data60.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 61:
                            data61.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 62:
                            data62.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 63:
                            data63.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 64:
                            data64.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 65:
                            data65.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 66:
                            data66.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 67:
                            data67.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 68:
                            data68.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 69:
                            data69.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 70:
                            data70.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 71:
                            data71.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 72:
                            data72.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 73:
                            data73.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 74:
                            data74.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 75:
                            data75.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 76:
                            data76.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 77:
                            data77.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 78:
                            data78.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 79:
                            data79.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 80:
                            data80.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 81:
                            data81.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 82:
                            data82.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 83:
                            data83.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 84:
                            data84.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 85:
                            data85.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 86:
                            data86.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 87:
                            data87.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 88:
                            data88.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 89:
                            data89.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 90:
                            data90.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 91:
                            data91.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 92:
                            data92.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 93:
                            data93.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 94:
                            data94.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 95:
                            data95.append([0, 0, 0, 0, 0, 0, 0])
                        elif i == 96:
                            data96.append([0, 0, 0, 0, 0, 0, 0])

                else:
                    if i == 1:
                        data1.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 2:
                        data2.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 3:
                        data3.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 4:
                        data4.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 5:
                        data5.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 6:
                        data6.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 7:
                        data7.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 8:
                        data8.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 9:
                        data9.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 10:
                        data10.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 11:
                        data11.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 12:
                        data12.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 13:
                        data13.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 14:
                        data14.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 15:
                        data15.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 16:
                        data16.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 17:
                        data17.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 18:
                        data18.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 19:
                        data19.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 20:
                        data20.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 21:
                        data21.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 22:
                        data22.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 23:
                        data23.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 24:
                        data24.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 25:
                        data25.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 26:
                        data26.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 27:
                        data27.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 28:
                        data28.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 29:
                        data29.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 30:
                        data30.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 31:
                        data31.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 32:
                        data32.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 33:
                        data33.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 34:
                        data34.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 35:
                        data35.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 36:
                        data36.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 37:
                        data37.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 38:
                        data38.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 39:
                        data39.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 40:
                        data40.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 41:
                        data41.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 42:
                        data42.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 43:
                        data43.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 44:
                        data44.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 45:
                        data45.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 46:
                        data46.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 47:
                        data47.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 48:
                        data48.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 49:
                        data49.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 50:
                        data50.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 51:
                        data51.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 52:
                        data52.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 53:
                        data53.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 54:
                        data54.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 55:
                        data55.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 56:
                        data56.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 57:
                        data57.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 58:
                        data58.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 59:
                        data59.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 60:
                        data60.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 61:
                        data61.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 62:
                        data62.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 63:
                        data63.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 64:
                        data64.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 65:
                        data65.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 66:
                        data66.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 67:
                        data67.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 68:
                        data68.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 69:
                        data69.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 70:
                        data70.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 71:
                        data71.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 72:
                        data72.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 73:
                        data73.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 74:
                        data74.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 75:
                        data75.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 76:
                        data76.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 77:
                        data77.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 78:
                        data78.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 79:
                        data79.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 80:
                        data80.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 81:
                        data81.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 82:
                        data82.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 83:
                        data83.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 84:
                        data84.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 85:
                        data85.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 86:
                        data86.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 87:
                        data87.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 88:
                        data88.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 89:
                        data89.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 90:
                        data90.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 91:
                        data91.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 92:
                        data92.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 93:
                        data93.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 94:
                        data94.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 95:
                        data95.append([0, 0, 0, 0, 0, 0, 0])
                    elif i == 96:
                        data96.append([0, 0, 0, 0, 0, 0, 0])

        for i in range(96):
            cv2.circle(gray2, (int(ROIXY[i - 1, 0]), int(ROIXY[i - 1, 1])), int(ROIXY[i - 1, 2]), (255, 0, 0), 2)
            cv2.circle(gray2, (int(ROIXY[i - 1, 0]), int(ROIXY[i - 1, 1])), int(ROIXY[i - 1, 2] * 0.71), (255, 255, 255), 1)
        if WellNumber > 0:
            cv2.rectangle(gray2, (boxes[(WellNumber-1)*2][0], boxes[(WellNumber-1)*2][1]), (boxes[(WellNumber-1)*2+1][0], boxes[(WellNumber-1)*2+1][1]), (0, 255, 255),3)  # bounding rectangle in green

        cv2.imshow('frame', gray2)

        gray2a = copy.deepcopy(gray2)
        gray2 = cv2.cvtColor(gray2,cv2.COLOR_BGR2GRAY)
        # gray6 = cv2.cvtColor(gray6,cv2.COLOR_BGR2GRAY)
        gray9 = cv2.cvtColor(gray9, cv2.COLOR_BGR2GRAY)

        if j == 1:
            maskA = np.zeros((300, 600))
            maskB = cv2.convertScaleAbs(maskA)
            cv2.imshow('Setting', maskB)
            cv2.createTrackbar('Smallest', 'frame', 225, 625, on_change)
            cv2.setTrackbarPos("Smallest", 'frame', SmallestSize)
            cv2.createTrackbar('Largest', 'frame', 1600, 3600, on_change)
            cv2.setTrackbarPos("Largest", 'frame', LargestSize)
            cv2.createTrackbar("Protection", 'Setting', ProtectionState, 1, on_change)
            cv2.createTrackbar('Well_ID', 'Setting', 1, 96, on_change)
            cv2.setTrackbarPos("Well_ID", 'Setting', 1)
            cv2.createTrackbar('X', 'Setting', int(ROIXY[Well_ID,0]), int(ROIXY[Well_ID,0])+40, on_change)
            cv2.setTrackbarPos("X", 'Setting', int(ROIXY[Well_ID,0]))
            cv2.setTrackbarMin('X', 'Setting', int(ROIXY[Well_ID,0]-40))
            cv2.createTrackbar('Y', 'Setting', int(ROIXY[Well_ID,1]), int(ROIXY[Well_ID,1])+40, on_change)
            cv2.setTrackbarPos("Y", 'Setting', int(ROIXY[Well_ID,1]))
            cv2.setTrackbarMin('Y', 'Setting', int(ROIXY[Well_ID,1]-40))
            cv2.createTrackbar('Radius', 'Setting', int(ROIXY[Well_ID,2]), int(ROIXY[Well_ID,2])+20, on_change)
            cv2.setTrackbarPos("Radius", 'Setting', int(ROIXY[Well_ID,2]))
            cv2.setTrackbarMin("Radius", 'Setting', int(ROIXY[Well_ID,2])-20)
            cv2.createTrackbar('Threshold', 'Setting', 40, 80, on_change)
            cv2.setTrackbarPos("Threshold", 'Setting', int(FishThreshold))
            cv2.setTrackbarMin("Threshold", 'Setting', 1)
            cv2.createTrackbar('Global-X', 'Setting', 0, 2, on_change)
            cv2.setTrackbarPos("Global-X", 'Setting', 1)
            cv2.setTrackbarMin("Global-X", 'Setting', 0)
            cv2.setTrackbarMax("Global-X", 'Setting', 2)
            cv2.createTrackbar('Global-Y', 'Setting', 0, 2, on_change)
            cv2.setTrackbarPos("Global-Y", 'Setting', 1)
            cv2.setTrackbarMin("Global-Y", 'Setting', 0)
            cv2.setTrackbarMax("Global-Y", 'Setting', 2)
            GlobalX = 1
            GlobalY = 1
        else:
            if tempSave == 0:
                cv2.putText(maskB, 'The setting is not saved yet.', (5,100), font, 1, 255, 2, cv2.LINE_AA)
            elif tempSave == 1:
                maskB = cv2.convertScaleAbs(maskA)
                cv2.putText(maskB, 'The setting is saved.', (5,100), font, 1, 255, 2, cv2.LINE_AA)
                tempSave == 2
            cv2.imshow('Setting', maskB)
            SmallestSize = cv2.getTrackbarPos('Smallest', 'frame')
            LargestSize = cv2.getTrackbarPos('Largest', 'frame')
            ProtectionState = cv2.getTrackbarPos('Protection', 'Setting')
            Well_ID = cv2.getTrackbarPos('Well_ID', 'Setting')
            tempX1 = cv2.getTrackbarPos('X', 'Setting')
            tempY1 = cv2.getTrackbarPos('Y', 'Setting')
            if GlobalX != cv2.getTrackbarPos('Global-X', 'Setting'):
                GlobalX = cv2.getTrackbarPos('Global-X', 'Setting')
                if GlobalX == 0:
                    for i in range(96):
                        ROIXY[i,0] = ROIXY[i,0]  -1
                else:
                    for i in range(96):
                        ROIXY[i, 0] = ROIXY[i, 0] + 1
                GlobalX = 1
                cv2.setTrackbarPos("Global-X", 'Setting', 1)
            if GlobalY != cv2.getTrackbarPos('Global-Y', 'Setting'):
                GlobalY = cv2.getTrackbarPos('Global-Y', 'Setting')
                if GlobalY == 0:
                    for i in range(96):
                        ROIXY[i,1] = ROIXY[i,1] - 1
                else:
                    for i in range(96):
                        ROIXY[i,1] = ROIXY[i,1] + 1
                GlobalY = 1
                cv2.setTrackbarPos("Global-Y", 'Setting', 1)
            if Well_ID > 0:
                if tempX1 > int(ROIXY[Well_ID-1, 0]) + 40 or tempX1 < int(ROIXY[Well_ID-1, 0]) - 40:
                    cv2.setTrackbarMin('X', 'Setting', int(ROIXY[Well_ID-1, 0]) - 40)
                    cv2.setTrackbarMax('X', 'Setting', int(ROIXY[Well_ID-1, 0]) + 40)
                    cv2.setTrackbarPos("X", 'Setting', int(ROIXY[Well_ID-1, 0]))
                    cv2.setTrackbarPos("Threshold", 'Setting', int(ROIXY[Well_ID - 1, 3]))
                if tempY1 > int(ROIXY[Well_ID-1, 1]) + 40 or tempY1 < int(ROIXY[Well_ID-1, 1]) - 40:
                    cv2.setTrackbarMin('Y', 'Setting', int(ROIXY[Well_ID-1, 1]) - 40)
                    cv2.setTrackbarMax('Y', 'Setting', int(ROIXY[Well_ID-1, 1]) + 40)
                    cv2.setTrackbarPos("Y", 'Setting', int(ROIXY[Well_ID-1, 1]))
                    cv2.setTrackbarPos("Threshold", 'Setting', int(ROIXY[Well_ID - 1, 3]))
                if ProtectionState == 0:
                    ROIXY[Well_ID-1, 0] = cv2.getTrackbarPos('X', 'Setting')
                    ROIXY[Well_ID-1, 1] = cv2.getTrackbarPos('Y', 'Setting')
                    ROIXY[Well_ID-1, 2] = cv2.getTrackbarPos('Radius', 'Setting')
                    # FishThreshold = cv2.getTrackbarPos('Threshold', 'Setting')
                    ROIXY[Well_ID-1, 3] = cv2.getTrackbarPos('Threshold', 'Setting')
                else:
                    cv2.setTrackbarPos("Threshold", 'Setting', int(ROIXY[Well_ID-1, 3]))
                    cv2.setTrackbarPos("X", 'Setting', int(ROIXY[Well_ID-1, 0]))
                    cv2.setTrackbarPos("Y", 'Setting', int(ROIXY[Well_ID-1, 1]))
                    cv2.setTrackbarPos("Radius", 'Setting', int(ROIXY[Well_ID-1, 2]))

        elapsed_time=(datetime.datetime.now() - starttime).total_seconds()

        # tank1 = gray2[boxes[0][1]:boxes[NumberOfFish*2-1][1], boxes[0][0]:boxes[NumberOfFish*2-1][0]]
        tank1a = gray9[boxes[0][1]:boxes[NumberOfFish*2-1][1], boxes[0][0]:boxes[NumberOfFish*2-1][0]]
        tank1b = gray2a[boxes[0][1]:boxes[NumberOfFish*2-1][1], boxes[0][0]:boxes[NumberOfFish*2-1][0]]
        # tank2 = gray2[boxes[NumberOfFish*2][1]:boxes[NumberOfFish*4-1][1], boxes[NumberOfFish*2][0]:boxes[NumberOfFish*4-1][0]]
        tank2a = gray9[boxes[NumberOfFish*2][1]:boxes[NumberOfFish*4-1][1], boxes[NumberOfFish*2][0]:boxes[NumberOfFish*4-1][0]]
        tank2b = gray2a[boxes[NumberOfFish*2][1]:boxes[NumberOfFish*4-1][1],boxes[NumberOfFish*2][0]:boxes[NumberOfFish*4-1][0]]
        # resize1 = cv2.resize(tank1, (int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/2)+1,int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/2)+1))
        # resize1a = cv2.resize(tank1a, (int((boxes[NumberOfFish * 2 - 1][0] - boxes[0][0]) / 2) + 1,int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) / 2) + 1))
        # resize2 = cv2.resize(tank2, (int((boxes[NumberOfFish*4-1][0]-boxes[NumberOfFish*2][0])/2)+1,int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1])/2)+1))
        # resize2a = cv2.resize(tank2a, (int((boxes[NumberOfFish*4-1][0] - boxes[NumberOfFish*2][0]) / 2) + 1,int((boxes[NumberOfFish*4-1][1] - boxes[NumberOfFish*2][1]) / 2) + 1))
        # resize1 = cv2.resize(tank1, (int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/1)+1,int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/1)+1))
        resize1a = cv2.resize(tank1a, (int((boxes[NumberOfFish*2-1][0]-boxes[0][0])/2)+1,int((boxes[NumberOfFish*2-1][1]-boxes[0][1])/2)+1))
        resize1b = cv2.resize(tank1b, (int((boxes[NumberOfFish * 2 - 1][0] - boxes[0][0])/3) + 1,int((boxes[NumberOfFish * 2 - 1][1] - boxes[0][1]) /3) + 1))
        # resize2 = cv2.resize(tank2, (int((boxes[NumberOfFish*4-1][0]-boxes[NumberOfFish*2][0])/1)+1, int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1])/1)+1))
        resize2a = cv2.resize(tank2a, (int((boxes[NumberOfFish*4-1][0]-boxes[NumberOfFish*2][0])/2)+1, int((boxes[NumberOfFish*4-1][1]-boxes[NumberOfFish*2][1])/2) + 1))
        resize2b = cv2.resize(tank2b, (int((boxes[NumberOfFish * 4 - 1][0] - boxes[NumberOfFish * 2][0]) /3) + 1,int((boxes[NumberOfFish * 4 - 1][1] - boxes[NumberOfFish * 2][1])/3) + 1))
        # Tank1.write(resize1)
        Tank1a.write(resize1a)
        Tank1b.write(resize1b)
        # Tank2.write(resize2)
        Tank2a.write(resize2a)
        Tank2b.write(resize2b)
        if tt9653 == 1:    # denoting the midnight vibration status
            data2Stamp.append([j, 0-elapsed_time])

        else:
            data2Stamp.append([j, elapsed_time])

        if LightIndex == 1 or LightIndex == 2:
            data1Stamp.append([j,0-elapsed_time])
        else:
            if tt9652 == 1:
                data1Stamp.append([j, 0-elapsed_time])
            else:
                data1Stamp.append([j, elapsed_time])
        frametime = datetime.datetime.now()
        elapsed_time = 0

    else:
        # gray3 = copy.copy(frame)
        preset = preset + 1
        while preset1 < 101:
            if preset%5 == 1:
                preset1 = preset1 + 1
                if preset1 < 100:
                    cv2.accumulateWeighted(gray2, gray4, 0.1)
                    gray5 = cv2.convertScaleAbs(gray4)
                    cv2.putText(gray5, 'Please press the space bar for 50 times to establish the background image.', [10, 40] , font, 1, (0, 0, 0), 1, cv2.LINE_AA)
                    cv2.putText(gray5, str(preset1) + '/50', [10, 70],  font, 1, (0, 0, 0), 1, cv2.LINE_AA)
                    cv2.imshow('frame',gray5)
                else:
                    cv2.putText(gray2, 'Now you can select the ROIs', [10, 40],font, 1, (0, 0, 0), 1, cv2.LINE_AA)
                    cv2.imshow('frame',gray2)

    i=i+1
    cv2.setMouseCallback('frame', set_up_ROIs)
    if cv2.waitKey(3) & 0xFF == ord('q'):
        save1=1
        break
    elif cv2.waitKey(3) & 0xFF == ord('s'):
        ROIXY1 = pd.DataFrame(ROIXY)
        header = ['X', 'Y', 'Radius', 'Threshold']
        ROIXY1.to_csv("D:\\Sleep_Rig\\Excel\\" + GroupName + "\\" + GroupName + "_Settings.csv",  header=header, encoding='utf-8')
        print('The settings are saved into a csv files in your folder')
        ShowPanels = 1
        tempSave = 1

else:
    ROIXY1 = pd.DataFrame(ROIXY)
    header = ['X', 'Y', 'Radius', 'Threshold']
    ROIXY1.to_csv("D:\\Sleep_Rig\\Excel\\" + GroupName + "\\" + GroupName + "_ExpEndSettings.csv",  header=header, encoding='utf-8')
    print('The settings are saved into a csv files in your folder')



# When everything is done release the capture

if save1==1:
    CurrentTime = strftime("%H",) + strftime("%M",) + strftime("%S",)
    tt91 = '%02d' % int((i-preset+1) / 3600)
    #tt92 = '%02d' % (int((i-preset+1) / 3600)+1)
    tt91 = '%03d' % int(tt9655)
    tt91 = int(tt91) + 1
    tt91 = '%03d' % int(tt91)
    tt91 = str(tt91)

    print ('Files_' + tt91 + '_quit were saved at ' + CurrentTime)
    tt99 = Excel_Save(tt91, GroupName, boxes, data1, data1Stamp, data2, data2Stamp, data3, data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24,data25,data26,data27,data28,data29,data30,data31,data32,data33,data34,data35,data36,data37,data38,data39,data40,data41,data42,data43,data44,data44, data46,data47,data48,data49,data50,data51,data52,data53,data54,data55,data56,data57,data58,data59,data60,data61,data62,data63,data64,data65,data66,data67,data68,data69,data70,data71,data72,data73,data74,data75,data76,data77,data78,data79,data80,data81,data82,data83,data84,data85,data86,data87,data88,data89,data90,data91,data92,data93,data94,data95,data96, Tank1a, Tank1b, Tank2a,Tank2b, FishName1,DOB1,FishType1,FishSex1,FishName2,DOB2,FishType2,FishSex2,FrameWidth,FrameHeight,TankWidth,save1)

cap.release()


CurrentDate = strftime("%y",) + strftime("%m",) + strftime("%d",)
CurrentTime = strftime("%H",) + strftime("%M",) + strftime("%S",)
print ('The experiment ended at ...ymd & hms ____' + CurrentDate + ' & ' + CurrentTime)

cv2.destroyAllWindows()