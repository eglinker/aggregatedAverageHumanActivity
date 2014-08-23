## CODEBOOK FOR AGGREGATED AVERAGE OF HUMAN ACTIVITY RECOGNITION USING SMARTPHONE�S DATA SET

The variables in this tidy data set represent aggregated average values of raw data obtained from the Human Activity Recognition Using Smartphone�s Data Set.  The original source data set is obtained at:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The original data set consists of data generated by the accelerometers and gyro scopes of a Samsung Galaxy S II phone.  The phones were worn by 30 subjects while they performed six different activities 

The final data set was produced by modifying and transforming the original data set as follows:

* Merge data from training and test data set files below into single data frame:X_train.txt,  y_train.txt, X_test.txt', y_test.txt, subject_train.txt and subject_test.txt

* Extract only mean and standard deviations from original data set.

* Replace numbers designating activities in original data set with descriptive activity names from file named activity_labels.txt.

* Make labels from table more appropriate for the final tidy dataset by using string substitutions and by removing �untidy characters� such as), -, (, _, and coma. 

* The averages are aggregated for each activity and subject.

 
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md

# [1]	subject

This is an id number for each subject.
	
	VALUES:	Integer value ranging 1 - 30

 
# [2]	trainingLabels

Descriptive labels for training activities.

VALUES:
	WALKING
	WALKING_UPSTAIRS
	WALKING_DOWNSTAIRS
	SITTING
	STANDING
	LAYING

                   
# [3]	timeBodyAccMeanX

Time domain aggregate average of X mean acceleration value.

VALUES: normalized and bounded within [-1,1]



# [4]	timeBodyAccMeanY

Time domain aggregate average of Y mean acceleration value.

VALUES: normalized and bounded within [-1,1]


                  
# [5]	timeBodyAccMeanZ

Time domain aggregate average of Z mean acceleration value.  

VALUES: normalized and bounded within [-1,1]


                   
# [6]	timeBodyAccStdX
	
Time domain aggregate average of X mean acceleration standard deviation value.

VALUES: normalized and bounded within [-1,1]


                     
[7]	timeBodyAccStdY

Time domain aggregate average of Y acceleration standard deviation value.

VALUES: normalized and bounded within [-1,1]



[8]	timeBodyAccStdZ

Time domain aggregate average of Z acceleration standard deviation value.
 
VALUES: normalized and bounded within [-1,1]


                   
[9]	timeGravityAccMeanX

Time domain aggregate average of mean X gravity acceleration value.

VALUES: normalized and bounded within [-1,1]



[10]	timeGravityAccMeanY

Time domain aggregate average of mean y gravity acceleration value.

VALUES: normalized and bounded within [-1,1]


                
[11]	timeGravityAccMeanZ

Time domain aggregate average of mean Z gravity acceleration value.

VALUES: normalized and bounded within [-1,1]



[12]	timeGravityAccStdX

Time domain aggregate average of X gravity acceleration standard deviation value.

VALUES: normalized and bounded within [-1,1]


                  
[13]	timeGravityAccStdY

Time domain aggregate average of Y gravity acceleration standard deviation value.

VALUES: normalized and bounded within [-1,1]



[14]	timeGravityAccStdZ

Time domain aggregate average of Z gravity acceleration standard deviation value.

VALUES: normalized and bounded within [-1,1]


               
[15]	timeBodyAccJerkMeanX

Time domain aggregate average of mean X acceleration jerk value.


[16]	timeBodyAccJerkMeanY        

Time domain aggregate average of mean y acceleration jerk value.
        

[17]	timeBodyAccJerkMeanZ

Time domain aggregate average of mean X acceleration jerk value.


[18]	timeBodyAccJerkStdX 

Time domain aggregate average of X acceleration jerk standard deviation value.

                
[19]	timeBodyAccJerkStdY

Time domain aggregate average of Y acceleration jerk standard deviation value.

[20]	timeBodyAccJerkStdZ       

Time domain aggregate average of Z acceleration jerk standard deviation value.
          
[21]	timeBodyGyroMeanX

Time domain aggregate average of X gyro mean value.



[22]	timeBodyGyroMeanY  

Time domain aggregate average of Y gyro mean value.

                 

[23]	timeBodyGyroMeanZ

Time domain aggregate average of Z gyro mean value.



[24]	timeBodyGyroStdX 

Time domain aggregate average of X gyro standard deviation value.


                   
[25]	timeBodyGyroStdY

Time domain aggregate average of Y gyro standard deviation value.



[26]	timeBodyGyroStdZ    

Time domain aggregate average of Z gyro standard deviation value.


                
[27]	timeBodyGyroJerkMeanX

Time domain aggregate average of X gyro jerk mean value.



[28]	timeBodyGyroJerkMeanY   

Time domain aggregate average of Y gyro jerk mean value.


            
[29]	timeBodyGyroJerkMeanZ

Time domain aggregate average of Z gyro jerk mean value.



[30]	timeBodyGyroJerkStdX 

Time domain aggregate average of X gyro jerk standard deviation value.
               
[31]	timeBodyGyroJerkStdY

Time domain aggregate average of Y gyro jerk standard deviation value.


[32]	timeBodyGyroJerkStdZ  

Time domain aggregate average of X gyro jerk standard deviation value.

              
[33]	timeBodyAccMagMean

Time domain aggregate average of AccMag mean value.


[34]	timeBodyAccMagStd  

Time domain aggregate average of AccMag standard deviation value.

                 
[35]	timeGravityAccMagMean

Time domain aggregate average of gravity AccMag mean value.


[36]	timeGravityAccMagStd  

Time domain aggregate average of gravity AccMag standard deviation value.


[37]	timeBodyAccJerkMagMean

Time domain aggregate average of AccJerkMag mean value.


[38]	timeBodyAccJerkMagStd 

Time domain aggregate average of AccJerkMag standard deviation value.

              
[39]	timeBodyGyroMagMean

Time domain aggregate average of GyroMag mean value.


[40]	timeBodyGyroMagStd 

Time domain aggregate average of acceleration standard deviation value.
                 
[41]	timeBodyGyroJerkMagMean

Time domain aggregate average of GyroJerkMag mean value.

[42]	timeBodyGyroJerkMagStd

Time domain aggregate average of GyroJerkMag standard deviation value.

              
[43]	frequencyBodyAccMeanX

Aggregate average of FFT transformed version of timeBodyAccMeanX.


[44]	frequencyBodyAccMeanY   

Aggregate average of FFT transformed version of timeBodyAccMeanY.

            
[45]	frequencyBodyAccMeanZ

Aggregate average of FFT transformed version of timeBodyAccMeanZ.



[46]	frequencyBodyAccStdX                

Aggregate average of FFT transformed version of timeBodyAccStdX.


[47]	frequencyBodyAccStdY

Aggregate average of FFT transformed version of timeBodyAccStdY.



[48]	frequencyBodyAccStdZ                

Aggregate average of FFT transformed version of timeBodyAccStdZ.


[49]	frequencyBodyAccMeanFreqX

Aggregate average of FFT transformed version of timeBodyAccMeanFreqX.


[50]	frequencyBodyAccMeanFreqY           

Aggregate average of FFT transformed version of timeBodyAccMeanFreqY.


[51]	frequencyBodyAccMeanFreqZ

Aggregate average of FFT transformed version of timeBodyAccMeanFreqZ.


[52]	frequencyBodyAccJerkMeanX           

Aggregate average of FFT transformed version of timeBodyAccJerkMeanX.


[53]	frequencyBodyAccJerkMeanY

Aggregate average of FFT transformed version of timeBodyAccJerkMeanY.


[54]	frequencyBodyAccJerkMeanZ

Aggregate average of FFT transformed version of timeBodyAccJerkMeanZ.


[55]	frequencyBodyAccJerkStdX

Aggregate average of FFT transformed version of timeBodyAccJerkStdX.


[56]	frequencyBodyAccJerkStdY            

Aggregate average of FFT transformed version of timeBodyAccJerkStdY.


[57]	frequencyBodyAccJerkStdZ

Aggregate average of FFT transformed version of timeBodyAccJerkStdZ.


[58]	frequencyBodyAccJerkMeanFreqX       

Aggregate average of FFT transformed version of timeBodyAccJerkMeanFreqX.


[59]	frequencyBodyAccJerkMeanFreqY

Aggregate average of FFT transformed version of timeBodyAccJerkMeanFreqY.


[60]	frequencyBodyAccJerkMeanFreqZ       

Aggregate average of FFT transformed version of timeBodyAccJerkMeanFreqZ.


[61]	frequencyBodyGyroMeanX

Aggregate average of FFT transformed version of timeBodyGyroMeanX.


[62]	frequencyBodyGyroMeanY              

Aggregate average of FFT transformed version of timeBodyGyroMeanY.


[63]	frequencyBodyGyroMeanZ

Aggregate average of FFT transformed version of timeBodyGyroMeanZ.


[64]	frequencyBodyGyroStdX               

Aggregate average of FFT transformed version of timeBodyGyroStdX.


[65]	frequencyBodyGyroStdY

Aggregate average of FFT transformed version of timeBodyGyroStdY.


[66]	frequencyBodyGyroStdZ               

Aggregate average of FFT transformed version of timeBodyGyroStdZ.


[67]	frequencyBodyGyroMeanFreqX

Aggregate average of FFT transformed version of timeBodyGyroMeanFreqX.


[68]	frequencyBodyGyroMeanFreqY          

Aggregate average of FFT transformed version of time BodyGyroMeanFreqY.  


[69]	frequencyBodyGyroMeanFreqZ

Aggregate average of FFT transformed version of timeBodyGyroMeanFreqZ.


[70]	frequencyBodyAccMagMean             

Aggregate average of FFT transformed version of timeBodyAccMagMean.


[71]	frequencyBodyAccMagStd

Aggregate average of FFT transformed version of timeBodyAccMagStd.


[72]	frequencyBodyAccMagMeanFreq         

Aggregate average of FFT transformed version of timeBodyAccMagMeanFreq.


[73]	frequencyBodyBodyAccJerkMagMean

Aggregate average of FFT transformed version of timeBodyBodyAccJerkMagMean.



[74]	frequencyBodyBodyAccJerkMagStd      

Aggregate average of FFT transformed version of timeBodyBodyAccJerkMagStd.


[75]	frequencyBodyBodyAccJerkMagMeanFreq

Aggregate average of FFT transformed version of timeBodyBodyAccJerkMagMeanFreq.



[76]	frequencyBodyBodyGyroMagMean        

Aggregate average of FFT transformed version of timeBodyBodyGyroMagMean  


[77]	frequencyBodyBodyGyroMagStd

Aggregate average of FFT transformed version of timeBodyBodyGyroMagStd.


[78]	frequencyBodyBodyGyroMagMeanFreq    

Aggregate average of FFT transformed version of timeBodyBodyGyroMagMeanFreq.


[79]	frequencyBodyBodyGyroJerkMagMean

Aggregate average of FFT transformed version of timeBodyBodyGyroJerkMagMean.


[80]	frequencyBodyBodyGyroJerkMagStd 

Aggregate average of FFT transformed version of timeBodyBodyGyroJerkMagStd.

    
[81]	frequencyBodyBodyGyroJerkMagMeanFreq

Aggregate average of FFT transformed version of timeBodyBodyGyroJerkMagMeanFreq.


[82]	angletBodyAccMeanGravity            

Aggregate average of angletBodyAccMeanGravity from original data set.


[83]	angletBodyAccJerkMeanGravityMean

Aggregate average of angletBodyAccJerkMeanGravityMean from original data set.


[84]	angletBodyGyroMeanGravityMean       

Aggregate average of angletBodyGyroMeanGravityMean from original data set.

[85]	angletBodyGyroJerkMeanGravityMean

Aggregate average of angletBodyGyroJerkMeanGravityMean from original data set.


[86]	angleXGravityMean                   

Aggregate average of angleXGravityMean from original data set.


[87]	angleYGravityMean

Aggregate average of angleYGravityMean from original data set.


[88]	angleZGravityMean  

Aggregate average of angleZGravityMean from original data set.


Aggregated average of human activity recognition using Smartphone�s data set.


