?UCBAdmissions
str(UCBAdmissions)
UCBAdmissions
plot(UCBAdmissions)

#Use of Marginal Frequencies
margin.table(UCBAdmissions,1) #Admit
margin.table(UCBAdmissions,2) #Gender
margin.table(UCBAdmissions,3) #Dept
margin.table(UCBAdmissions) #Total
?margin.table

#We will now create a new structure starting from these data
admit.dept <- margin.table(UCBAdmissions,3)
str(admit.dept)
barplot(admit.dept)
prop.table(admit.dept)
round(prop.table(admit.dept),2) #show as proportions with 2 digits
round(prop.table(admit.dept),2)*100

#Go from table to one row per case
admit <- as.data.frame.table(UCBAdmissions) #Coerces to Data Frame
admit2 <- lapply(admit, function(x)rep(x,admit$Freq)) #Repeats each row by frequency as indictaed in the column Freq of the newly created object admit
admit3 <- as.data.frame(admit2) # Cobverts from list back to data frame

#we now rremove column 4 which is redundant information in admit3
admit4 <- admit3[,-4]

#Now to do it all at once
admit.rows <- as.data.frame(lapply(as.data.frame.table(UCBAdmissions),function(x)rep(x,as.data.frame.table(UCBAdmissions)$Freq)))[,-4]