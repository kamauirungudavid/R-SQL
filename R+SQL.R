library(DBI)
library(RSQLite)
library(RODBC)
library(odbc)

library(tidyverse)
########CREATING THE CONNECTION##################
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};",
                 Server = 'localhost',Database="data",UID='root',PWD='dairung254',
                 Port=26022)


##########GETTING THE DATA#################
contact <- read.csv('C:/Users/IRUNGU/Documents/dscproj/cont.csv',header = T)
grade <- read.csv('C:/Users/IRUNGU/Documents/dscproj/grade.csv',header = T)
fmark <- read.csv('C:/Users/IRUNGU/Documents/dscproj/fmark.csv',header = T)
fmark1 <- read.csv('C:/Users/IRUNGU/Documents/dscproj/fmark1.csv',header = T)
course <- read.csv('C:/Users/IRUNGU/Documents/dscproj/course.csv',header = T)
#################ATTRIB CONSTR########################
 
TYPES_cont <- list(Name = 'varchar(100)',Email='varchar(100)',Country='varchar(30)',
              Background='varchar(100)',Gender = 'varchar(1)',Streams='varchar(30)',
              DOB='varchar(20)',POB='varchar(20)')
TYPES_grade <- list(Email='varchar(100)',SRR= 'varchar(1)',PP= 'varchar(1)',DA='varchar(1)',
                    SM='varchar(1)',RMDS='varchar(1)')
TYPES_fmark <- list(Email='varchar(100)',SRR = 'numeric(3,1)',PP= 'numeric(3,1)',DA = 'numeric(3,1)',
                    SM = 'numeric(3,1)',RMDS = 'numeric(3,1)')

TYPES_course <- list(Courses='varchar(100)', Acronyms = 'varchar(7)')

#set time to see how long it will take to write the data.
system.time(dbWriteTable(con,name='student',value = contact,
                         field.types=TYPES_cont,row.names=F))

#
system.time(dbWriteTable(con,name='grade',value = grade,
                         field.types=TYPES_grade,row.names=F))
#
system.time(dbWriteTable(con,name='fmark',value = fmark,
                         field.types=TYPES_fmark,row.names=F))
#
system.time(dbWriteTable(con,name='course',value = course,
                         field.types=TYPES_course,row.names=F))


###QUERRYING#######
dt <- dbSendQuery(con, 'select * from student Where Background = "Statistics"' )
df <- dbFetch(dt,3)
head(df)
dbClearResult(dt)

send <- dbSendQuery(con,'insert into student (Name,Email,Country,Background,Gender) values ("Ruth Wangui","ruth@aims.ac.ke","Kenya","Statistics","F")')

r <- dbSendQuery(con,'select * from student where Name = "Ruth Wangui"')
r <- dbFetch(r)
r




table <- dbSendQuery(con,'show tables')
tab <- dbFetch(table)
tab
dbClearResult(table)

#
student <- dbSendQuery(con, 'select * from grade')
result <- dbFetch(student) 
head(result)
library(ggplot2)
library(plotly)
p = ggplot(data = result, aes(x = SRR, fill = SRR )) + geom_bar(stat = 'count')
dbClearResult(student)
ggplotly(p)
dbClearResult(m)
###Name of student with distinction in SRR###########
g1 <- dbSendQuery(con,'select Name from  student c, grade g where g.Email = c.Email and g.SRR = "D" ')
g <- dbFetch(g1)
g 
dbClearResult(g1)

###marks###
t1 <- dbSendQuery(con, 'select s.Name,f.SRR, f.PP, f.DA, f.RMDS,f.DA,f.SM  
                  from fmark f, student s where s.Email = f.Email and s.Name = "Caren Muhonja NDEDA"')
t <- dbFetch(t1)
t
##
m <- dbSendQuery(con,'select PP from fmark')
m <- dbFetch(m)
ggplot(data = m, aes(x=PP)) + geom_density(fill = 'green')

########
q = dbSendQuery(con, 'select count(PP) as PP ,count(SRR) as SRR from fmark')
q <- dbFetch(q)
q 
p = ggplot(c(q$PP,q$SRR),aes(c(q$PP,q$SRR))) + geom_bar()
ggplotly(p)
dbClearResult(m)
####MAP###
af <- dbSendQuery(con, 'select country as region, count(*) as value from student group by country ')
af <- dbFetch(af)
library(dplyr)
library(mapproj)
###########################
af$value = as.numeric(af$value)
w <- map_data("world")
wdata <- merge(w, af, by.X="region", by.y = "region", all.x=T)
wdata <- wdata[order(wdata$group,wdata$order),]

ggplotly(ggplot(wdata, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=value))+coord_map("ortho", orientation = c(15,18,5)) +
  ggtitle('Distribution of students across Africa') + xlab('')+ylab(''))#+coord_map("mercator")

mapdt <- left_join(world, af, by = "region" )
mapdt <- mapdt %>% filter(!is.na(mapdt$value))
map1 <- ggplot(mapdt, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=value), color = "black")
map1

##################
df <- data.frame(name = c('Chantal UMUTONI'), post = c('middle', 'fist', 'last'))
k <- as.matrix(df)
k = k[1]

sql <- "select * from student where Name = ?Name" 
query <- sqlInterpolate(con, sql, Name = "Caren Muhonja NDEDA")

k <- dbSendQuery(con,query)
s <- dbFetch(k)
s
################################


############################

##dropping########### 
dbSendQuery(con, 'drop table student')
###



