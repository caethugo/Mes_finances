dt
class(dt)
str(dt)

# We need to change the type of columns.





#Global expenses
#We need to create a table for daily data

globex <- dt[DoG=="D", .(Expenses=sum(Montant)), by=Date]

globex

plot_ly(globex, x=~Date, y=~Expenses)



#Spent resources division





#One shots
dt[is.na(Montant) & DoG =="D"]

head(dt)

#Elementary tests
x <- data.table(a=c("a","b","c"), b=c("01/12/2000", "16/04/1968", "22/10/1969"), c=c(10.9, 18, 45.98))

x[[2]] <- x[,dmy(b)]

x[,b] <- x[,dmy(b)]
x
x[a=="b", sum(c)]

x[[1]] <- c("br","B","jau")
x
