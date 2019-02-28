###
#
#  RSQLite (Quick Intro)
#
#  adapted from:
#
# https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
#   (Hadley Wickham)
#
# and RSQLite.pdf (package documentation from CRAN) [ v. 2.1.1 ]
#
# run in MS R Open v. 3.5.1
#
###
#
# this is a VERY short intro providing enough information to make
# RSQLite useful in several circumstances
#
# be sure to read the SQLite documentation from sqlite.org for a more
# complete intro to the capabilities of this small but useful system
#
# Also: be aware that python has a sqlite library and thus can be used
#       to exchange information between R and Python for a collaborative
#       system
# 
###
#
# Testing code from Wickham (RSQLite vignette)
#
require(DBI)

# note: setwd() might be a good idea if you want to find the tables again
wd   <- "/home/navarurh/R/x86_64-pc-linux-gnu-library/3.5/RSQLite"  # be sure this directory exists before use
setwd(wd)

# comment: run the following commands individually and
#          verify their basic actions

# disk file created if not present
mydb <- dbConnect(RSQLite::SQLite(),"my-db.sqlite") # create new db on disk
dbDisconnect(mydb)                                  # close connection
unlink("my-db.sqlite")                              # remove the disk file

# temp database (disk)
mydb <- dbConnect(RSQLite::SQLite(),"")             # empty string
dbDisconnect(mydb)                                  # close w/ auto delete

# temp database (memory)
mydb <- dbConnect(RSQLite::SQLite(),":memory:")     # keyword
dbDisconnect(mydb)                                  # close and dispose :-)

# bundled database from "RSQLite"; duplicates "datasets" 
db   <- RSQLite::datasetsDb()
dbListTables(db)                # tables list

# general info
RSQLite::rsqliteVersion()

# scenario 1: keep copy of "datasets" and add "Boston" from MASS
mydb <- dbConnect(RSQLite::SQLite(),"myDatasets.sqlite")
RSQLite::sqliteCopyDatabase(db, mydb)               # from, to
dbListTables(mydb)

require(MASS)
data(Boston)
head(Boston)

dbWriteTable(mydb,"Boston",Boston)
dbListTables(mydb)

# scenario 2: split Boston into myBoston1 and myBoston2 (300, 206 rows)
myBoston1 <- Boston[1:300, ]
myBoston2 <- Boston[301:506, ]

# now write each of these to a "mydb" forming "myBoston"
dbWriteTable(mydb, "myBoston", myBoston1)           # create
dbWriteTable(mydb, "myBoston", myBoston2, append=T) # append

dbListTables(mydb)

ls()

myBoston <- dbReadTable(mydb,"myBoston")
ls()
all.equal(Boston, myBoston)

# comment: files can be built incrementally

# scenario 3: subset a table
#
# without parameter(s) 
cars4cyl  <- dbGetQuery(mydb, "SELECT * from mtcars WHERE cyl = 4")
# with    parameter(s)
cars4cyl2 <- dbGetQuery(mydb, "SELECT * from mtcars WHERE cyl = :x",
                        params=list(x=4) )
# same result
all.equal(cars4cyl, cars4cyl2)

# scenario 4: page through a query result (user specified size)
rs <- dbSendQuery(mydb,"SELECT * From myBoston")  # rs : return set

rows <- 100                                       # per fetch
while (!dbHasCompleted(rs)) {                     # run batches
  df <- dbFetch(rs, n=rows)
  print(nrow(df))
}

dbClearResult(rs)                                 # clean up

# scenario 5: no records returned
#
#  create tables, create indexes, drop tables,
#  attach databases
#
#  read the SQLite documentation for the non-return statements
#       and what they can do
#       http://www.sqlite.org
#
###
dbExecute(mydb,"CREATE INDEX imedv ON myBoston(medv)" )
dbExecute(mydb,"CREATE TABLE myMtcars AS SELECT * FROM mtcars")

dbListTables(mydb)

# and always
dbDisconnect(mydb)
