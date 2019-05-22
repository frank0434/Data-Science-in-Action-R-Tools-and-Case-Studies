第6章
================
刘健

``` r
library(data.table)
library(magrittr)
```

``` r
fread("./RawData/mtcars_DT.csv")
```

    ##                   V1  mpg cyl disp
    ## 1:         Mazda RX4 21.0   6  160
    ## 2:     Mazda RX4 Wag 21.0   6  160
    ## 3:        Datsun 710 22.8   4  108
    ## 4:    Hornet 4 Drive 21.4   6  258
    ## 5: Hornet Sportabout 18.7   8  360

``` r
fread("./RawData/mtcars_DT.csv", skip = "Mazda")
```

    ##                   V1   V2 V3  V4
    ## 1:         Mazda RX4 21.0  6 160
    ## 2:     Mazda RX4 Wag 21.0  6 160
    ## 3:        Datsun 710 22.8  4 108
    ## 4:    Hornet 4 Drive 21.4  6 258
    ## 5: Hornet Sportabout 18.7  8 360

``` r
fread("./RawData/mtcars_DT.csv", skip = "Mazda RX4")
```

    ##                   V1   V2 V3  V4
    ## 1:         Mazda RX4 21.0  6 160
    ## 2:     Mazda RX4 Wag 21.0  6 160
    ## 3:        Datsun 710 22.8  4 108
    ## 4:    Hornet 4 Drive 21.4  6 258
    ## 5: Hornet Sportabout 18.7  8 360

``` r
fread("./RawData/mtcars_DT.csv", skip = "Mazda RX4 Wag")
```

    ##                   V1   V2 V3  V4
    ## 1:     Mazda RX4 Wag 21.0  6 160
    ## 2:        Datsun 710 22.8  4 108
    ## 3:    Hornet 4 Drive 21.4  6 258
    ## 4: Hornet Sportabout 18.7  8 360

``` r
fread("./RawData/mtcars_DT.csv", skip = "710")
```

    ##                   V1   V2 V3  V4
    ## 1:        Datsun 710 22.8  4 108
    ## 2:    Hornet 4 Drive 21.4  6 258
    ## 3: Hornet Sportabout 18.7  8 360

``` r
fread("./RawData/mtcars_DT.csv", skip = "Drive")
```

    ##                   V1   V2 V3  V4
    ## 1:    Hornet 4 Drive 21.4  6 258
    ## 2: Hornet Sportabout 18.7  8 360

``` r
fread("./RawData/mtcars_DT.csv", select = c("V1","cyl"))
```

    ##                   V1 cyl
    ## 1:         Mazda RX4   6
    ## 2:     Mazda RX4 Wag   6
    ## 3:        Datsun 710   4
    ## 4:    Hornet 4 Drive   6
    ## 5: Hornet Sportabout   8

``` r
fread("./RawData/mtcars_DT.csv", select = c(1,3))
```

    ##                   V1 cyl
    ## 1:         Mazda RX4   6
    ## 2:     Mazda RX4 Wag   6
    ## 3:        Datsun 710   4
    ## 4:    Hornet 4 Drive   6
    ## 5: Hornet Sportabout   8

``` r
fread("./RawData/mtcars_DT.csv", drop = "cyl")
```

    ##                   V1  mpg disp
    ## 1:         Mazda RX4 21.0  160
    ## 2:     Mazda RX4 Wag 21.0  160
    ## 3:        Datsun 710 22.8  108
    ## 4:    Hornet 4 Drive 21.4  258
    ## 5: Hornet Sportabout 18.7  360

``` r
fread("./RawData/mtcars_DT.csv", drop = 2)
```

    ##                   V1 cyl disp
    ## 1:         Mazda RX4   6  160
    ## 2:     Mazda RX4 Wag   6  160
    ## 3:        Datsun 710   4  108
    ## 4:    Hornet 4 Drive   6  258
    ## 5: Hornet Sportabout   8  360

``` r
df <- mtcars[1:5,1:3]
colnames(df) <- c("V1","V2","V2") 
df
```

    ##                     V1 V2  V2
    ## Mazda RX4         21.0  6 160
    ## Mazda RX4 Wag     21.0  6 160
    ## Datsun 710        22.8  4 108
    ## Hornet 4 Drive    21.4  6 258
    ## Hornet Sportabout 18.7  8 360

``` r
fwrite(df,"./RawData/mtcars_wrongnaming.csv",row.names = TRUE)
fread("./RawData/mtcars_wrongnaming.csv", check.names = TRUE)
```

    ##                   V1 V1.1 V2 V2.1
    ## 1:         Mazda RX4 21.0  6  160
    ## 2:     Mazda RX4 Wag 21.0  6  160
    ## 3:        Datsun 710 22.8  4  108
    ## 4:    Hornet 4 Drive 21.4  6  258
    ## 5: Hornet Sportabout 18.7  8  360

``` r
fread("find /input/projects/processedData/ -type f -name *.csv")
```

    ## Warning in (if (.Platform$OS.type == "unix") system else shell)
    ## (paste0("(", : '(find /input/projects/processedData/ -type f -name *.csv)
    ## > C:\WINDOWS\TEMP\Rtmpuqfuhg\filee58276f12b0' execution failed with error
    ## code 2

    ## Warning in fread("find /input/projects/processedData/ -type f -name
    ## *.csv"): File 'C:\WINDOWS\TEMP\Rtmpuqfuhg\filee58276f12b0' has size 0.
    ## Returning a NULL data.table.

    ## Null data.table (0 rows and 0 cols)

``` r
DT %>% 
  filter(i) %>% 
  select(j) %>% 
  group_by()
```

    ## Error in eval(lhs, parent, parent): object 'DT' not found

``` r
DT <- data.table(mtcars,keep.rownames = TRUE)
unique(DT$rn)
```

    ##  [1] "Mazda RX4"           "Mazda RX4 Wag"       "Datsun 710"         
    ##  [4] "Hornet 4 Drive"      "Hornet Sportabout"   "Valiant"            
    ##  [7] "Duster 360"          "Merc 240D"           "Merc 230"           
    ## [10] "Merc 280"            "Merc 280C"           "Merc 450SE"         
    ## [13] "Merc 450SL"          "Merc 450SLC"         "Cadillac Fleetwood" 
    ## [16] "Lincoln Continental" "Chrysler Imperial"   "Fiat 128"           
    ## [19] "Honda Civic"         "Toyota Corolla"      "Toyota Corona"      
    ## [22] "Dodge Challenger"    "AMC Javelin"         "Camaro Z28"         
    ## [25] "Pontiac Firebird"    "Fiat X1-9"           "Porsche 914-2"      
    ## [28] "Lotus Europa"        "Ford Pantera L"      "Ferrari Dino"       
    ## [31] "Maserati Bora"       "Volvo 142E"

``` r
DT[rn == "Datsun 710"]
```

    ##            rn  mpg cyl disp hp drat   wt  qsec vs am gear carb
    ## 1: Datsun 710 22.8   4  108 93 3.85 2.32 18.61  1  1    4    1

``` r
DT[mpg < 18 & cyl == 6 ]
```

    ##           rn  mpg cyl  disp  hp drat   wt qsec vs am gear carb
    ## 1: Merc 280C 17.8   6 167.6 123 3.92 3.44 18.9  1  0    4    4

``` r
DT[gear == 5 | mpg == 21 ]
```

    ##                rn  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## 1:      Mazda RX4 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## 2:  Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## 3:  Porsche 914-2 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    ## 4:   Lotus Europa 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## 5: Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    ## 6:   Ferrari Dino 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    ## 7:  Maserati Bora 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8

``` r
DT[,.(rn,mpg,cyl)] %>% 
  head()
```

    ##                   rn  mpg cyl
    ## 1:         Mazda RX4 21.0   6
    ## 2:     Mazda RX4 Wag 21.0   6
    ## 3:        Datsun 710 22.8   4
    ## 4:    Hornet 4 Drive 21.4   6
    ## 5: Hornet Sportabout 18.7   8
    ## 6:           Valiant 18.1   6

``` r
models <- c("Merc 240D","Merc 230","Merc 280")
DT[rn %in% models, .(rn,mpg,cyl,hp,gear)]
```

    ##           rn  mpg cyl  hp gear
    ## 1: Merc 240D 24.4   4  62    4
    ## 2:  Merc 230 22.8   4  95    4
    ## 3:  Merc 280 19.2   6 123    4

``` r
DT[, .(mpg_mean = mean(mpg)), by = cyl][order(cyl)]
```

    ##    cyl mpg_mean
    ## 1:   4 26.66364
    ## 2:   6 19.74286
    ## 3:   8 15.10000

``` r
DT[, .(wt_max = max(wt)), by = cyl][order(cyl)]
```

    ##    cyl wt_max
    ## 1:   4  3.190
    ## 2:   6  3.460
    ## 3:   8  5.424

``` r
DT[, .(hp_max = max(hp)), by = cyl][order(cyl)]
```

    ##    cyl hp_max
    ## 1:   4    113
    ## 2:   6    175
    ## 3:   8    335

``` r
DT[vs == 1 , .(hp_max = max(hp)), by = cyl][order(cyl)]
```

    ##    cyl hp_max
    ## 1:   4    113
    ## 2:   6    123

``` r
DT_small <- DT[1:3,1:5]

DT_small[, new_col:= "value"][]
```

    ##               rn  mpg cyl disp  hp new_col
    ## 1:     Mazda RX4 21.0   6  160 110   value
    ## 2: Mazda RX4 Wag 21.0   6  160 110   value
    ## 3:    Datsun 710 22.8   4  108  93   value

``` r
DT_small[, new_col_2:= 1:nrow(DT_small)][]
```

    ##               rn  mpg cyl disp  hp new_col new_col_2
    ## 1:     Mazda RX4 21.0   6  160 110   value         1
    ## 2: Mazda RX4 Wag 21.0   6  160 110   value         2
    ## 3:    Datsun 710 22.8   4  108  93   value         3

``` r
DT_small[, new_col:= NULL][]
```

    ##               rn  mpg cyl disp  hp new_col_2
    ## 1:     Mazda RX4 21.0   6  160 110         1
    ## 2: Mazda RX4 Wag 21.0   6  160 110         2
    ## 3:    Datsun 710 22.8   4  108  93         3

``` r
DT_small[rn == "Datsun 710", cyl := 0][]
```

    ##               rn  mpg cyl disp  hp new_col_2
    ## 1:     Mazda RX4 21.0   6  160 110         1
    ## 2: Mazda RX4 Wag 21.0   6  160 110         2
    ## 3:    Datsun 710 22.8   0  108  93         3

``` r
DT_small[mpg > 21, calculte := round(mpg,digits = 0)][]
```

    ##               rn  mpg cyl disp  hp new_col_2 calculte
    ## 1:     Mazda RX4 21.0   6  160 110         1       NA
    ## 2: Mazda RX4 Wag 21.0   6  160 110         2       NA
    ## 3:    Datsun 710 22.8   0  108  93         3       23

``` r
DT_small[mpg > 21, `:=`(calculte = round(mpg,digits = 0),
                        cyl = 4)][]
```

    ##               rn  mpg cyl disp  hp new_col_2 calculte
    ## 1:     Mazda RX4 21.0   6  160 110         1       NA
    ## 2: Mazda RX4 Wag 21.0   6  160 110         2       NA
    ## 3:    Datsun 710 22.8   4  108  93         3       23

``` r
fwrite(DT_small[,list:= list(1:3,LETTERS[1:3],c("a","b","c"))],sep = "/", sep2 = c("{",",","}"))
```

    ## rn/mpg/cyl/disp/hp/new_col_2/calculte/list
    ## Mazda RX4/21/6/160/110/1//{1,2,3}
    ## Mazda RX4 Wag/21/6/160/110/2//{A,B,C}
    ## Datsun 710/22.8/4/108/93/3/23/{a,b,c}

``` r
DT <- data.table(mtcars,keep.rownames = TRUE)

names(DT)
```

    ##  [1] "rn"   "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"  
    ## [11] "gear" "carb"

``` r
theCols <- names(DT)[c(2:5,7,10)]
one <- DT[,.(mpg,cyl,disp,hp,wt,am)]
two <- DT[, c(2:5,7,10)]

three <- DT[,.SD, .SDcols = theCols]
four <- DT[,..theCols]

identical(one, two)
```

    ## [1] TRUE

``` r
identical(three, four)
```

    ## [1] TRUE

``` r
identical(one, four)
```

    ## [1] TRUE

``` r
names(DT)
```

    ##  [1] "rn"   "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"  
    ## [11] "gear" "carb"

``` r
setcolorder(DT, order(names(DT)))

head(DT)
```

    ##    am carb cyl disp drat gear  hp  mpg  qsec                rn vs    wt
    ## 1:  1    4   6  160 3.90    4 110 21.0 16.46         Mazda RX4  0 2.620
    ## 2:  1    4   6  160 3.90    4 110 21.0 17.02     Mazda RX4 Wag  0 2.875
    ## 3:  1    1   4  108 3.85    4  93 22.8 18.61        Datsun 710  1 2.320
    ## 4:  0    1   6  258 3.08    3 110 21.4 19.44    Hornet 4 Drive  1 3.215
    ## 5:  0    2   8  360 3.15    3 175 18.7 17.02 Hornet Sportabout  0 3.440
    ## 6:  0    1   6  225 2.76    3 105 18.1 20.22           Valiant  1 3.460

``` r
setcolorder(DT, c(names(DT)[ncol(DT)],names(DT)[-ncol(DT)]))

head(DT)
```

    ##       wt am carb cyl disp drat gear  hp  mpg  qsec                rn vs
    ## 1: 2.620  1    4   6  160 3.90    4 110 21.0 16.46         Mazda RX4  0
    ## 2: 2.875  1    4   6  160 3.90    4 110 21.0 17.02     Mazda RX4 Wag  0
    ## 3: 2.320  1    1   4  108 3.85    4  93 22.8 18.61        Datsun 710  1
    ## 4: 3.215  0    1   6  258 3.08    3 110 21.4 19.44    Hornet 4 Drive  1
    ## 5: 3.440  0    2   8  360 3.15    3 175 18.7 17.02 Hornet Sportabout  0
    ## 6: 3.460  0    1   6  225 2.76    3 105 18.1 20.22           Valiant  1

``` r
DT <- data.table(mtcars,keep.rownames = TRUE)


setkey(DT, cyl)
DT[.(6)]
```

    ##                rn  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## 1:      Mazda RX4 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## 2:  Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## 3: Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## 4:        Valiant 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    ## 5:       Merc 280 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    ## 6:      Merc 280C 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    ## 7:   Ferrari Dino 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6

``` r
setkey(DT, cyl, carb)
DT[.(4,2)]
```

    ##               rn  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## 1:     Merc 240D 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    ## 2:      Merc 230 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    ## 3:   Honda Civic 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    ## 4: Porsche 914-2 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    ## 5:  Lotus Europa 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## 6:    Volvo 142E 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

``` r
Toyota <- grep("Toyota.+", x = DT$rn, value = T)

setkey(DT,rn)

DT[Toyota]
```

    ##                rn  mpg cyl  disp hp drat    wt  qsec vs am gear carb
    ## 1: Toyota Corolla 33.9   4  71.1 65 4.22 1.835 19.90  1  1    4    1
    ## 2:  Toyota Corona 21.5   4 120.1 97 3.70 2.465 20.01  1  0    3    1

``` r
DT[.N]
```

    ##            rn  mpg cyl disp  hp drat   wt qsec vs am gear carb
    ## 1: Volvo 142E 21.4   4  121 109 4.11 2.78 18.6  1  1    4    2

``` r
DT[, .SD[1] , by =.(cyl)]
```

    ##    cyl           rn  mpg disp  hp drat    wt  qsec vs am gear carb
    ## 1:   8  AMC Javelin 15.2  304 150 3.15 3.435 17.30  0  0    3    2
    ## 2:   4   Datsun 710 22.8  108  93 3.85 2.320 18.61  1  1    4    1
    ## 3:   6 Ferrari Dino 19.7  145 175 3.62 2.770 15.50  0  1    5    6

``` r
DT[, .SD[.N] , by =.(cyl)]
```

    ##    cyl               rn  mpg disp  hp drat    wt  qsec vs am gear carb
    ## 1:   8 Pontiac Firebird 19.2  400 175 3.08 3.845 17.05  0  0    3    2
    ## 2:   4       Volvo 142E 21.4  121 109 4.11 2.780 18.60  1  1    4    2
    ## 3:   6          Valiant 18.1  225 105 2.76 3.460 20.22  1  0    3    1

``` r
DT[, .I[1], by=.(cyl)] 
```

    ##    cyl V1
    ## 1:   8  1
    ## 2:   4  5
    ## 3:   6  8

``` r
DT[, .I[.N], by=.(cyl)] 
```

    ##    cyl V1
    ## 1:   8 27
    ## 2:   4 32
    ## 3:   6 31

``` r
DT[sample(nrow(DT), 3)]
```

    ##                rn  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## 1:       Merc 230 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    ## 2: Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## 3:       Fiat 128 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1

``` r
DT[sample(.N, 3)]
```

    ##                  rn  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## 1:         Fiat 128 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    ## 2: Dodge Challenger 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    ## 3:    Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4

j based mutate

``` r
DT <- data.table(mtcars,keep.rownames = TRUE)
str(DT[,1:3])
```

    ## Classes 'data.table' and 'data.frame':   32 obs. of  3 variables:
    ##  $ rn : chr  "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
    ##  $ mpg: num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
    ##  $ cyl: num  6 6 4 6 8 6 8 4 4 6 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
str(DT[,1:3][, lapply(.SD, as.character)])
```

    ## Classes 'data.table' and 'data.frame':   32 obs. of  3 variables:
    ##  $ rn : chr  "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
    ##  $ mpg: chr  "21" "21" "22.8" "21.4" ...
    ##  $ cyl: chr  "6" "6" "4" "6" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
str(DT[,1:3][, (2:3) := lapply(.SD, as.factor), .SDcols = 2:3]) #
```

    ## Classes 'data.table' and 'data.frame':   32 obs. of  3 variables:
    ##  $ rn : chr  "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
    ##  $ mpg: Factor w/ 25 levels "10.4","13.3",..: 16 16 19 17 13 12 3 20 19 14 ...
    ##  $ cyl: Factor w/ 3 levels "4","6","8": 2 2 1 2 3 2 3 1 1 2 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
cols <- colnames(DT)[2:3]
DT[,1:3][, (cols) := lapply(.SD, as.factor), .SDcols = cols]
DT[1:3][, c("Brand","Model") := tstrsplit(rn, split = " ")][]
```

    ## Warning in `[.data.table`(DT[1:3], , `:=`(c("Brand", "Model"),
    ## tstrsplit(rn, : Supplied 2 columns to be assigned a list (length 3) of
    ## values (1 unused)

    ##               rn  mpg cyl disp  hp drat    wt  qsec vs am gear carb  Brand
    ## 1:     Mazda RX4 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4  Mazda
    ## 2: Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4  Mazda
    ## 3:    Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 Datsun
    ##    Model
    ## 1:   RX4
    ## 2:   RX4
    ## 3:   710

``` r
DT[1:3][, c("Brand","Model") := tstrsplit(rn, split = " ", keep = 1)][]
```

    ##               rn  mpg cyl disp  hp drat    wt  qsec vs am gear carb  Brand
    ## 1:     Mazda RX4 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4  Mazda
    ## 2: Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4  Mazda
    ## 3:    Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 Datsun
    ##     Model
    ## 1:  Mazda
    ## 2:  Mazda
    ## 3: Datsun

``` r
DT[1:3][, category_1 := ifelse(hp > 240, "A", 
                               ifelse(hp > 150 & hp <= 240, "B", "C"))][]
```

    ##               rn  mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## 1:     Mazda RX4 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## 2: Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## 3:    Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ##    category_1
    ## 1:          C
    ## 2:          C
    ## 3:          C

``` r
DT[1:3][, (colnames(DT)):= lapply(.SD, function(x){x = ifelse(x == 0,"--", x)}),.SDcols = colnames(DT)][]
```

    ##               rn  mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## 1:     Mazda RX4 21.0   6  160 110 3.90 2.620 16.46 --  1    4    4
    ## 2: Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02 --  1    4    4
    ## 3:    Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1

``` r
set.seed(42)
DT_sampled <- DT[, .SD[sample(.N, 2)], by = cyl][,1:5]
DT_sampled
```

    ##    cyl                  rn  mpg  disp  hp
    ## 1:   6        Ferrari Dino 19.7 145.0 175
    ## 2:   6           Merc 280C 17.8 167.6 123
    ## 3:   4            Fiat 128 32.4  78.7  66
    ## 4:   4       Porsche 914-2 26.0 120.3  91
    ## 5:   8    Dodge Challenger 15.5 318.0 150
    ## 6:   8 Lincoln Continental 10.4 460.0 215

``` r
index_1 <- data.table(value = unique(DT$cyl),
                      index = c(letters[1:3]),
                      comments = c("中等耗油","相对省油","油老虎"))
index_1
```

    ##    value index                         comments
    ## 1:     6     a <U+4E2D><U+7B49><U+8017><U+6CB9>
    ## 2:     4     b <U+76F8><U+5BF9><U+7701><U+6CB9>
    ## 3:     8     c         <U+6CB9><U+8001><U+864E>

``` r
DT_sampled[index_1,on = c("cyl"="value")][]
```

    ##    cyl                  rn  mpg  disp  hp index
    ## 1:   6        Ferrari Dino 19.7 145.0 175     a
    ## 2:   6           Merc 280C 17.8 167.6 123     a
    ## 3:   4            Fiat 128 32.4  78.7  66     b
    ## 4:   4       Porsche 914-2 26.0 120.3  91     b
    ## 5:   8    Dodge Challenger 15.5 318.0 150     c
    ## 6:   8 Lincoln Continental 10.4 460.0 215     c
    ##                            comments
    ## 1: <U+4E2D><U+7B49><U+8017><U+6CB9>
    ## 2: <U+4E2D><U+7B49><U+8017><U+6CB9>
    ## 3: <U+76F8><U+5BF9><U+7701><U+6CB9>
    ## 4: <U+76F8><U+5BF9><U+7701><U+6CB9>
    ## 5:         <U+6CB9><U+8001><U+864E>
    ## 6:         <U+6CB9><U+8001><U+864E>

``` r
melt(DT_sampled, id.vars = "rn") %>% 
  str()
```

    ## Classes 'data.table' and 'data.frame':   24 obs. of  3 variables:
    ##  $ rn      : chr  "Ferrari Dino" "Merc 280C" "Fiat 128" "Porsche 914-2" ...
    ##  $ variable: Factor w/ 4 levels "cyl","mpg","disp",..: 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ value   : num  6 6 4 4 8 8 19.7 17.8 32.4 26 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
dcast(melt(DT_sampled, id.vars = "rn"),  rn ~ ...)
```

    ##                     rn cyl  mpg  disp  hp
    ## 1:    Dodge Challenger   8 15.5 318.0 150
    ## 2:        Ferrari Dino   6 19.7 145.0 175
    ## 3:            Fiat 128   4 32.4  78.7  66
    ## 4: Lincoln Continental   8 10.4 460.0 215
    ## 5:           Merc 280C   6 17.8 167.6 123
    ## 6:       Porsche 914-2   4 26.0 120.3  91

``` r
DT[, c(.N, lapply(.SD, mean)), by= .(cyl), .SDcols = 2:5]
```

    ##    cyl  N      mpg cyl     disp        hp
    ## 1:   6  7 19.74286   6 183.3143 122.28571
    ## 2:   4 11 26.66364   4 105.1364  82.63636
    ## 3:   8 14 15.10000   8 353.1000 209.21429

``` r
DT[, lapply(.SD,mean), by=.(cyl), .SDcols = 2:(ncol(DT)-1)]
```

    ##    cyl      mpg cyl     disp        hp     drat       wt     qsec
    ## 1:   6 19.74286   6 183.3143 122.28571 3.585714 3.117143 17.97714
    ## 2:   4 26.66364   4 105.1364  82.63636 4.070909 2.285727 19.13727
    ## 3:   8 15.10000   8 353.1000 209.21429 3.229286 3.999214 16.77214
    ##           vs        am     gear
    ## 1: 0.5714286 0.4285714 3.857143
    ## 2: 0.9090909 0.7272727 4.090909
    ## 3: 0.0000000 0.1428571 3.285714

``` r
DT[, sapply(.SD, function(x) list(mean(x), 
                                  median(x),
                                  max(x))), .SDcols = c("mpg","hp"), by=.(cyl)]
```

    ##    cyl       V1   V2   V3        V4    V5  V6
    ## 1:   6 19.74286 19.7 21.4 122.28571 110.0 175
    ## 2:   4 26.66364 26.0 33.9  82.63636  91.0 113
    ## 3:   8 15.10000 15.2 19.2 209.21429 192.5 335
