第11章
================
邬书豪 刘健

``` r
library(here)         # here()
```

    ## here() starts at C:/Data/R-Tools-for-data-science

``` r
library(data.table)   # fread()
library(tidyverse)    # %>% / arrange / ggplot()
```

    ## -- Attaching packages ---------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0       v purrr   0.3.1  
    ## v tibble  2.0.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts ------------------------- tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
library(DT)           # datatable()
library(corrplot)     # corrplot.mixed()
```

    ## corrplot 0.84 loaded

``` r
library(lubridate)    # ymd()
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday,
    ##     week, yday, year

    ## The following object is masked from 'package:here':
    ## 
    ##     here

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(stringr)      # substr()
```

``` r
here::here()
```

    ## [1] "C:/Data/R-Tools-for-data-science"

``` r
gb <- tail(fread('GBvideos.csv', encoding = 'UTF-8'), 200) %>%
  .[, 'Location':='英国']

fr <- tail(fread('FRvideos.csv', encoding = 'UTF-8'), 200) %>%
  .[, 'Location':='法国']

ca <- tail(fread('CAvideos.csv', encoding = 'UTF-8'), 200) %>%
  .[, 'Location':='加拿大']
```

``` r
videos <- rbindlist(list(gb, fr, ca))
dim(videos)
```

    ## [1] 424  17

``` r
videos %>% 
  select(title, Location, likes, dislikes,views)
```

    ##                                                                             title
    ##   1:                                John Lewis Christmas Ad 2017 - #MozTheMonster
    ##   2:                                    Taylor Swift: …Ready for It? (Live) - SNL
    ##   3:                                   Eminem - Walk On Water (Audio) ft. Beyoncé
    ##   4: Goals from Salford City vs Class of 92 and Friends at The Peninsula Stadium!
    ##   5:                      Dashcam captures truck's near miss with child in Norway
    ##  ---                                                                             
    ## 420:      The Trump Presidency (11/12/17) Last Week Tonight with John Oliver(HBO)
    ## 421:                                     The Original Ramen in Japan - Chuka Soba
    ## 422:                                      What They Found Should Rewrite History!
    ## 423:                                     7 Second Challenge: KNOCK-OFF DAN & PHIL
    ## 424:                                     Post Malone – Rockstar (feat. 21 Savage)
    ##                      Location  likes dislikes    views
    ##   1:         <U+82F1><U+56FD>  55681    10247  7224515
    ##   2:         <U+82F1><U+56FD>  25561     2294  1053632
    ##   3:         <U+82F1><U+56FD> 787420    43420 17158579
    ##   4:         <U+82F1><U+56FD>    193       12    27833
    ##   5:         <U+82F1><U+56FD>     30        2     9815
    ##  ---                                                  
    ## 420: <U+52A0><U+62FF><U+5927>     33       66    10037
    ## 421: <U+52A0><U+62FF><U+5927>  10217      125   212040
    ## 422: <U+52A0><U+62FF><U+5927>   1895      187   157289
    ## 423: <U+52A0><U+62FF><U+5927>  29389      121   236040
    ## 424: <U+52A0><U+62FF><U+5927>  26947      534  1237072

``` r
videos <- videos[,.(title, Location, likes, dislikes, views)][, title := gsub("\\W", " ", title)]
head(videos)
```

    ##                                                                           title
    ## 1:                                John Lewis Christmas Ad 2017    MozTheMonster
    ## 2:                                    Taylor Swift   Ready for It   Live    SNL
    ## 3:                                   Eminem   Walk On Water  Audio  ft  Beyoncé
    ## 4: Goals from Salford City vs Class of 92 and Friends at The Peninsula Stadium 
    ## 5:                      Dashcam captures truck s near miss with child in Norway
    ## 6:                                                 How My Relationship Started 
    ##            Location  likes dislikes    views
    ## 1: <U+82F1><U+56FD>  55681    10247  7224515
    ## 2: <U+82F1><U+56FD>  25561     2294  1053632
    ## 3: <U+82F1><U+56FD> 787420    43420 17158579
    ## 4: <U+82F1><U+56FD>    193       12    27833
    ## 5: <U+82F1><U+56FD>     30        2     9815
    ## 6: <U+82F1><U+56FD>  52708     1431  1182775

``` r
arr_views <-   videos[, c('title', 'views')] %>%  
  arrange(desc(views)) %>%          
  .[!duplicated(.$title), ] %>%     
  .[1:10, ]                        

arr_views <- setDT(arr_views)[, ranking := 1:.N]
arr_views[]
```

    ##                                                                                                title
    ##  1:                                                      Ed Sheeran   Perfect  Official Music Video 
    ##  2:                                                       Eminem   Walk On Water  Audio  ft  Beyoncé
    ##  3: Padmavati   Ek Dil Ek Jaan Video Song   Deepika Padukone   Shahid Kapoor   Sanjay Leela Bhansali
    ##  4:                                                                              Harry Styles   Kiwi
    ##  5:                                     Jennifer Lopez   Amor  Amor  Amor  Official Video  ft  Wisin
    ##  6:                                                    John Lewis Christmas Ad 2017    MozTheMonster
    ##  7: Daang   Full Video     Mankirt Aulakh   Sukh Sanghera   Latest Punjabi Song 2017   Speed Records
    ##  8:                                      The  Stranger Things  Kids Were Nearly a Motown Super Group
    ##  9:                                          Camila Cabello   Havana  Vertical Video  ft  Young Thug
    ## 10:                                                Chayanne   Choka Choka  Official Video  ft  Ozuna
    ##        views ranking
    ##  1: 33523622       1
    ##  2: 17158579       2
    ##  3: 10588371       3
    ##  4:  9632678       4
    ##  5:  9548677       5
    ##  6:  7224515       6
    ##  7:  5718766       7
    ##  8:  5541767       8
    ##  9:  5476738       9
    ## 10:  5331049      10

``` r
dat <- data.frame(x1 = rep(1:3, each = 2), x2 = rep(letters[1:6]))
dat
```

    ##   x1 x2
    ## 1  1  a
    ## 2  1  b
    ## 3  2  c
    ## 4  2  d
    ## 5  3  e
    ## 6  3  f

``` r
duplicated(dat$x1) 
```

    ## [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE

``` r
dat[!duplicated(dat$x1), ] 
```

    ##   x1 x2
    ## 1  1  a
    ## 3  2  c
    ## 5  3  e

``` r
arr_dislikes <- videos[, c('title', 'dislikes')] %>% 
  arrange(desc(dislikes)) %>%
  .[!duplicated(.$title), ] %>% 
  .[1:3, ]

arr_dislikes
```

    ##                                                                                        title
    ## 1 Jake Paul   Saturday Night  Song  feat  Nick Crompton   Chad Tepper  Official Music Video 
    ## 2                                                 Eminem   Walk On Water  Audio  ft  Beyoncé
    ## 5                                                Ed Sheeran   Perfect  Official Music Video 
    ##   dislikes
    ## 1   167908
    ## 2    43420
    ## 5    21082

``` r
videos <- within(videos, {
  likes_per = round(likes/views, 4)*100
  dislikes_per = round(dislikes/views, 4)*100
})
videos %>% 
  select(title, ends_with("per")) %>% 
  head(3)
```

    ##                                            title dislikes_per likes_per
    ## 1: John Lewis Christmas Ad 2017    MozTheMonster         0.14      0.77
    ## 2:     Taylor Swift   Ready for It   Live    SNL         0.22      2.43
    ## 3:    Eminem   Walk On Water  Audio  ft  Beyoncé         0.25      4.59

``` r
arr_likes_per <- 
  videos[, c('title', 'likes_per')] %>% 
  arrange(desc(likes_per)) %>%
  .[!duplicated(.$title), ] %>% 
  .[1:5, ]

arr_likes_per
```

    ##                                                     title likes_per
    ## 1      Fin du Game   FINAL SAISON 3  DerrièreYoutube Ep 6     20.79
    ## 2 DESSINS ANIME S FRANC AIS VS RUSSES 2   Daniil le Russe     15.80
    ## 4                                    iPhone X VS iPhone 1     13.88
    ## 5                Malika LePen   Femme de Gauche   Trailer     13.77
    ## 6      FIFA 18   ESWC   LES PAPYS FONT DE LA RÉSISTANCE       13.04

``` r
arr_dislikes_per <- videos[, c('title', 'dislikes_per')] %>% 
  arrange(desc(dislikes_per)) %>%
  .[!duplicated(.$title), ] %>% 
  .[1:3, ]

arr_dislikes_per
```

    ##                                                                                        title
    ## 1              How To Make Money Online With Mobile Tablet   Make Money By Installing Apps  
    ## 2 Jake Paul   Saturday Night  Song  feat  Nick Crompton   Chad Tepper  Official Music Video 
    ## 3         T es qui toi   Squeezie  le youtubeur aux 4 milliards de vues   Salut les Terriens
    ##   dislikes_per
    ## 1         8.11
    ## 2         3.99
    ## 3         2.09

5、细分国家粒度分析
-------------------

### 5.1探索不同国家的视频类型个数

探索不同国家的视频类型个数
--------------------------

对视频样本中重复视频标题进行去重
================================

``` r
videos <- rbindlist(list(gb, fr, ca))
videos2 <-  videos %>%
  select(category_id, Location, title, views, likes, dislikes) %>% 
  arrange(desc(views)) %>%
  .[!duplicated(.$title), ]
head(videos2)
```

    ##    category_id                 Location
    ## 1           10         <U+82F1><U+56FD>
    ## 3           10         <U+82F1><U+56FD>
    ## 6           10 <U+52A0><U+62FF><U+5927>
    ## 7           10         <U+82F1><U+56FD>
    ## 8           10         <U+82F1><U+56FD>
    ## 10          26         <U+82F1><U+56FD>
    ##                                                                                               title
    ## 1                                                       Ed Sheeran - Perfect (Official Music Video)
    ## 3                                                        Eminem - Walk On Water (Audio) ft. Beyoncé
    ## 6  Padmavati : Ek Dil Ek Jaan Video Song | Deepika Padukone | Shahid Kapoor | Sanjay Leela Bhansali
    ## 7                                                                               Harry Styles - Kiwi
    ## 8                                      Jennifer Lopez - Amor, Amor, Amor (Official Video) ft. Wisin
    ## 10                                                    John Lewis Christmas Ad 2017 - #MozTheMonster
    ##       views   likes dislikes
    ## 1  33523622 1634124    21082
    ## 3  17158579  787420    43420
    ## 6  10588371  132738     8812
    ## 7   9632678  810895    16139
    ## 8   9548677  190084    15015
    ## 10  7224515   55681    10247

### 5.2 统计不同国家的不同视频类型个数

``` r
videos_category <-   videos2 %>% 
  group_by(category_id, Location) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))
head(videos_category)
```

    ## # A tibble: 6 x 3
    ## # Groups:   category_id [4]
    ##   category_id Location                  freq
    ##         <int> <chr>                    <int>
    ## 1          24 <U+52A0><U+62FF><U+5927>    45
    ## 2          24 <U+6CD5><U+56FD>            40
    ## 3          22 <U+52A0><U+62FF><U+5927>    30
    ## 4          17 <U+6CD5><U+56FD>            25
    ## 5          10 <U+82F1><U+56FD>            23
    ## 6          22 <U+6CD5><U+56FD>            22

6、探索不同类型视频的喜欢与不喜欢人数

探索不同视频类型的喜欢与不喜欢人数
----------------------------------

按照视频类型统计喜欢/不喜欢的人数
---------------------------------

``` r
likes_dislikes <- videos2 %>% 
  group_by(category_id) %>%
  summarise(c_likes = sum(likes), c_dislikes = sum(dislikes)) %>%
  arrange(desc(c_likes))

head(likes_dislikes)
```

    ## # A tibble: 6 x 3
    ##   category_id c_likes c_dislikes
    ##         <int>   <int>      <int>
    ## 1          10 5218010     158876
    ## 2          24 1959272      68566
    ## 3          23 1912205      54576
    ## 4          22  839403     190584
    ## 5          26  567099      37090
    ## 6          20  490749       8982
